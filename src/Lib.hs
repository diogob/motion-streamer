module Lib
  ( play,
  )
where

import Config
import Control.Exception (Exception, throwIO)
import Control.Monad (unless, when)
import Control.Monad.Catch (throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.Foldable (find)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified GI.Gst as GET
import qualified GI.Gst as GST

data GstreamerTestException = LinkingError | StateChangeError | WaitingError
  deriving (Show)

instance Exception GstreamerTestException

play :: Config -> IO ()
play config = do
  p <- runMaybeT $ maybePlay config
  case p of
    Nothing -> error "Something when wrong"
    Just msg -> print msg

maybePlay :: Config -> MaybeT IO ()
maybePlay config = do
  pipeline <- makePipeline
  elements <- addMany pipeline [if configTest config then "videotestsrc" else "libcamerasrc", "videoconvert", "motioncells", "videoconvert", "tee", "queue", "autovideosink", "queue", "autovideosink"]
  let source : _ : _ : lastConvert : tee : _ : _ : queue : videoSink : _ = elements
      pairsToLink = (zip <*> tail) (take 7 elements)
  GST.utilSetObjectArg source "pattern" "ball"

  --  <- GST.elementLink source convert1

  linkResults <- mapM (uncurry GST.elementLink) pairsToLink
  unless (and linkResults) (throwM LinkingError)

  teeVideoSinkPad <- MaybeT $ GST.elementRequestPadSimple tee "src_%u"
  qVideoSinkPad <- MaybeT $ GST.elementGetStaticPad queue "sink"
  r <- GST.padLink teeVideoSinkPad qVideoSinkPad

  GST.elementLink queue videoSink
  unless (r == GST.PadLinkReturnOk) (throwM LinkingError)

  -- Start playing
  result <- GST.elementSetState pipeline GST.StatePlaying
  when
    (result == GST.StateChangeReturnFailure)
    (throwM StateChangeError)

  -- Wait until error or EOS
  respondToMessages pipeline
  where
    -- onMessageType messageType action
    respondToMessages pipeline = do
      msg <- waitForErrorOrEosOrElement pipeline
      messageTypes <- GST.getMessageType msg

      case messageTypes of
        [GST.MessageTypeError] -> do
          liftIO $ putStrLn "Error: "
          (_, errorMessage) <- GST.messageParseError msg
          liftIO $ print errorMessage
          throwM WaitingError
        [GST.MessageTypeElement] -> do
          str <- MaybeT $ GST.messageGetStructure msg
          nfields <- GST.structureNFields str
          when (nfields > 0) $ do
            fname <- GST.structureNthFieldName str (fromIntegral nfields - 1)
            when (fname == "motion_begin" || fname == "motion_finished") $ liftIO $ print $ "Motion change: " <> fname
        _ -> pure ()

      respondToMessages pipeline

    makePipeline :: MaybeT IO GST.Pipeline
    makePipeline = GST.init Nothing >> GST.pipelineNew Nothing

    waitForMessageTypes :: [GST.MessageType] -> GST.Pipeline -> MaybeT IO GST.Message
    waitForMessageTypes messageTypes pipeline = do
      b <- MaybeT $ GST.elementGetBus pipeline
      MaybeT $ GST.busTimedPopFiltered b GST.CLOCK_TIME_NONE messageTypes

    waitForErrorOrEosOrElement :: GST.Pipeline -> MaybeT IO GST.Message
    waitForErrorOrEosOrElement = waitForMessageTypes [GST.MessageTypeError, GST.MessageTypeEos, GST.MessageTypeElement]

    addMany :: GST.Pipeline -> [Text] -> MaybeT IO [GST.Element]
    addMany pipeline names = do
      elements <- mapM make names
      addResults <- mapM (GST.binAdd pipeline) elements
      when (any not addResults) (error "Could not add some element")
      pure elements

    make kind = MaybeT $ GST.elementFactoryMake kind Nothing

-- gst-launch-1.0 udpsrc address=192.168.1.101 port=5000 caps=application/x-rtp ! rtph264depay ! h264parse ! avdec_h264 ! videorate ! videoscale ! video/x-raw,width=320,height=240,framerate=5/1 ! videoconvert ! motioncells datafile=motion-yo  ! videoconvert ! xvimagesink