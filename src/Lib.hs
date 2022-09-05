module Lib
  ( play,
  )
where

import Control.Exception (Exception, throwIO)
import Control.Monad (unless, when)
import Control.Monad.Catch (throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.Foldable (find)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified GI.Gst as GST

data GstreamerTestException = LinkingError | StateChangeError | WaitingError
  deriving (Show)

instance Exception GstreamerTestException

play :: IO ()
play = do
  p <- runMaybeT maybePlay
  case p of
    Nothing -> error "Something when wrong"
    Just msg -> print msg

maybePlay :: MaybeT IO ()
maybePlay = do
  pipeline <- makePipeline
  elements <- addMany pipeline ["videotestsrc", "videoconvert", "motioncells", "videoconvert", "autovideosink"]
  let [source, _, motion, _, sink] = elements
      pairsToLink = (zip <*> tail) elements
  GST.utilSetObjectArg source "pattern" "ball"

  --  <- GST.elementLink source convert1

  linkResults <- mapM (uncurry GST.elementLink) pairsToLink
  unless (and linkResults) (throwM LinkingError)

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
      -- Wait until error or EOS
      liftIO $ putStrLn "Waiting for pipeline message..."

      msg <- waitForErrorOrEosOrElement pipeline
      messageTypes <- GST.getMessageType msg

      when (isJust $ find (GST.MessageTypeError ==) messageTypes) $ do
        liftIO $ putStrLn "Error: "
        (_, errorMessage) <- GST.messageParseError msg
        liftIO $ print errorMessage
        throwM WaitingError

      when (isJust $ find (GST.MessageTypeElement ==) messageTypes) $ do
        str <- MaybeT $ GST.messageGetStructure msg
        nfields <- GST.structureNFields str
        when (nfields > 0) $ do
          fname <- GST.structureNthFieldName str (fromIntegral nfields - 1)
          liftIO $ print $ "Message: " <> fname

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