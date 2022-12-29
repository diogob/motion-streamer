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
import qualified GI.Gst as GST
import qualified GI.Gst.Objects.Element as GST

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
  elements <- addMany pipeline [if configTest config then "videotestsrc" else "libcamerasrc", "videoconvert", "motioncells", "videoconvert", "tee", "queue", "autovideosink", "queue", "vp9enc", "rtpvp9pay", "udpsink"]
  let source : _ : _ : lastConvert : tee : _ : _ : queue : vp9 : rtp : udpSink : _ = elements
      pairsToLink = (zip <*> tail) (take 7 elements)
  GST.utilSetObjectArg source "pattern" "ball"

  linkResults <- mapM (uncurry GST.elementLink) pairsToLink
  unless (and linkResults) (throwM LinkingError)

  teeVideoSinkPad <- MaybeT $ GST.elementGetRequestPad tee "src_%u"
  qVideoSinkPad <- MaybeT $ GST.elementGetStaticPad queue "sink"
  r <- GST.padLink teeVideoSinkPad qVideoSinkPad

  unless (r == GST.PadLinkReturnOk) (throwM LinkingError)

  GST.utilSetObjectArg udpSink "host" "0.0.0.0"
  GST.utilSetObjectArg udpSink "port" "5000"

  linkResults <- mapM (uncurry GST.elementLink) [(queue, vp9), (vp9, rtp), (rtp, udpSink)]
  unless (and linkResults) (throwM LinkingError)

  -- recording portion of the pipeline
  teePad <- MaybeT $ GST.elementGetRequestPad tee "src_%u"
  [queue, valve, vp9enc, webmmux, filesink] <- addMany pipeline ["queue", "valve", "vp9enc", "webmmux", "filesink"]
  GST.utilSetObjectArg filesink "location" "./test.webm"
  GST.utilSetObjectArg valve "drop" "true"
  linkResults <- mapM (uncurry GST.elementLink) [(queue, vp9enc), (vp9enc, webmmux), (webmmux, filesink)]
  unless (and linkResults) (throwM LinkingError)

  queuePad <- MaybeT $ GST.elementGetStaticPad queue "sink"
  r <- GST.padLink teePad queuePad
  unless (r == GST.PadLinkReturnOk) (throwM LinkingError)

  -- Start playing
  result <- GST.elementSetState pipeline GST.StatePlaying
  when
    (result == GST.StateChangeReturnFailure)
    (throwM StateChangeError)

  -- Wait until error or EOS
  respondToMessages pipeline valve
  where
    stopRecording valve = do
      GST.utilSetObjectArg valve "drop" "true"
      liftIO $ print "Recording stopped"
    startRecording valve = do
      GST.utilSetObjectArg valve "drop" "false"
      liftIO $ print "Recording..."

    respondToMessages pipeline valve = do
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
            when (fname == "motion_begin") $ startRecording valve
            when (fname == "motion_finished") $ stopRecording valve
        _ -> pure ()

      respondToMessages pipeline valve

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

-- ❯ gst-launch-1.0 -v udpsrc port=5000  ! "application/x-rtp, media=(string)video, clock-rate=(int)90000, encoding-name=(string)VP9, payload=(int)96, ssrc=(uint)101494402, timestamp-offset=(uint)1062469180, seqnum-offset=(uint)10285, a-framerate=(string)30" ! rtpvp9depay ! vp9dec ! decodebin ! videoconvert ! autovideosink sync=false
