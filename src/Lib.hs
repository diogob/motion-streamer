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
import Data.Maybe (fromJust, isJust)
import Data.Text (Text)
import qualified GI.Gst as GST
import qualified GI.Gst.Objects.Element as GST

data GSError = LinkingError | StateChangeError | WaitingError | AddError | FactoryError | BusError | MessageError | PadError
  deriving (Show)

instance Exception GSError

play :: Config -> IO ()
play config = do
  pipeline <- makePipeline
  [source, _, _, lastConvert, tee, _, _] <- addManyLinked pipeline [if configTest config then "videotestsrc" else "libcamerasrc", "videoconvert", "motioncells", "videoconvert", "tee", "queue", "autovideosink"]
  [queue, vp9, rtp, udpSink] <- addManyLinked pipeline ["queue", "vp9enc", "rtpvp9pay", "udpsink"]
  GST.utilSetObjectArg source "pattern" "ball"

  branchPipeline tee queue

  GST.utilSetObjectArg udpSink "host" "0.0.0.0"
  GST.utilSetObjectArg udpSink "port" "5000"

  -- recording portion of the pipeline
  teePad <- maybeThrow PadError $ GST.elementGetRequestPad tee "src_%u"
  [queue, valve, vp9enc, webmmux, filesink] <- addManyLinked pipeline ["queue", "vp9enc", "webmmux", "valve", "filesink"]
  GST.utilSetObjectArg filesink "location" "./test.webm"
  GST.utilSetObjectArg valve "drop" "true"

  queuePad <- maybeThrow PadError $ GST.elementGetStaticPad queue "sink"
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
          str <- maybeThrow MessageError $ GST.messageGetStructure msg
          nfields <- GST.structureNFields str
          when (nfields > 0) $ do
            fname <- GST.structureNthFieldName str (fromIntegral nfields - 1)
            when (fname == "motion_begin" || fname == "motion_finished") $ liftIO $ print $ "Motion change: " <> fname
            when (fname == "motion_begin") $ startRecording valve
            when (fname == "motion_finished") $ stopRecording valve
        _ -> pure ()

      respondToMessages pipeline valve

maybeThrow :: GSError -> IO (Maybe a) -> IO a
maybeThrow error action = do
  maybeAction <- action
  case maybeAction of
    Just a -> pure a
    Nothing -> throwM error

makePipeline :: IO GST.Pipeline
makePipeline = GST.init Nothing >> GST.pipelineNew Nothing

waitForMessageTypes :: [GST.MessageType] -> GST.Pipeline -> IO GST.Message
waitForMessageTypes messageTypes pipeline = do
  b <- maybeThrow BusError $ GST.elementGetBus pipeline
  maybeThrow MessageError $ GST.busTimedPopFiltered b GST.CLOCK_TIME_NONE messageTypes

waitForErrorOrEosOrElement :: GST.Pipeline -> IO GST.Message
waitForErrorOrEosOrElement = waitForMessageTypes [GST.MessageTypeError, GST.MessageTypeEos, GST.MessageTypeElement]

addMany :: GST.Pipeline -> [Text] -> IO [GST.Element]
addMany pipeline names = do
  let make = flip GST.elementFactoryMake Nothing
  let maybeMake name = maybeThrow FactoryError $ make name
  elements <- mapM maybeMake names
  addResults <- mapM (GST.binAdd pipeline) elements
  when (any not addResults) (throwM AddError)
  pure elements

linkMany :: [(GST.Element, GST.Element)] -> IO ()
linkMany pairsToLink = do
  linkResults <- mapM (uncurry GST.elementLink) pairsToLink
  unless (and linkResults) (throwM LinkingError)

addManyLinked :: GST.Pipeline -> [Text] -> IO [GST.Element]
addManyLinked pipeline elements = do
  elements <- addMany pipeline elements
  linkMany $ (zip <*> tail) elements
  pure elements

branchPipeline :: GST.Element -> GST.Element -> IO ()
branchPipeline tee queue = do
  teePad <- maybeThrow PadError $ GST.elementGetRequestPad tee "src_%u"
  queuePad <- maybeThrow PadError $ GST.elementGetStaticPad queue "sink"
  r <- GST.padLink teePad queuePad
  unless (r == GST.PadLinkReturnOk) (throwM LinkingError)



-- ❯ gst-launch-1.0 -v udpsrc port=5000  ! "application/x-rtp, media=(string)video, clock-rate=(int)90000, encoding-name=(string)VP9, payload=(int)96, ssrc=(uint)101494402, timestamp-offset=(uint)1062469180, seqnum-offset=(uint)10285, a-framerate=(string)30" ! rtpvp9depay ! vp9dec ! decodebin ! videoconvert ! autovideosink sync=false
