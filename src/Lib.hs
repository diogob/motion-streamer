module Lib
  ( play,
  )
where

import Config
import Control.Exception (Exception)
import Control.Monad (unless, when)
import Control.Monad.Catch (throwM)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import qualified GI.Gst as GST

data GSError = LinkingError | StateChangeError | WaitingError | AddError | FactoryError | BusError | MessageError | PadError
  deriving (Show)

instance Exception GSError

play :: Config -> IO ()
play config = do
  pipeline <- makePipeline

  -- Add 3 segments to pipeline: source, network sink and file sink
  [source, tee] <- addManyLinked pipeline [if configTest config then "videotestsrc" else "libcamerasrc", "tee"]
  [motionQ, _, _, _] <- addManyLinked pipeline ["queue", "videoconvert", "motioncells", "fakesink"]
  [networkQ, _, _, tcpSink] <- addManyLinked pipeline ["queue", "jpegenc", "avimux", "tcpserversink"]
  [fileQ, valve, clock, _, _, filesink] <- addManyLinked pipeline ["queue", "valve", "clockoverlay", "jpegenc", "avimux", "filesink"]

  -- Connect segments using T
  mapM_ (branchPipeline tee) [motionQ, fileQ, networkQ]

  -- Set properties
  startTime <- getCurrentTime
  GST.utilSetObjectArg filesink "location" ("./motion-" <> T.pack (show startTime) <> ".avi")
  GST.utilSetObjectArg source "pattern" "ball"
  GST.utilSetObjectArg tcpSink "host" (configHost config)
  GST.utilSetObjectArg tcpSink "port" (T.pack $ show $ configPort config)
  GST.utilSetObjectArg valve "drop" "false"
  GST.utilSetObjectArg clock "shaded-background" "true"
  GST.utilSetObjectArg clock "time-format" "%a %y-%m-%d %H:%M"

  -- Start playing
  result <- GST.elementSetState pipeline GST.StatePlaying
  when
    (result == GST.StateChangeReturnFailure)
    (throwM StateChangeError)

  -- Main message loop
  let stopRecording = do
        GST.utilSetObjectArg valve "drop" "true"
        GST.utilSetObjectArg source "pattern" "ball"
        liftIO $ putStrLn "Recording stopped"

      startRecording = do
        GST.utilSetObjectArg valve "drop" "false"
        GST.utilSetObjectArg source "pattern" "circular"
        liftIO $ putStrLn "Recording..."

      respondToMessages = do
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
              when (fname == "motion_begin") startRecording
              when (fname == "motion_finished") stopRecording
          _ -> pure ()
        respondToMessages

  respondToMessages

maybeThrow :: GSError -> IO (Maybe a) -> IO a
maybeThrow gsError action = do
  maybeAction <- action
  case maybeAction of
    Just a -> pure a
    Nothing -> throwM gsError

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
  e <- addMany pipeline elements
  linkMany $ (zip <*> tail) e
  pure e

branchPipeline :: GST.Element -> GST.Element -> IO ()
branchPipeline tee queue = do
  teePad <- maybeThrow PadError $ GST.elementGetRequestPad tee "src_%u"
  queuePad <- maybeThrow PadError $ GST.elementGetStaticPad queue "sink"
  r <- GST.padLink teePad queuePad
  unless (r == GST.PadLinkReturnOk) (throwM LinkingError)

-- ❯ gst-launch-1.0 -v udpsrc port=5000  ! "application/x-rtp, media=(string)video, clock-rate=(int)90000, encoding-name=(string)VP9, payload=(int)96, ssrc=(uint)101494402, timestamp-offset=(uint)1062469180, seqnum-offset=(uint)10285, a-framerate=(string)30" ! rtpvp9depay ! vp9dec ! decodebin ! videoconvert ! autovideosink sync=false
