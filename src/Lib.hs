module Lib
  ( play,
  )
where

import Config
import Control.Concurrent (MVar, isEmptyMVar, newEmptyMVar, putMVar, readMVar, takeMVar, threadDelay)
import Control.Exception (Exception)
import Control.Monad (unless, void, when, (>=>))
import Control.Monad.Catch (throwM)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (defaultTimeLocale, getZonedTime)
import Data.Time.Format (formatTime)
import qualified GI.GLib as GLib
import qualified GI.Gst as GST
import System.Directory (getFileSize)

data GSError = LinkingError | StateChangeError | WaitingError | AddError | FactoryError Text | BusError | MessageError | PadError
  deriving (Show)

instance Exception GSError

play :: Config -> IO ()
play = setupPipeline >=> loopAndClear

loopAndClear :: GST.Pipeline -> IO ()
loopAndClear pipeline = do
  -- Start the main loop to handle messages
  loop <- GLib.mainLoopNew Nothing False
  GLib.mainLoopRun loop

  -- Let the pipeline run for a while
  threadDelay (10 * 1000000)

  -- Cleanup
  putStrLn "Stopping pipeline."
  void $ GST.elementSetState pipeline GST.StateNull
  putStrLn "Pipeline stopped."

data FileRecorder = FileRecorder
  { fileName :: Text,
    fileQ :: GST.Element,
    fileValve :: GST.Element,
    fileClock :: GST.Element,
    fileEnc :: GST.Element,
    fileMux :: GST.Element,
    fileSink :: GST.Element,
    filePad :: GST.Pad,
    teePad :: GST.Pad
  }

addFileRecorder :: GST.Bin -> GST.Element -> Text -> IO FileRecorder
addFileRecorder pipelineBin tee name = do
  putStrLn "Adding file recorder..."
  [q, valve, clock, enc] <- addManyLinked pipelineBin ["queue", "valve", "clockoverlay", "v4l2h264enc"]
  [mux, sink] <- addManyLinked pipelineBin ["avimux", "filesink"]
  linkCaps "video/x-h264,level=(string)4" enc mux
  GST.utilSetObjectArg clock "shaded-background" "true"
  GST.utilSetObjectArg clock "time-format" "%a %y-%m-%d %H:%M"
  GST.utilSetObjectArg sink "location" name
  GST.utilSetObjectArg sink "async" "false"
  GST.utilSetObjectArg valve "drop" "false"
  putStrLn "Setting new elements to play..."
  mapM_ (`GST.elementSetState` GST.StatePlaying) [q, valve, clock, enc, mux, sink]
  putStrLn "Connecting to the pipeline..."
  (tP, fP) <- branchPipeline tee q
  pure
    FileRecorder
      { fileName = name,
        fileQ = q,
        fileValve = valve,
        fileClock = clock,
        fileEnc = enc,
        fileMux = mux,
        fileSink = sink,
        filePad = fP,
        teePad = tP
      }

removeFileRecorder :: FileRecorder -> GST.Bin -> IO ()
removeFileRecorder recorder bin = do
  void $ GST.padUnlink (teePad recorder) (filePad recorder)
  mapM_ (`GST.elementSetState` GST.StateNull) [fileQ recorder, fileClock recorder, fileEnc recorder, fileMux recorder, fileSink recorder]
  mapM_ (GST.binRemove bin) [fileQ recorder, fileClock recorder, fileEnc recorder, fileMux recorder, fileSink recorder]

setupPipeline :: Config -> IO GST.Pipeline
setupPipeline config = do
  pipeline <- makePipeline
  pipelineBin <- GST.toBin pipeline
  -- Add 4 segments to pipeline:
  -- source ---> motion detector -> fake sink
  --         |-> network sink
  --         |-> file sink
  [source, tee] <- addMany pipelineBin [if configTest config then "videotestsrc" else "libcamerasrc", "tee"]
  linkCaps "video/x-raw,width=1024,height=768,framerate=24/1,format=YUY2,colorimetry=bt709,interlace-mode=progressive" source tee

  [motionQ, scale] <- addManyLinked pipelineBin ["queue", "videoscale"]
  [convert, motion, _] <- addManyLinked pipelineBin ["videoconvert", "motioncells", "fakesink"]
  linkCaps "video/x-raw,width=320,height=240" scale convert

  [tcpQ, tcpEnc] <- addManyLinked pipelineBin ["queue", "v4l2h264enc"]
  [tcpMux, tcpSink] <- addManyLinked pipelineBin ["h264parse", "tcpserversink"]
  linkCaps "video/x-h264,level=(string)4" tcpEnc tcpMux
  -- Raw h264 bitstream. Client-side: gst-launch-1.0 -v tcpclientsrc host=rpi port=5000 ! h264parse ! avdec_h264 ! autovideosink

  -- Connect segments using T
  mapM_ (branchPipeline tee) [motionQ, tcpQ]

  recorderVar <- newEmptyMVar :: IO (MVar FileRecorder)

  -- Set properties
  GST.utilSetObjectArg source "pattern" "ball"
  GST.utilSetObjectArg tcpSink "host" (configHost config)
  GST.utilSetObjectArg tcpSink "port" (T.pack $ show $ configPort config)
  GST.utilSetObjectArg motion "display" "false"
  GST.utilSetObjectArg motion "sensitivity" $ configSensitivity config
  GST.utilSetObjectArg tcpMux "config-interval" "1"

  bus <- GST.pipelineGetBus pipeline
  -- Start playing
  result <- GST.elementSetState pipeline GST.StatePlaying
  when
    (result == GST.StateChangeReturnFailure)
    (throwM StateChangeError)
  putStrLn "Pipeline set to PLAYING state."

  -- Main message loop
  let stopRecording = do
        liftIO $ putStrLn "Stopping recorder..."
        r <- readMVar recorderVar
        when (configTest config) $
          GST.utilSetObjectArg source "pattern" "ball"
        GST.utilSetObjectArg (fileValve r) "drop" "true"
        liftIO $ putStrLn "Recording stopped"

      addRecorder = do
        startTime <- getZonedTime
        let name = "./motion-" <> formatTime defaultTimeLocale "%Y-%m-%d-%H%M%S" startTime <> ".avi"
        liftIO $ putStrLn ("New file: " <> name)
        newRecorder <- addFileRecorder pipelineBin tee $ T.pack name
        void $ putMVar recorderVar newRecorder

      startRecording = do
        when (configTest config) $
          GST.utilSetObjectArg source "pattern" "circular"
        empty <- isEmptyMVar recorderVar
        when empty $ void addRecorder
        liftIO $ putStrLn "Recording..."
        r <- readMVar recorderVar
        GST.utilSetObjectArg (fileValve r) "drop" "false"
        fileSize <- getFileSize (T.unpack (fileName r) :: String)
        when (fileSize > 100000000) $
          do
            liftIO $ putStrLn "Rotating file..."
            rotatedOutRecorder <- takeMVar recorderVar
            void $ removeFileRecorder rotatedOutRecorder pipelineBin
            addRecorder

      respondToMessages :: GST.Bus -> GST.Message -> IO Bool
      respondToMessages _ msg = do
        messageTypes <- GST.getMessageType msg
        case messageTypes of
          [GST.MessageTypeError] -> do
            liftIO $ putStrLn "Error: "
            (_, errorMessage) <- GST.messageParseError msg
            liftIO $ print errorMessage
            throwM WaitingError
          [GST.MessageTypeStreamStart] -> do
            liftIO $ putStrLn "Start streaming"
          [GST.MessageTypeElement] -> do
            str <- maybeThrow MessageError $ GST.messageGetStructure msg
            nfields <- GST.structureNFields str
            when (nfields > 0) $ do
              fname <- GST.structureNthFieldName str (fromIntegral nfields - 1)
              when (fname == "motion_begin" || fname == "motion_finished") $ liftIO $ print $ "Motion change: " <> fname
              when (fname == "motion_begin") startRecording
              when (fname == "motion_finished") stopRecording
          _ -> pure ()
        return True

  void $ GST.busAddWatch bus GLib.PRIORITY_DEFAULT respondToMessages
  pure pipeline

maybeThrow :: GSError -> IO (Maybe a) -> IO a
maybeThrow gsError action = do
  maybeAction <- action
  case maybeAction of
    Just a -> pure a
    Nothing -> throwM gsError

makePipeline :: IO GST.Pipeline
makePipeline = GST.init Nothing >> GST.pipelineNew Nothing

addMany :: GST.Bin -> [Text] -> IO [GST.Element]
addMany pipeline names = do
  let make = flip GST.elementFactoryMake Nothing
  let maybeMake name = maybeThrow (FactoryError name) $ make name
  elements <- mapM maybeMake names
  addResults <- mapM (GST.binAdd pipeline) elements
  when (any not addResults) (throwM AddError)
  pure elements

linkMany :: [(GST.Element, GST.Element)] -> IO ()
linkMany pairsToLink = do
  linkResults <- mapM (uncurry GST.elementLink) pairsToLink
  unless (and linkResults) (throwM LinkingError)

linkCaps :: Text -> GST.Element -> GST.Element -> IO ()
linkCaps caps elementA elementB = do
  maybeCaps <- GST.capsFromString caps
  linkResults <- GST.elementLinkFiltered elementA elementB maybeCaps
  unless linkResults $ throwM LinkingError

addManyLinked :: GST.Bin -> [Text] -> IO [GST.Element]
addManyLinked pipeline elements = do
  e <- addMany pipeline elements
  linkMany $ (zip <*> tail) e
  pure e

branchPipeline :: GST.Element -> GST.Element -> IO (GST.Pad, GST.Pad)
branchPipeline tee queue = do
  teePad <- maybeThrow PadError $ GST.elementRequestPadSimple tee "src_%u"
  queuePad <- maybeThrow PadError $ GST.elementGetStaticPad queue "sink"
  r <- GST.padLink teePad queuePad
  unless (r == GST.PadLinkReturnOk) (throwM LinkingError)
  pure (teePad, queuePad)

-- ❯ gst-launch-1.0 -v udpsrc port=5000  ! "application/x-rtp, media=(string)video, clock-rate=(int)90000, encoding-name=(string)VP9, payload=(int)96, ssrc=(uint)101494402, timestamp-offset=(uint)1062469180, seqnum-offset=(uint)10285, a-framerate=(string)30" ! rtpvp9depay ! vp9dec ! decodebin ! videoconvert ! autovideosink sync=false
