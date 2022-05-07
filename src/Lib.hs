module Lib
  ( play,
  )
where

import Control.Exception (throwIO)
import Control.Monad (unless, when, (>=>))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.Foldable (find)
import Data.Maybe (isJust)
import Data.Text (Text)
import GI.Gst (mk_IteratorFoldFunction)
import qualified GI.Gst as GST

play :: IO ()
play = do
  p <- runMaybeT maybePlay
  case p of
    Nothing -> error "Something when wrong"
    Just msg -> print msg

maybePlay :: MaybeT IO Text
maybePlay = do
  pipeline <- makePipeline
  [source, sink] <- addMany pipeline ["videotestsrc", "autovideosink"]

  linkResult <- GST.elementLink source sink
  unless linkResult (error "Could not link elements")

  -- Start playing
  result <- GST.elementSetState pipeline GST.StatePlaying
  when
    (result == GST.StateChangeReturnFailure)
    (error "Could not set state to playing")

  -- Wait until error or EOS
  msg <- waitForErrorOrEos pipeline
  messageTypes <- GST.getMessageType msg

  when (isJust $ find (GST.MessageTypeError ==) messageTypes) $ do
    error "Error waiting for message"

  pure "Got to the end!"
  where
    makePipeline :: MaybeT IO GST.Pipeline
    makePipeline = GST.init Nothing >> GST.pipelineNew Nothing

    waitForErrorOrEos :: GST.Pipeline -> MaybeT IO GST.Message
    waitForErrorOrEos pipeline = do
      b <- MaybeT $ GST.elementGetBus pipeline
      MaybeT $ GST.busTimedPopFiltered b GST.CLOCK_TIME_NONE [GST.MessageTypeError, GST.MessageTypeEos]

    addMany :: GST.Pipeline -> [Text] -> MaybeT IO [GST.Element]
    addMany pipeline names = do
      elements <- mapM make names
      addResults <- mapM (GST.binAdd pipeline) elements
      when (any not addResults) (error "Could not add some element")
      pure elements

    make kind = MaybeT $ GST.elementFactoryMake kind Nothing

-- gst-launch-1.0 udpsrc address=192.168.1.101 port=5000 caps=application/x-rtp ! rtph264depay ! h264parse ! avdec_h264 ! videorate ! videoscale ! video/x-raw,width=320,height=240,framerate=5/1 ! videoconvert ! motioncells datafile=motion-yo  ! videoconvert ! xvimagesink