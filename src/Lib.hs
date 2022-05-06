module Lib
  ( play,
  )
where

import Control.Monad (when)
import Data.Foldable (find)
import Data.Maybe (isJust)
import GI.Gst (mk_IteratorFoldFunction)
import qualified GI.Gst as GST

play :: IO ()
play = do
  GST.init Nothing
  p <- GST.parseLaunch "playbin uri=https://www.freedesktop.org/software/gstreamer-sdk/data/media/sintel_trailer-480p.webm"
  maybeBus <- GST.elementGetBus p
  GST.elementSetState p GST.StatePlaying
  maybeMsg <- maybe (pure Nothing) maybeErrorOrEos maybeBus
  messageTypes <- maybe (pure []) GST.getMessageType maybeMsg

  when (isJust $ find (GST.MessageTypeError ==) messageTypes) $ do
    error "Something went wrong"

  putStrLn "Got to the end!"
  where
    maybeErrorOrEos b = GST.busTimedPopFiltered b GST.CLOCK_TIME_NONE [GST.MessageTypeError, GST.MessageTypeEos]