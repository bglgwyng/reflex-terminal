module Data.Conduit.Handle.Timeout (sourceHandleWithTimeout) where

import Conduit
import Control.Monad
import Data.ByteString.Char8 qualified as BS
import System.IO

sourceHandleWithTimeout ::
  (MonadIO m) =>
  Handle ->
  Int ->
  Int ->
  ConduitT i BS.ByteString m ()
sourceHandleWithTimeout h chunkSize timeout =
  loop
 where
  loop = do
    hasInput <- liftIO $ hWaitForInput h timeout
    when hasInput do
      bs <- liftIO (BS.hGetSome h chunkSize)
      unless (BS.null bs) $
        yield bs >> loop
