import Conduit
import Control.Monad
import Data.ByteString.Builder.Extra (defaultChunkSize)
import Data.ByteString.Char8 qualified as BS
import Data.Conduit.Attoparsec
import Data.Conduit.Handle.Timeout
import System.Console.Input
import System.IO

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  runConduit $
    forever
      ( sourceHandleWithTimeout stdin defaultChunkSize 10
          .| peekForeverE (sinkParserEither keyPress >>= yield)
      )
      .| awaitForever (liftIO . print)
