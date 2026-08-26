import Control.Exception (finally, throw)
import Control.Monad
import Control.Monad.Cont
import Control.Monad.Except (MonadError (throwError), runExceptT)
import Control.Monad.State.Strict (MonadIO (liftIO), MonadState (get), evalState, evalStateT, execStateT, lift, modify)
import Data.Foldable
import Data.Foldable1 hiding (foldr1)
import Data.Function
import Data.Functor
import Data.String qualified as T
import Data.Text qualified as T hiding (foldr, foldr1)
import Data.Text.IO qualified as T
import Data.Word
import Reflex.Terminal.Image
import Reflex.Terminal.Interval
import Reflex.Terminal.Rect
import System.Console.ANSI
import System.IO

main :: IO ()
main = do
  stdoutSupportsANSI <- hNowSupportsANSI stdout
  if stdoutSupportsANSI
    then runTerminal
    else
      T.hPutStrLn stderr "Standard output does not support 'ANSI' escape codes."

runTerminal :: IO ()
runTerminal = do
  oldBuffering <- hGetBuffering stdin
  oldEcho <- hGetEcho stdin

  useAlternateScreenBuffer
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False

  terminalLoop `finally` do
    hSetBuffering stdin oldBuffering
    hSetEcho stdin oldEcho
    setSGR [Reset]
    showCursor

-- useMainScreenBuffer

terminalLoop :: IO ()
terminalLoop = do
  clearScreen
  setCursorPosition 0 10
  Just (height, width) <- getTerminalSize
  hPrint stderr (height, width)
  renderWithClipping Rect {top = 5, left = 0, height, width} $
    Container
      { axis = Column,
        children =
          [ Line [Span (T.show x)]
          | x <- [0 .. 30]
          ]
      }
  -- Span/
  -- setCursorPosition 0 11
  -- render $
  --   Span
  --     "h"
  hFlush stdout
  void $ getChar

-- clearScreen
-- setCursorPosition 5 0
-- setTitle "ANSI Terminal Short Example"
-- setSGR
--   [ SetConsoleIntensity BoldIntensity,
--     SetColor Foreground Vivid Red
--   ]
-- putStrLn "Type characters (q to quit):"
-- setSGR [SetColor Foreground Vivid White]
-- hFlush stdout
-- readCharacters 0

readCharacters :: Int -> IO ()
readCharacters characterCount = do
  character <- getChar
  case character of
    'q' -> pure ()
    '\b' -> deleteCharacter characterCount
    '\x7f' -> deleteCharacter characterCount
    _ -> do
      putChar character
      hFlush stdout
      readCharacters (characterCount + 1)

deleteCharacter :: Int -> IO ()
deleteCharacter characterCount = do
  if characterCount > 0
    then do
      putStr "\b \b"
      hFlush stdout
      readCharacters (characterCount - 1)
    else readCharacters characterCount
