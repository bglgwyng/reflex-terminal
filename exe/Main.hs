import Control.Exception (finally)
import Control.Monad
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Text qualified as T hiding (foldr, foldr1)
import Data.Text.IO qualified as T
import Reflex.Terminal.Image
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
  runReaderT
    ( renderWithClipping Rect {top = 0, left = 0, height, width} $
        Container
          Row
          [ Container
              Column
              [ Span ("HIsdfdsfdsfdsf"),
                Span ("Hell22o")
              ],
            Container
              Column
              [ Span ("HI"),
                Span ("Hello")
              ]
          ]
    )
    (0, 0)

  -- renderWithClipping Rect {_top = 5, _left = 0, _height = height, _width = width} $
  --   Container
  --     { axis = Column,
  --       children =
  --         [
  --           Line [Span (T.show x)],
  --         | x <- [0 .. 0]
  --         ]
  --     }
  -- Span/
  -- setCursorPosition 0 11
  -- render $
  --   Span
  --     "h"
  hFlush stdout
  void $ getChar
  clearScreen
  setCursorPosition 0 0

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
