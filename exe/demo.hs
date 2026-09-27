import Control.Exception (finally)
import Control.Monad
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.FingerTree
import Data.Foldable (for_)
import Data.List
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

terminalLoop :: IO ()
terminalLoop = do
  clearScreen
  setCursorPosition 0 0
  Just (height, width) <- getTerminalSize
  hPrint stderr (height, width)
  lines <-
    runReaderT
      ( renderWithClipping Rect {top = 0, left = 0, height, width}
          $ Container
            @Column
          $ fromList . fmap ContainerChild
          $ [ Container
                @Row
                $ fromList . fmap ContainerChild
                $ [ Container
                      @Column
                      $ fromList . fmap ContainerChild
                      $ ["0123456789", "Hello"],
                    Container
                      @Column
                      $ fromList . fmap ContainerChild
                      $ [ Span (T.pack ['A' .. 'Z']),
                          "Hello",
                          "Bye"
                        ]
                  ],
              Container
                @Row
                $ fromList . fmap ContainerChild
                $ [ Container
                      @Column
                      $ fromList . fmap ContainerChild
                      $ ["0123456789", "Hello"],
                    Container
                      @Column
                      $ fromList . fmap ContainerChild
                      $ [ Span (T.pack ['A' .. 'Z']),
                          "Hello",
                          "Bye"
                        ]
                  ]
            ]
      )
      (0, 0)

  case unsnoc lines of
    Nothing -> forM_ lines $ T.hPutStr stdout
    Just (rest, last) -> forM_ (rest) (T.hPutStrLn stdout) *> T.hPutStr stdout last

  hFlush stdout
  void $ getChar
  clearScreen
  setCursorPosition 0 0

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
