module Reflex.Terminal.Host (runTerminal) where

import Control.Concurrent
import Control.Exception
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Reflex
import Reflex.Host.Headless
import Reflex.Terminal.Image
import Reflex.Terminal.Prelude
import Reflex.Terminal.Rect qualified as R
import System.Console.ANSI
import System.IO
import System.Posix

runTerminal ::
  ( forall t m.
    (MonadHeadlessApp t m) =>
    Event t Char -> m (Dynamic t (Image t), Event t ())
  ) ->
  IO ()
runTerminal guest = do
  -- oldStdinBuffering <- hGetBuffering stdin
  -- oldStdinEcho <- hGetEcho stdin
  oldStdoutBuffering <- hGetBuffering stdout

  (setAttrs, resetAttrs) <- attributeControl stdInput

  ( setAttrs *> do
      hSetBuffering stdin NoBuffering
      hSetBuffering stdout (BlockBuffering Nothing)
      hSetEcho stdin False
      hideCursor

      app
    )
    `finally` do
      -- TODO: uncomment this and fix termination
      -- hSetBuffering stdin oldBuffering
      -- hSetEcho stdin oldEcho
      hSetBuffering stdout oldStdoutBuffering
      resetAttrs
      setSGR [Reset]
      showCursor
 where
  app = do
    Just (height, width) <- getTerminalSize
    setCursorPosition 0 0

    runHeadlessApp do
      let renderLines :: [T.Text] -> IO ()
          renderLines lines = do
            case unsnoc lines of
              Nothing -> forM_ lines $ T.hPutStr stdout
              Just (rest, last) -> forM_ (rest) (T.hPutStrLn stdout) *> T.hPutStr stdout last
            setCursorPosition 0 0
            hFlush stdout

      ePostBuild <- getPostBuild
      (eInput, triggerInput) <- newTriggerEvent

      (dImage, eShutdown) <- guest eInput
      performEvent_ $
        ePostBuild <&> \_ ->
          void . liftIO . forkIO $ forever do getChar >>= triggerInput

      let dLines = dImage >>= renderWithClipping R.Rect{top = 0, left = 0, height, width}

      performEvent_ $ current dLines `tag` ePostBuild <&> liftIO . renderLines
      performEvent_ $ updated dLines <&> liftIO . renderLines

      pure eShutdown

-- Copied from vty-unix
attributeControl :: Fd -> IO (IO (), IO ())
attributeControl fd = do
  original <- getTerminalAttributes fd
  let tuiMode = foldl withMode clearedFlags flagsToSet
      clearedFlags = foldl withoutMode original flagsToUnset
      flagsToSet =
        [ MapCRtoLF -- ICRNL
        ]
      flagsToUnset =
        [ StartStopOutput -- IXON
        , KeyboardInterrupts -- ISIG
        , EnableEcho -- ECHO
        , ProcessInput -- ICANON
        , ExtendedFunctions -- IEXTEN
        ]
  let setAttrs = setTerminalAttributes fd tuiMode Immediately
      unsetAttrs = setTerminalAttributes fd original Immediately
  return (setAttrs, unsetAttrs)
