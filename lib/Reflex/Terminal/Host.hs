module Reflex.Terminal.Host (runTerminal, TerminalContext (..)) where

import Control.Concurrent
import Control.Exception
import Control.Monad.Fix
import Control.Monad.Reader
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Reflex
import Reflex.Host.Headless
import Reflex.Network
import Reflex.Terminal.Prelude
import System.Console.ANSI
import System.Console.Terminal.Size qualified as TS
import System.IO
import System.Posix
import System.Posix.Signals.Exts

data TerminalContext t = TerminalContext
  { eInput :: Event t Char
  , size :: (Dynamic t Int, Dynamic t Int)
  }

runTerminal ::
  ( forall t r m.
    ( MonadHeadlessApp t m
    , MonadReader r m
    , HasField "eInput" r (Event t Char)
    , HasField "size" r (Dynamic t Int, Dynamic t Int)
    ) =>
    m (Dynamic t [Dynamic t T.Text], Event t ())
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
    setCursorPosition 0 0

    runHeadlessApp do
      ePostBuild <- getPostBuild
      (eInput, triggerInput) <- newTriggerEvent

      dSize <- holdTerminalSize

      (ddLines, eShutdown :: Event t ()) <-
        runReaderT
          (guest)
          ( TerminalContext
              { eInput
              , size = dSize
              }
          )
      performEvent_ $
        ePostBuild <&> \_ ->
          void . liftIO . forkIO $ forever do getChar >>= triggerInput

      void . networkView $ do
        ddLines <&> \dLines -> do
          for_ (zip [0 ..] dLines) \(i, dLine) -> do
            e <-
              networkView $
                dLine <&> \line -> liftIO do
                  setCursorPosition i 0
                  T.hPutStr stdout line
            performEvent_ $ e <&> const (liftIO $ hFlush stdout)

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

holdTerminalSize ::
  ( Reflex t
  , MonadHold t m
  , TriggerEvent t m
  , MonadIO m
  , MonadFix m
  ) =>
  m (Dynamic t Int, Dynamic t Int)
holdTerminalSize = do
  mSize <- liftIO TS.size
  let Just (TS.Window height0 width0) = mSize

  (eHeight, triggerHeight) <- newTriggerEvent
  (eWidth, triggerWidth) <- newTriggerEvent
  void . liftIO $
    installHandler
      sigWINCH
      ( Catch
          ( do
              Just (TS.Window height width) <- TS.size
              triggerHeight height
              triggerWidth width
          )
      )
      Nothing

  dHeight <- holdUniqDyn =<< holdDyn height0 eHeight
  dWidth <- holdUniqDyn =<< holdDyn width0 eWidth
  pure (dHeight, dWidth)
