{-# LANGUAGE ImpredicativeTypes #-}

import Control.Arrow ((>>>))
import Control.Monad
import Control.Monad.Reader
import Data.Functor
import Data.Functor.Misc (Const2 (..))
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Reflex
import Reflex.Terminal.Host
import Reflex.Terminal.Image
import System.Console.ANSI
import System.Console.Input
import System.Exit (exitFailure)
import System.IO

main :: IO ()
main = do
  stdoutSupportsANSI <- hNowSupportsANSI stdout

  if stdoutSupportsANSI
    then do
      useAlternateScreenBuffer
      runTerminal do
        eInput <- asks (.eInput)
        size@(dHeight, dWidth) <- asks (.size)

        dChildHeight :: Dynamic t Int <- count eInput

        let dContainer =
              box
                '.'
                (\size -> vertical (children size) size)
                size
            children :: (Reflex t) => (Dynamic t Int, Dynamic t Int) -> Dynamic t (M.Map Int ((Int, Renderer t)))
            children (dHeight, _) =
              distributeMapOverDynPure $
                M.fromList
                  [
                    ( 0
                    , do
                        childHeight <- dChildHeight
                        pure (childHeight, horizontal (M.fromList [(0, (5, fill 'a')), (1, (10, fill 'b')), (2, (10, fill 'c'))]))
                    )
                  ,
                    ( 1
                    , do
                        height <- dHeight
                        childHeight <- dChildHeight
                        pure (height - childHeight, horizontal (M.fromList [(0, (5, fill 'd')), (1, (10, fill 'e')), (2, (10, fill 'f'))]))
                    )
                  ]
        pure (dContainer, void $ ffilter (== KeyPress (KChar 'c') [Ctrl]) eInput)
      useNormalScreenBuffer
    else do
      T.hPutStrLn stderr "Standard output does not support 'ANSI' escape codes."
      exitFailure
