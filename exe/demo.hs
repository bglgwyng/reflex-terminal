{-# LANGUAGE ImpredicativeTypes #-}

import Control.Monad
import Control.Monad.Reader
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Reflex
import Reflex.Terminal.Host
import Reflex.Terminal.Image
import System.Console.ANSI
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

        dText <- accumDyn (\acc c -> (acc <> T.pack [c])) ("." :: T.Text) (eInput)

        let dContainer =
              box
                '.'
                ( vertical $
                    M.fromList
                      [
                        ( 0
                        , (5, horizontal (M.fromList [(0, (5, fill 'a')), (1, (10, fill 'b')), (2, (10, fill 'c'))]))
                        )
                      ,
                        ( 1
                        , (5, horizontal (M.fromList [(0, (5, fill 'd')), (1, (10, fill 'e')), (2, (10, fill 'f'))]))
                        )
                      ]
                )
                size
        pure (dContainer, void $ ffilter (== 'q') eInput)
      useNormalScreenBuffer
    else do
      T.hPutStrLn stderr "Standard output does not support 'ANSI' escape codes."
      exitFailure
