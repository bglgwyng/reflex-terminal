import Control.Monad
import Data.FingerTree qualified as FT
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
      runTerminal \eStdin -> do
        dImage <- holdDyn (sampleImage 'a') (sampleImage <$> eStdin)
        pure (dImage, void $ ffilter (== 'q') eStdin)
      useNormalScreenBuffer
    else do
      T.hPutStrLn stderr "Standard output does not support 'ANSI' escape codes."
      exitFailure

sampleImage :: forall t. Char -> Image t
sampleImage char =
  Container
    @t
    @Column
    $ FT.fromList . fmap ContainerChild
    $ [ Container
          @t
          @Row
          $ FT.fromList . fmap ContainerChild
          $ [ Container
                @t
                @Column
                $ FT.fromList . fmap ContainerChild
                $ ["0123456789", "Hello"]
            , Container
                @t
                @Column
                $ FT.fromList . fmap ContainerChild
                $ [
                    -- Span (T.pack ['A' .. 'Z'])
                    Span (T.replicate 10 (T.pack [char]))
                  , "Hello"
                  , "Bye"
                  ]
            ]
      , Container
          @t
          @Row
          $ FT.fromList . fmap ContainerChild
          $ [ Container
                @t
                @Column
                $ FT.fromList . fmap ContainerChild
                $ ["0123456789", "Hello"]
            , Container
                @t
                @Column
                $ FT.fromList . fmap ContainerChild
                $ [ Span (T.pack ['A' .. 'Z'])
                  , "Hello"
                  , "Bye"
                  ]
            ]
      ]
