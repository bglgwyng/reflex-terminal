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
        dText <- accumDyn (\acc c -> (acc <> T.pack [c])) ("." :: T.Text) (eStdin)
        dText2 <- accumDyn (\acc c -> (acc <> T.pack [c, c])) ("." :: T.Text) (eStdin)

        let dSpan = Span <$> dText
            dSpan2 = Span <$> dText2
            dContainer =
              Container
                @_
                @Row
                $ FT.fromList . fmap (pure . ContainerChild)
                $ [ Container
                      @_
                      @Column
                      $ FT.fromList
                      $ [ContainerChild <$> dSpan, ContainerChild <$> dSpan2]
                  , Container
                      @_
                      @Column
                      $ FT.fromList
                      $ [ContainerChild <$> dSpan, ContainerChild <$> dSpan2]
                  ]

        pure (pure dContainer, void $ ffilter (== 'q') eStdin)
      useNormalScreenBuffer
    else do
      T.hPutStrLn stderr "Standard output does not support 'ANSI' escape codes."
      exitFailure

sampleImage :: forall t. (Reflex t) => Image t
sampleImage =
  Container
    @t
    @Column
    $ FT.fromList . fmap (pure . ContainerChild)
    $ [ Container
          @t
          @Row
          $ FT.fromList . fmap (pure . ContainerChild)
          $ [ Container
                @t
                @Column
                $ FT.fromList . fmap (pure . ContainerChild)
                $ ["0123456789", "Hello"]
            , Container
                @t
                @Column
                $ FT.fromList . fmap (pure . ContainerChild)
                $ [ Span (T.pack ['A' .. 'Z'])
                  , "Hello"
                  , "Bye"
                  ]
            ]
      , Container
          @t
          @Row
          $ FT.fromList . fmap (pure . ContainerChild)
          $ [ Container
                @t
                @Column
                $ FT.fromList . fmap (pure . ContainerChild)
                $ ["0123456789", "Hello"]
            , Container
                @t
                @Column
                $ FT.fromList . fmap (pure . ContainerChild)
                $ [ Span (T.pack ['A' .. 'Z'])
                  , "Hello"
                  , "Bye"
                  ]
            ]
      ]
