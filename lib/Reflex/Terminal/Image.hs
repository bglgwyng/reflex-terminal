module Reflex.Terminal.Image where

import Control.Monad
import Control.Monad.Except (MonadError (throwError), runExceptT)
import Control.Monad.State.Strict (MonadIO (liftIO), MonadState (get), execStateT, lift, modify)
import Data.Char.WCWidth (wcwidth)
import Data.Foldable
import Data.Text qualified as T hiding (foldr, foldr1)
import Reflex.Terminal.Interval
import Reflex.Terminal.Rect
import System.Console.ANSI
import System.IO

data Span = Span
  { text :: T.Text
  -- style :: Style
  }

data Axis = Row | Column

data Image
  = Line {spans :: [Span]}
  | Container
      { axis :: Axis,
        children :: [Image]
      }

class HasWidth a where
  width :: a -> Int

instance HasWidth Span where
  width span = T.foldl' (\l c -> wcwidth c + l) 0 span.text

-- instance HasWidth Line where
--   width line = sum $ width <$> line.spans

class HasHeight a where
  height :: a -> Int

-- instance HasHeight Line where
--   height _ = 1

instance HasHeight Image where
  height Line {} = 1
  height container@Container {}
    | Row <- container.axis = foldr1 max (height <$> container.children)
    | Column <- container.axis = sum (height <$> container.children)

instance HasWidth Image where
  width line@Line {} = sum $ width <$> line.spans
  width container@Container {}
    | Row <- container.axis = sum (height <$> container.children)
    | Column <- container.axis = foldr1 max (height <$> container.children)

class Renderable a where
  render :: a -> IO ()
  renderWithClipping :: Rect -> a -> IO ()

instance Renderable Span where
  render span = do
    setSGR (Reset : [])
    putStr (T.unpack span.text)

instance Renderable Image where
  render line@Line {} = for_ line.spans render
  render container@Container {}
    | Row <- container.axis = do
        for_ container.children render
    | Column <- container.axis =
        for_ container.children render
  renderWithClipping clip line@Line {}
    | 0 <- clip.top = render line
    | otherwise = pure ()
  renderWithClipping clip container@Container {}
    | Column <- container.axis = do
        let interval = (clip.top, bottom clip)
        let getSize = height
        -- liftIO $ hPrint stderr "Hi"
        void $ (`execStateT` 0) do
          runExceptT $ do
            for_ container.children \child -> do
              offset <- lift get
              let childInterval = (offset, offset + getSize child)

              liftIO $ hPrint stderr offset
              case intersection childInterval interval of
                Before -> pure ()
                OverlapsLeft overlap@(start, _) ->
                  liftIO $ do
                    setCursorPosition offset 0
                    renderWithClipping
                      clip {top = start - offset, height = size overlap}
                      child
                Contains -> undefined
                ContainedBy ->
                  liftIO $ do
                    setCursorPosition offset 0
                    renderWithClipping
                      clip {top = 0, height = height child}
                      child
                OverlapsRight overlap ->
                  liftIO $ do
                    setCursorPosition 0 0
                    renderWithClipping
                      clip {top = 0, height = size overlap}
                      child
                After -> throwError ()
              --
              -- if offset < fst interval
              --   then
              --     if offset + height child > fst interval
              --       then
              --         liftIO $
              --           renderWithClipping
              --             ( clip
              --                 { top = fst interval - offset,
              --                   height = offset + height child - fst interval
              --                 }
              --             )
              --             child
              --       else
              --         pure ()
              --   else
              --     if offset < snd interval
              --       then
              --         liftIO $ renderWithClipping (clip {top = 0, height = max 0 (offset + height child)}) child
              --       else
              --         throwError ()
              lift $ modify (+ getSize child)
            pure ()
        -- let x =
        --       flip evalState 0 $
        --         spanM
        --           ( \image -> do
        --               modify (+ width image)
        --               accWidth <- get
        --               pure $ accWidth < clip.x
        --           )
        --           container.children
        pure ()
    | Row <- container.axis = undefined

spanM :: forall m a. (Monad m) => (a -> m Bool) -> [a] -> m ([a], [a])
spanM predicate = go []
  where
    go :: [a] -> [a] -> m ([a], [a])
    go acc [] = pure (reverse acc, [])
    go acc rest@(x : xs) = do
      matches <- predicate x
      if matches
        then go (x : acc) xs
        else pure (reverse acc, rest)

-- pure (spanM f xs)
-- newtype Horiz a = Horiz [a]

-- newtype Vert a = Vert [a]

-- newtype Stacked a = Stacked [a]

-- instance (Displayable a) => Displayable (Stacked a) where
--   display (Stacked xs) = for_ xs display
