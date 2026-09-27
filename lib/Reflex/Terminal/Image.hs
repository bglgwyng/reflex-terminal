module Reflex.Terminal.Image (Axis (..), Image (..), MonadRender, renderWithClipping) where

import Control.Exception.Base
import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Data.Char.WCWidth
import Data.Text qualified as T
import Data.Text.IO ()
import Reflex.Terminal.Interval
import Reflex.Terminal.Prelude
import Reflex.Terminal.Rect hiding (height, width)

data Axis = Row | Column deriving (Show)

data Image
  = Span {text :: T.Text}
  | Container
      { axis :: Axis,
        children :: [Image]
      }
  | Clip
      { rect :: Rect Int,
        image :: Image
      }
  deriving (Show)

instance HasField "height" Image Int where
  getField Span {} = 1
  getField container@Container {}
    | Row <- container.axis = foldr1 max ((.height) <$> container.children)
    | Column <- container.axis = sum ((.height) <$> container.children)
  getField (Clip {rect}) = rect.height

instance HasField "width" Image Int where
  getField span@Span {} = T.foldl' (\l c -> wcwidth c + l) 0 span.text
  getField container@Container {}
    | Row <- container.axis = sum ((.width) <$> container.children)
    | Column <- container.axis = foldr1 max ((.width) <$> container.children)
  getField (Clip {rect}) = rect.width

renderWithClipping :: (MonadRender m) => Rect Int -> Image -> m [T.Text]
renderWithClipping clip span@Span {}
  | 0 <- clip.top = do
      -- TODO: avoid unpack/pack
      clipped <-
        (`evalStateT` 0) $ do
          T.unpack span.text
            & spanM
              (\c -> gets (< clip.left) <* modify (+ wcwidth c))
            <&> snd
            >>= spanM
              (\c -> gets (<= clip.right) <* modify (+ wcwidth c))
              <&> fst
      pure $ take clip.height $ [T.justifyLeft clip.width ' ' $ T.pack clipped] <> repeat (T.replicate clip.width " ")
  | otherwise = pure []
renderWithClipping clip container@Container {axis} = do
  let sizeAlongAxis = case axis of
        Row -> (.width)
        Column -> (.height)
      interval' :: Lens' (Rect Int) (Interval Int)
      interval' = case axis of
        Row -> xInterval
        Column -> yInterval

  (`evalStateT` 0) do
    childrenLines :: [[T.Text]] <- execWriterT $
      runExceptT $ do
        for_ container.children \child -> do
          offset :: Int <- lift get
          let childInterval :: Interval Int
              childInterval = Interval (offset, offset + sizeAlongAxis child)

          case intersection childInterval (clip ^. interval') of
            Before -> pure ()
            OverlapsLeft overlap ->
              tell . pure =<< renderWithClipping (clip & interval' .~ Interval (overlap ^. _Wrapped . _1 - offset, overlap.size)) child
            Contains ->
              assert (offset <= (clip ^. interval' . _Wrapped . _1)) $
                tell . pure =<< renderWithClipping (clip & interval' .~ Interval (clip ^. interval' . _Wrapped . _1 - offset, clip ^. interval' . to (.size))) child
            ContainedBy ->
              tell . pure =<< renderWithClipping (clip & interval' .~ Interval (0, sizeAlongAxis child)) child
            OverlapsRight overlap ->
              tell . pure =<< renderWithClipping (clip & interval' .~ Interval (0, overlap.size)) child
            After -> throwError ()
          lift $ modify (+ sizeAlongAxis child)

    pure case axis of
      Row -> (T.justifyLeft clip.width ' ') <$> foldr1 (zipWith (<>)) childrenLines
      Column -> take clip.height $ join childrenLines <> repeat (T.replicate clip.width " ")
-- TODO: clipping
renderWithClipping clip (Clip {image}) = renderWithClipping clip image

type MonadRender m = (MonadReader (Int, Int) m, MonadIO m)
