module Reflex.Terminal.Image (Axis (..), Image (..), MonadRender, renderWithClipping) where

import Control.Arrow
import Control.Exception.Base
import Control.Monad.Reader
import Data.Char.WCWidth
import Data.Text qualified as T
import Data.Text.IO ()
import Reflex.Terminal.Interval
import Reflex.Terminal.Prelude
import Reflex.Terminal.Rect hiding (height, width)
import System.Console.ANSI

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

renderWithClipping :: (MonadRender m) => Rect Int -> Image -> m ()
renderWithClipping clip span@Span {}
  | 0 <- clip.top = do
      cursor <- ask
      liftIO $ do
        uncurry setCursorPosition cursor
        setSGR $ Reset : []
        clipped <-
          (`evalStateT` 0) $
            T.unpack span.text
              & spanM
                (\c -> gets (< clip.left) <* modify (+ wcwidth c))
              <&> snd
              >>= spanM
                (\c -> gets (<= clip.right) <* modify (+ wcwidth c))
                <&> fst
        putStr $ clipped
  | otherwise = pure ()
renderWithClipping clip container@Container {axis} = do
  let sizeAlongAxis = case axis of
        Row -> (.width)
        Column -> (.height)
      interval' :: Lens' (Rect Int) (Interval Int)
      interval' = case axis of
        Row -> xInterval
        Column -> yInterval

  void $ (`execStateT` 0) do
    runExceptT $ do
      for_ container.children \child -> do
        offset :: Int <- lift get
        let childInterval = Interval (offset, offset + sizeAlongAxis child)

        let withOrigin :: (MonadReader (Int, Int) m) => m a -> m a
            withOrigin m =
              do
                case axis of
                  Row ->
                    local (second (+ (offset - clip ^. interval' . _Wrapped . _1))) m
                  Column ->
                    local (first (+ (offset - clip ^. interval' . _Wrapped . _1))) m

        case intersection childInterval (clip ^. interval') of
          Before -> pure ()
          OverlapsLeft overlap ->
            withOrigin $
              renderWithClipping
                (clip & interval' .~ Interval (overlap ^. _Wrapped . _1 - offset, overlap.size))
                child
          Contains ->
            assert (offset <= (clip ^. interval' . _Wrapped . _1)) $
              withOrigin $
                renderWithClipping
                  (clip & interval' .~ Interval (clip ^. interval' . _Wrapped . _1 - offset, clip ^. interval' . to (.size)))
                  child
          ContainedBy ->
            withOrigin $
              renderWithClipping
                (clip & interval' .~ Interval (0, sizeAlongAxis child))
                child
          OverlapsRight overlap ->
            withOrigin $
              renderWithClipping
                (clip & interval' .~ Interval (0, overlap.size))
                child
          After -> throwError ()
        lift $ modify (+ sizeAlongAxis child)
-- TODO: clipping
renderWithClipping clip (Clip {image}) = renderWithClipping clip image

type MonadRender m = (MonadReader (Int, Int) m, MonadIO m)
