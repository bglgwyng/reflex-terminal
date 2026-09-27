module Reflex.Terminal.Image (Axis (..), Image (..), ContainerChild (..), MonadRender, renderWithClipping) where

import Control.Exception.Base
import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Data.Char.WCWidth
import Data.FingerTree
import Data.Monoid
import Data.Proxy
import Data.Semigroup
import Data.String (IsString (..))
import Data.Text qualified as T
import Data.Text.IO ()
import Reflex.Terminal.Interval
import Reflex.Terminal.Prelude
import Reflex.Terminal.Rect hiding (height, width)

data Axis = Row | Column deriving (Show)

class KnownAxis (axis :: Axis) where
  axisVal :: proxy axis -> Axis

instance KnownAxis 'Row where
  axisVal _ = Row

instance KnownAxis 'Column where
  axisVal _ = Column

data Image where
  Span :: {text :: T.Text} -> Image
  Container ::
    forall axis.
    (KnownAxis axis, Measured (ImageMetric axis) (ContainerChild axis)) =>
    { children :: FingerTree (ImageMetric axis) (ContainerChild axis)
    } ->
    Image
  Clip :: {rect :: Rect Int, image :: Image} -> Image

instance IsString Image where
  fromString string = Span (T.pack string)

newtype ContainerChild (axis :: Axis) = ContainerChild {unContainerChild :: Image}

data ImageMetric (axis :: Axis) = ImageMetric
  { alongAxis :: Sum Int,
    acrossAxis :: Max Int
  }

instance Semigroup (ImageMetric axis) where
  im1 <> im2 =
    ImageMetric
      { alongAxis = im1.alongAxis <> im2.alongAxis,
        acrossAxis = im1.acrossAxis <> im2.acrossAxis
      }

instance Monoid (ImageMetric axis) where
  mempty = ImageMetric {alongAxis = 0, acrossAxis = 0}

instance Measured (ImageMetric 'Row) (ContainerChild 'Row) where
  measure (ContainerChild image) = ImageMetric {alongAxis = Sum image.width, acrossAxis = Max image.height}

instance Measured (ImageMetric 'Column) (ContainerChild 'Column) where
  measure (ContainerChild image) = ImageMetric {alongAxis = Sum image.height, acrossAxis = Max image.width}

instance HasField "height" Image Int where
  getField Span {} = 1
  getField (Container @axis children) =
    case axisVal $ Proxy @axis of
      Row -> (measure children).acrossAxis.getMax
      Column -> (measure children).alongAxis.getSum
  getField (Clip {rect}) = rect.height

instance HasField "width" Image Int where
  getField span@Span {} = T.foldl' (\l c -> wcwidth c + l) 0 span.text
  getField (Container @axis children) =
    case axisVal $ Proxy @axis of
      Row -> (measure children).alongAxis.getSum
      Column -> (measure children).acrossAxis.getMax
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
renderWithClipping clip container@(Container @(axis' :: Axis) children) = do
  let interval' :: Lens' (Rect Int) (Interval Int)
      interval' = case axisVal (Proxy @axis') of
        Row {} -> xInterval
        Column {} -> yInterval

  (`evalStateT` 0) do
    childrenLines :: [[T.Text]] <- execWriterT $
      runExceptT $ do
        for_ children \c@(ContainerChild child) -> do
          offset :: Int <- lift get
          let sizeAlongAxis = (measure c).alongAxis.getSum
              childInterval :: Interval Int
              childInterval = Interval (offset, offset + sizeAlongAxis)

          case intersection childInterval (clip ^. interval') of
            Before -> pure ()
            OverlapsLeft overlap ->
              tell . pure =<< renderWithClipping (clip & interval' .~ Interval (overlap ^. _Wrapped . _1 - offset, overlap.size)) child
            Contains ->
              assert (offset <= (clip ^. interval' . _Wrapped . _1)) $
                tell . pure =<< renderWithClipping (clip & interval' .~ Interval (clip ^. interval' . _Wrapped . _1 - offset, clip ^. interval' . to (.size))) child
            ContainedBy ->
              tell . pure =<< renderWithClipping (clip & interval' .~ Interval (0, sizeAlongAxis)) child
            OverlapsRight overlap ->
              tell . pure =<< renderWithClipping (clip & interval' .~ Interval (0, overlap.size)) child
            After -> throwError ()
          lift $ modify (+ sizeAlongAxis)

    pure case axisVal (Proxy @axis') of
      Row -> (T.justifyLeft clip.width ' ') <$> foldr1 (zipWith (<>)) childrenLines
      Column -> take clip.height $ join childrenLines <> repeat (T.replicate clip.width " ")
-- TODO: clipping
renderWithClipping clip (Clip {image}) = renderWithClipping clip image

type MonadRender m = (MonadReader (Int, Int) m, MonadIO m)
