{-# LANGUAGE UndecidableInstances #-}

module Reflex.Terminal.Image (Axis (..), Image (..), ContainerChild (..), renderWithClipping) where

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
import Reflex
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

data Image t where
  Span :: {text :: T.Text} -> Image t
  Container ::
    forall t axis.
    (KnownAxis axis, Measured (Dynamic t (ContainerChildMetric axis)) (Dynamic t (ContainerChild t axis))) =>
    { children :: FingerTree (Dynamic t (ContainerChildMetric axis)) (Dynamic t (ContainerChild t axis))
    } ->
    Image t
  Clip :: {rect :: Rect Int, image :: Image t} -> Image t

instance IsString (Image t) where
  fromString string = Span (T.pack string)

newtype ContainerChild t (axis :: Axis) = ContainerChild {unContainerChild :: Image t}

data ContainerChildMetric (axis :: Axis) = ImageMetric
  { alongAxis :: Sum Int
  , acrossAxis :: Max Int
  }

instance Semigroup (ContainerChildMetric axis) where
  im1 <> im2 =
    ImageMetric
      { alongAxis = im1.alongAxis <> im2.alongAxis
      , acrossAxis = im1.acrossAxis <> im2.acrossAxis
      }

instance Monoid (ContainerChildMetric axis) where
  mempty = ImageMetric{alongAxis = 0, acrossAxis = 0}

instance (Reflex t) => Measured (Dynamic t (ContainerChildMetric 'Row)) (Dynamic t (ContainerChild t 'Row)) where
  measure dc = do
    ContainerChild image <- dc
    width <- image.width
    height <- image.height
    pure ImageMetric{alongAxis = Sum width, acrossAxis = Max height}

instance (Reflex t) => Measured (Dynamic t (ContainerChildMetric 'Column)) (Dynamic t (ContainerChild t 'Column)) where
  measure dc = do
    ContainerChild image <- dc
    width <- image.width
    height <- image.height
    pure ImageMetric{alongAxis = Sum height, acrossAxis = Max width}

instance (Reflex t) => HasField "height" (Image t) (Dynamic t Int) where
  getField Span{} = 1
  getField (Container @_ @axis children) =
    case axisVal $ Proxy @axis of
      Row -> do
        x <- measure children
        pure x.acrossAxis.getMax
      Column -> do
        x <- measure children
        pure x.alongAxis.getSum
  getField (Clip{rect}) = pure rect.height

instance (Reflex t) => HasField "width" (Image t) (Dynamic t Int) where
  getField span@Span{} = do
    pure $ T.foldl' (\l c -> wcwidth c + l) 0 span.text
  getField (Container @_ @axis children) =
    case axisVal $ Proxy @axis of
      Row -> do
        x <- measure children
        pure x.alongAxis.getSum
      Column -> do
        x <- measure children
        pure x.acrossAxis.getMax
  getField (Clip{rect}) = pure rect.width

renderWithClipping :: (Reflex t) => Rect Int -> Image t -> Dynamic t [T.Text]
renderWithClipping clip span@Span{}
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
renderWithClipping clip container@(Container @_ @axis children) = do
  let interval' :: Lens' (Rect Int) (Interval Int)
      interval' = case axisVal (Proxy @axis) of
        Row{} -> xInterval
        Column{} -> yInterval

  (`evalStateT` 0) do
    childrenLines :: [[T.Text]] <- execWriterT $
      runExceptT $ do
        for_ children \dc -> do
          containerChild <- lift . lift . lift $ dc
          let ContainerChild child = containerChild
          offset :: Int <- lift get
          metric <- lift . lift . lift $ (measure dc)
          let sizeAlongAxis = metric.alongAxis.getSum
              childInterval :: Interval Int
              childInterval = Interval (offset, offset + sizeAlongAxis)

          case intersection childInterval (clip ^. interval') of
            Before -> pure ()
            OverlapsLeft overlap -> do
              tell . pure =<< (lift . lift . lift $ renderWithClipping (clip & interval' .~ Interval (overlap ^. _Wrapped . _1 - offset, overlap.size)) child)
            Contains ->
              assert (offset <= (clip ^. interval' . _Wrapped . _1)) $
                tell . pure
                  =<< ( lift . lift . lift $
                          renderWithClipping (clip & interval' .~ Interval (clip ^. interval' . _Wrapped . _1 - offset, clip ^. interval' . to (.size))) child
                      )
            ContainedBy ->
              tell . pure =<< (lift . lift . lift $ renderWithClipping (clip & interval' .~ Interval (0, sizeAlongAxis)) child)
            OverlapsRight overlap ->
              tell . pure =<< (lift . lift . lift $ renderWithClipping (clip & interval' .~ Interval (0, overlap.size)) child)
            After -> throwError ()
          lift $ modify (+ sizeAlongAxis)

    pure case axisVal (Proxy @axis) of
      Row -> (T.justifyLeft clip.width ' ') <$> foldr1 (zipWith (<>)) childrenLines
      Column -> take clip.height $ join childrenLines <> repeat (T.replicate clip.width " ")
-- TODO: clipping
renderWithClipping clip (Clip{image}) = renderWithClipping clip image

instance (MonadSample t m, Monoid w) => MonadSample t (WriterT w m) where
  sample = lift . sample
