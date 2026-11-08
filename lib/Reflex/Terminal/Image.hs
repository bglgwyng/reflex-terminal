{-# LANGUAGE ImpredicativeTypes #-}

module Reflex.Terminal.Image (
  Renderer,
  box,
  horizontal,
  vertical,
  fill,
)
where

import Control.Exception.Base
import Control.Monad.Reader
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Text.IO ()
import Reflex
import Reflex.Terminal.Prelude

type Image t = Dynamic t [Dynamic t T.Text]
type Renderer t =
  forall m.
  (MonadReader (Dynamic t Int, Dynamic t Int) m) =>
  m (Image t)

fill :: (Reflex t) => Char -> Renderer t
fill ch = do
  (dHeight, dWidth) <- ask
  pure $ dHeight <&> (`replicate` (dWidth <&> T.pack . (`replicate` ch)))

box :: (Reflex t) => Char -> Renderer t -> Renderer t
box char body = do
  (dHeight, dWidth) <- ask
  pure do
    height <- dHeight
    assert (height >= 2) (pure ())
    let horizontalBar = dWidth <&> T.pack . (`replicate` char)
    ( cons horizontalBar . (`snoc` horizontalBar)
        <$> fmap (fmap (T.cons char . (`T.snoc` char)))
        <$> (body (dHeight - 2, dWidth - 2))
      )

-- TODO: add clipping
horizontal :: forall t k. (Reflex t) => M.Map k (Int, Renderer t) -> Renderer t
horizontal children = do
  (dHeight, dWidth) <- ask
  let bodyWidth = sum $ fst <$> M.elems children
      dPadWidth = dWidth - pure bodyWidth
      dPadding = (`replicate` ((`T.replicate` " ") <$> dPadWidth)) <$> dHeight

  pure $
    foldr1
      concatHorizontal
      ( M.elems children <&> \(width, child) -> do
          child (dHeight, pure width)
      )
      `concatHorizontal` dPadding
 where
  concatHorizontal = zipDynWith (zipWith (<>))

-- TODO: add clipping
vertical :: (Reflex t) => M.Map k (Int, Renderer t) -> Renderer t
vertical children = do
  (dHeight, dWidth) <- ask
  let bodyHeight = sum $ fst <$> M.elems children
      dPadHeight = dHeight - pure bodyHeight
      dPadding = (`replicate` ((`T.replicate` " ") <$> dWidth)) <$> dPadHeight
  pure $
    ( join
        <$> distributeListOverDyn
          ( (M.elems children) <&> \(height, child) -> do
              child (pure height, dWidth)
          )
    )
      <> dPadding
