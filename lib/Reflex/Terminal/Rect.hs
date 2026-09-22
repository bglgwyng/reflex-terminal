{-# LANGUAGE TemplateHaskell #-}

module Reflex.Terminal.Rect where

import Control.Lens.Internal.FieldTH (LensRules (_fieldToDef), makeFieldOptics)
import Language.Haskell.TH
import Reflex.Terminal.Interval
import Reflex.Terminal.Prelude

data Rect a = Rect
  { top :: a,
    left :: a,
    height :: a,
    width :: a
  }

makeLenses' 'Rect

xInterval :: (Num a) => Lens' (Rect a) (Interval a)
xInterval =
  lens
    (\rect -> Interval (rect ^. left, rect ^. left + rect ^. width))
    (\rect (Interval (start, end)) -> rect & left .~ start & width .~ end - start)

yInterval :: (Num a) => Lens' (Rect a) (Interval a)
yInterval =
  lens
    (\rect -> Interval (rect ^. top, rect ^. top + rect ^. height))
    (\rect (Interval (start, end)) -> rect & top .~ start & height .~ end - start)

translateX :: (Num a) => a -> Rect a -> Rect a
translateX dx rect =
  rect
    & xInterval
    . _Wrapped
    . _1
    %~ (+ dx)
    & xInterval
    . _Wrapped
    . _2
    %~ (+ dx)

bottom :: (Num a) => Rect a -> a
bottom rect = rect ^. top + rect ^. height

right :: (Num a) => (Rect a) -> a
right rect = rect ^. left + rect ^. width
