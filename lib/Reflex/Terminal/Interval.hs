{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Reflex.Terminal.Interval where

import Reflex.Terminal.Prelude

newtype Interval a = Interval (a, a) deriving (Show, Eq)

makeWrapped 'Interval

data Intersection a
  = Before
  | OverlapsLeft (Interval a)
  | Contains
  | ContainedBy
  | OverlapsRight (Interval a)
  | After

deriving instance (Show a) => Show (Intersection a)

deriving instance (Eq a) => Eq (Intersection a)

{- | Classify the first half-open interval relative to the second.

>>> intersection @Int (Interval (0, 2)) (Interval (2, 4))
Before
>>> intersection @Int (Interval (0, 3)) (Interval (2, 4))
OverlapsLeft (Interval (2,3))
>>> intersection @Int (Interval (0, 4)) (Interval (1, 3))
Contains
>>> intersection @Int (Interval (1, 3)) (Interval (0, 4))
ContainedBy
>>> intersection @Int (Interval (2, 5)) (Interval (1, 4))
OverlapsRight (Interval (2,4))
>>> intersection @Int (Interval (4, 6)) (Interval (1, 4))
After
-}
intersection :: (Ord a) => Interval a -> Interval a -> Intersection a
intersection (Interval (start, end)) (Interval (otherStart, otherEnd))
  | end <= otherStart = Before
  | start >= otherEnd = After
  | start <= otherStart && end >= otherEnd = Contains
  | start >= otherStart && end <= otherEnd = ContainedBy
  | start < otherStart =
      OverlapsLeft (Interval (otherStart, end))
  | otherwise =
      OverlapsRight (Interval (start, otherEnd))

instance (Num a) => HasField "size" (Interval a) a where
  getField (Interval (start, end)) = end - start
