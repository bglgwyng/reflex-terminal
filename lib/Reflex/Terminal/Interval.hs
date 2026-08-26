{-# LANGUAGE DerivingStrategies #-}

module Reflex.Terminal.Interval where

type Interval a = (a, a)

data Intersection a
  = Before
  | OverlapsLeft (Interval a)
  | Contains
  | ContainedBy
  | OverlapsRight (Interval a)
  | After

deriving instance (Show a) => Show (Intersection a)

deriving instance (Eq a) => Eq (Intersection a)

-- | Classify the first half-open interval relative to the second.
--
-- >>> intersection @Int (0, 2) (2, 4)
-- Before
-- >>> intersection @Int (0, 3) (2, 4)
-- OverlapsLeft (2,3)
-- >>> intersection @Int (0, 4) (1, 3)
-- Contains
-- >>> intersection @Int (1, 3) (0, 4)
-- ContainedBy
-- >>> intersection @Int (2, 5) (1, 4)
-- OverlapsRight (2,4)
-- >>> intersection @Int (4, 6) (1, 4)
-- After
intersection :: (Ord a) => Interval a -> Interval a -> Intersection a
intersection (start, end) (otherStart, otherEnd)
  | end <= otherStart = Before
  | start >= otherEnd = After
  | start <= otherStart && end >= otherEnd = Contains
  | start >= otherStart && end <= otherEnd = ContainedBy
  | start < otherStart =
      OverlapsLeft (otherStart, end)
  | otherwise =
      OverlapsRight (start, otherEnd)

size :: (Num a) => Interval a -> a
size (start, end) = end - start
