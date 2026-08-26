module Reflex.Terminal.Rect where

data Rect = Rect
  { top :: Int,
    left :: Int,
    height :: Int,
    width :: Int
  }

bottom :: Rect -> Int
bottom rect = rect.top + rect.height

right :: Rect -> Int
right rect = rect.left + rect.width

fromSize :: (Int, Int) -> Rect
fromSize (height, width) = Rect {top = 0, left = 0, height, width}

translateY :: Int -> Rect -> Rect
translateY dy rect = rect {top = rect.top + dy}

translateX :: Int -> Int -> Rect
translateX height width = Rect {top = 0, left = 0, height, width}

verticalInterval :: Rect -> (Int, Int)
verticalInterval rect = (rect.top, bottom rect)

horizontalInterval :: Rect -> (Int, Int)
horizontalInterval rect = (rect.left, right rect)
