-- | Rectangle functions to support RTree and to support testing.
module Rect where

-- | Rectangle defined by its lower left corner and upper right corner.
data Rect = Rect {
  xlow :: Int,
  ylow :: Int,
  xhigh :: Int,
  yhigh :: Int
} deriving (Show, Eq)

-- | A typeclass for object that have a minimum bounding rectangle (mbr)
class (Show a) => HasMBR a where
  getMBR :: a -> Rect

-- | A rectangle is its own MBR.
instance HasMBR Rect where
  getMBR rect = rect


-- | The center of the rectangle (+/- some rounding).
center :: Rect -> (Int, Int)
center Rect { xlow=xl, ylow=yl, xhigh=xh, yhigh=yh } =
  ((xl + xh) `div` 2, (yl + yh) `div` 2)

-- | The boudind box of two rectangles.
bound2 :: Rect -> Rect -> Rect
bound2 (Rect xl yl xh yh) (Rect xl' yl' xh' yh') =
  Rect (min xl xl') (min yl yl') (max xh xh') (max yh yh')

-- | The bounding rectangle of a list of input rectangles.
bound :: [Rect] -> Rect
bound = foldl1 bound2

-- | Returns True if the input rectangles intersect false otherwise.
intersect :: Rect -> Rect -> Bool
intersect (Rect xl yl xh yh) (Rect xl' yl' xh' yh')
  | (xl, xh) `overlap` (xl',xh') && (yl, yh) `overlap` (yl', yh') = True
  | otherwise = False
  
overlap :: (Int, Int) -> (Int, Int) -> Bool
overlap (a, b) (c, d) 
  | a > d || b < c = False
  | otherwise = True

-- | Returns True if the point is in the rectangle (or on its perimeter).
contains :: Rect -> (Int, Int) -> Bool
contains (Rect xl yl xh yh) (x, y)
  | x >= xl && x <= xh && y >= yl && y <= yh = True
  | otherwise = False
  
-- | Returns all points contained in Rect
points :: Rect -> [(Int, Int)]
points Rect{xlow=xl, ylow=yl, xhigh=xh, yhigh=yh} =
  [(x, y) | x <- [xl..xh], y <- [yl..yh]]
