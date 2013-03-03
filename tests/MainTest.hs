{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DisambiguateRecordFields #-}

module Main

where

import Data.Monoid (mempty)
import Test.Framework (Test, defaultMain, defaultMainWithOpts, testGroup)
import Test.Framework.Options (TestOptions, TestOptions'(..))
import Test.Framework.Runners.Options (RunnerOptions, RunnerOptions'(..))
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck

import qualified Data.List as L
import Data.Bits (shiftL)
import Data.Function (on)
import Hilbert
import RTree
import Rect

main :: IO ()
main = defaultMain tests

mainWithOpts :: IO ()
mainWithOpts = do
  -- Test options can also be specified in the code. The TestOptions
  -- type is an instance of the Monoid type class, so the easiest way
  -- to get an empty set of options is with `mempty`.
  let empty_test_opts = mempty :: TestOptions

  -- We update the empty TestOptions with our desired values.
  let my_test_opts = empty_test_opts {
    topt_maximum_generated_tests = Just 500
  }

  -- Now we create an empty RunnerOptions in the same way, and add
  -- our TestOptions to it.
  let empty_runner_opts = mempty :: RunnerOptions
  let my_runner_opts = empty_runner_opts {
    ropt_test_options = Just my_test_opts
  }

  defaultMainWithOpts tests my_runner_opts

tests :: [Test]
tests = [
        testGroup "Hilbert" [
                testProperty "unique distances" hilbert_unique_distances,
                testProperty "adjacent points" hilbert_adjacent_points,
                testProperty "start and end in corners" hilbert_start_end
            ],
        testGroup "Rect" [
                testProperty "rectangle intersection" test_rect_intersect,
                testProperty "rectangle intersection2" test_rect_intersect2
            ],
        testGroup "RTree" [
                testProperty "make rtrees" rtree_no_fail
                , testProperty "rtree is balanced" rtree_is_balanced
                , testProperty "rtree node utilization" node_utilization
                , testProperty "rtree search" rtree_search
                , testProperty "rtree search moved" rtree_search_moved
            ]
    ]


{---- Hilbert Curve Tests ----------------------------------------------------}

hilbert_unique_distances :: Int -> Property
hilbert_unique_distances order = 
  order <= 8 && order > 0 ==>
  let size = shiftL (1::Int) order
      l = size - 1
      pts = [(x, y) | x <- [0 .. l], y <- [0 .. l]]
      distances = map (toHilbert order) pts 
      n = size * size - 1
  in L.sortBy compare distances == [0 .. n] 

hilbert_adjacent_points :: Int -> Property
hilbert_adjacent_points order = 
  order <= 8 && order > 0 ==>
  let size = shiftL (1::Int) order
      l = size - 1
      pts = [(x, y) | x <- [0 .. l], y <- [0 .. l]]
      distances = map (toHilbert order) pts 
      zipped = zip pts distances
      sorted = L.sortBy (compare `on` snd) zipped
      sortedPoints = map fst sorted
      window = zip sortedPoints $ tail sortedPoints 
  in all adjacent window
    where adjacent ((x1, y1), (x2, y2)) | abs (x1 - x2) == 1 && y1 == y2 = True
                                        | abs (y1 - y2) == 1 && x1 == x2 = True
                                        | otherwise = False

hilbert_start_end :: Int -> Property
hilbert_start_end order = 
  order <= 20 && order > 0 ==>
  let size = shiftL (1::Int) order
      l = size - 1
      start = toHilbert order (0, 0)
      end = toHilbert order (l, 0)
  in start == 0 && end == (size * size - 1)


{---- Rectangle Tests --------------------------------------------------------}

-- | Arbitrary instance for Rect
-- Wrap in newtype due to orphan instance warning.
newtype ArbRect = ArbRect { getRect :: Rect } deriving (Show)
instance Arbitrary ArbRect where
  arbitrary = sized $ \s -> do
    xl <- choose (0, s)
    yl <- choose (0, s)
    xh <- choose (xl, s)
    yh <- choose (yl, s)
    return $ ArbRect Rect{xlow=xl, ylow=yl, xhigh=xh, yhigh=yh}
    
test_rect_intersect :: ArbRect -> ArbRect -> Bool
test_rect_intersect (ArbRect rect1) (ArbRect rect2) = 
  rect1 `intersect` rect2 /= null (points rect1 `L.intersect` points rect2)

test_rect_intersect2 :: Property
test_rect_intersect2 = mapSize (\_ -> 50) $ property test_rect_intersect


{---- Hilbert RTree Tests ----------------------------------------------------}

newtype ArbTree = ArbTree { getTree :: Tree Rect } deriving (Show)
instance Arbitrary ArbTree where
  arbitrary = sized $ \s -> do
    arbRects <- listOf1 (resize s arbitrary :: Gen ArbRect)
    let rects = map getRect arbRects
        first : rest = rects
        tree0 = single first
        tree = foldl insert tree0 rest
    return $ ArbTree tree

-- tests that we can construct the trees
rtree_no_fail :: Gen Bool
rtree_no_fail = do
    arbRects <- listOf1 (arbitrary :: Gen ArbRect)
    let rects = map getRect arbRects
        tree = fromList rects
        (l, _, _, _) = treeStats tree
    return $ l == length rects

-- | The @treeStats@ function uses an assert that should ensure the tree is 
-- balanced.
rtree_is_balanced :: ArbTree -> Bool
rtree_is_balanced (ArbTree tree) =
  let (_, _, _, depth) = treeStats tree
  in depth > 0
  
-- | The @treeStats@ function uses an assert that should ensure the tree
-- nodes and leaves don't go over @capacity@.
node_utilization :: ArbTree -> Bool
node_utilization (ArbTree tree) =
  let (size, _, nLeaves, _) = treeStats tree
  in size < capacity * 5 
      || (fromIntegral size / fromIntegral (nLeaves*capacity) :: Double) >= 0.75
  
rtree_search :: Gen Bool
rtree_search = do
    arbRects <- listOf1 (arbitrary :: Gen ArbRect)
    let rects = map getRect arbRects
        first : rest = rects
        tree0 = single first
        tree = foldl insert tree0 rest
    return $ L.all (\r -> length (search tree r) >= 1) rects

-- | Move the rectangles a random amount so that we don't have too much overlap
rtree_search_moved :: Gen Bool
rtree_search_moved = do
    arbRects <- resize 1000 (listOf1 (resize 100 arbitrary :: Gen ArbRect))
    let l = length arbRects
    dxs <- vectorOf l (choose(0, 3000))
    dys <- vectorOf l (choose(0, 3000))
    let ds = dxs `zip` dys
        rects0 = map getRect arbRects
        rects = map (\((Rect xl yl xh yh), (dx, dy)) -> Rect (xl+dx) (yl+dy) (xh+dx) (yh+dy)) (rects0 `zip` ds)
        first : rest = rects
        tree0 = single first
        tree = foldl insert tree0 rest
    return $ L.all (\r -> length (search tree r) >= 1) rects
    