module Main where

import RTree
import Rect
import Data.List.Split (splitOn)
import Data.Char (isSpace)
import System.IO (isEOF)
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import Text.Printf (printf)
import Control.Monad (forM_, unless)

-- | Rectangle objects
-- We will use sourceLine to print out sample search results.
data Lab2Rect = Lab2Rect { sourceLine :: String
                         , rectangle :: Rect
                         } deriving (Show)

-- The minimum bouding box is the rectangle itself.
instance HasMBR Lab2Rect where getMBR = rectangle

main::IO()
main = do
  args <- getArgs
  _ <- if null args
         then fail "missing file name"
         else loadAndSearch $ head args
  return ()

-- | Returns the result of the IO action and the elapsed time in pico seconds.  
-- b is indirectly used to force the computation to be timed.
time :: IO (a, b) -> IO (Double, (a, b))
time action = do
  start <- getCPUTime
  (a, b) <- action
  end <- b `seq` getCPUTime
  return (fromIntegral (end - start), (a, b))


{---- Real application starts here -------------------------------------------}

loadAndSearch :: FilePath -> IO ()
loadAndSearch fileName = do
   (loadTime, (tree, count)) <- time $ load fileName 
   _ <- printf "%s: %d rectangles read in %.1f milliseconds\n"
          fileName count (loadTime / 1e9)
   timedSearch tree

-- | While not end of file, read a line from stdin, search, print timing and up
-- to 4 sample intersecting rectangles.  
timedSearch :: Tree Lab2Rect -> IO ()
timedSearch tree = do
  done <- isEOF
  unless done $ do
    line <- getLine
    (searchTime, (rects, count)) <- time $ do
                           let rects = processSearch tree line
                           return (rects, length rects)
    _ <- printf "found %d matches in %.0f microseconds:\n" count (searchTime/1e6)
    forM_ (take 4 rects) $ printf "    %s\n" . sourceLine
    timedSearch tree
  return ()

-- | Convert the query line to a rectangle, run a search, returns the
-- intersecting rectangles.
processSearch :: Tree Lab2Rect -> String -> [Lab2Rect]
processSearch tree = search tree . rectangle . fromLine

-- | Load the files converting from lines to Lab2Rect records.
-- Returns count of rectangle and RTree.
load :: FilePath -> IO (Tree Lab2Rect, Int)
load name = do
  content <- readFile name
  let rows = lines content
      rects = map fromLine rows
      tree = fromList rects
      -- we use size to force evaluation
      (size, _, _, _) = treeStats tree
  return (tree, size)    

-- | Convert a line into a Lab2Rect record 
-- Fails on parse error. 
fromLine :: String -> Lab2Rect
fromLine line | all isSpace line = error "Invalid search query"
fromLine line = 
  let ints = map read $ splitOn "," (filter (not . isSpace) line)
      x1 : y1 : x2 : y2 : x3 : y3 : x4 : y4 : _ = ints
      xl = minimum [x1, x2, x3, x4]
      yl = minimum [y1, y2, y3, y4]
      xh = maximum [x1, x2, x3, x4]
      yh = maximum [y1, y2, y3, y4]
  in if xl >= 0 && yl >= 0 && xh < gridSize && yh < gridSize
       then Lab2Rect{ sourceLine = line
                    , rectangle = Rect {xlow=xl, ylow=yl, xhigh=xh, yhigh=yh}
                    }
       else error $ "Cannot determine rectangle : " ++ line 

       