-- | Hilbert curve (x, y) to hilbert distance conversions. 
-- See <http://blog.notdot.net/2009/11/Damn-Cool-Algorithms-Spatial-indexing-with-Quadtrees-and-Hilbert-Curves>
-- and <http://www.compuphase.com/hilbert.htm>
module Hilbert (
  toHilbert
) where

import Data.Bits ((.&.), complement, shiftL)

-- | Convert a point to its Hilbert distance given a squace of n x n cells. 
-- Process recursively by looking at the high order bit and determining in
-- which quadrant the point is located. Rotate the input point according to
-- the hilbert curve pattern orientation in the quadrant (incrementing the 
-- distance by the right amount) and repeat.        
toHilbert :: Int        -- ^ order of the Hilbert curve
          -> (Int, Int) -- ^ the point to convert '(x, y)'
          -> Int        -- ^ the resulting hilbert distance 'd' of '(x, y)'
toHilbert order0 (x0, y0) =
  let rec 0      d  _     = d
      rec order  d (x, y) = 
        let highbit = 1 `shiftL` (order - 1)
            highbitmask = complement highbit
            nx = x .&. highbitmask -- trim highest bit
            ny = y .&. highbitmask -- trim highest bit
            quadrantsize = highbit * highbit
        in case (x .&. highbit, y .&. highbit) of
             (0, 0) -> -- first quadrant, flip diagonally
                               rec (order - 1)  d                     (ny, nx)
             (0, _) -> -- second quadrant, no rotation
                               rec (order - 1) (d +     quadrantsize) (nx, ny) 
             (_, 0) -> -- fourth quadrant, flip diagonally and around center
                               rec (order - 1) (d + 3 * quadrantsize) (quadrantsize - 1 - ny, quadrantsize - 1 - nx) 
             (_, _) -> -- third quadrant, no rotation
                               rec (order - 1) (d + 2 * quadrantsize) (nx, ny) 
  in rec order0 0 (x0, y0) 


