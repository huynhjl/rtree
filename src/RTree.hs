{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DisambiguateRecordFields #-}

-- | Lab2 from <http://www.scs.stanford.edu/11au-cs240h/labs/lab2.html>
-- Insertion and search for Hilbert R-Tree.
module RTree 
  ( Tree
  , insert
  , single
  , fromList
  , search
  , capacity
  , gridSize
  , treeStats) where

import qualified Hilbert as H
import Rect
import Data.Bits (shiftL)
import Control.Exception (assert)
import Data.List (sortBy)
import Data.Function (on)

type HilbertValue = Int
type MBR = Rect

-- | The entries stores the object, its mbr and hilbert value.
data (HasMBR a) => Entry a = Entry a MBR HilbertValue deriving (Show)

-- Leaf contains the entries.
-- Node contains either other nodes or leaves.
-- The RTree is supposed to be balanced. I am not sure how to enfore in the
-- type system that Leaf and Node should not be mixed.
data (HasMBR a) => Tree a = 
          Leaf { entries :: [Entry a], mbr :: MBR, lhv :: HilbertValue }
          | Node { children :: [Tree a], mbr :: MBR, lhv :: HilbertValue }
          deriving (Show)

-- | Some constants (capacity: how many child nodes/leaves per nodes).
-- hilbertOrder the order of the hilbert curve.
-- gridSize the size of the grid (gridSize x gridSize)                         
capacity, hilbertOrder, gridSize :: Int
capacity = 3
hilbertOrder = 16
gridSize = 1 `shiftL` hilbertOrder

-- | Computes hilbert value of the center of the rectangle.
-- > hilbertValue rect == Hilbert.toHilbert hilbertOrder $ center $ rect
hilbert :: MBR -> HilbertValue
hilbert rect = H.toHilbert hilbertOrder $ center rect

-- | Returns an tree with a single leaf with unique element.
single :: (HasMBR a) => a -> Tree a
single a = Node [leaf] rect hv
  where rect = getMBR a
        hv = hilbert rect
        entry = Entry a rect hv
        leaf = Leaf [entry] rect hv

-- | Create a leaf from a list of entries. 
makeLeaf :: (HasMBR a) => [Entry a] -> Tree a
makeLeaf entries = Leaf { entries=entries, mbr=leafmbr, lhv=leaflhv}
  where leafmbr = bound $ map (\(Entry _ rect _) -> rect) entries
        leaflhv = maximum $ map (\(Entry _ _ lhv) -> lhv) entries

-- | Create a node from a list of nodes/leaves.
makeNode :: (HasMBR a) => [Tree a] -> Tree a
makeNode nodes = Node { children=nodes, mbr=nodembr, lhv=nodelhv}
  where nodembr = bound $ map mbr nodes
        nodelhv = maximum $ map lhv nodes

{--- SEARCH ------------------------------------------------------------------}

-- | Search and return the entries rectangles intersecting with @rect@.
search :: (HasMBR a) => Tree a -> Rect -> [a]
search Leaf{entries, mbr} rect
        | mbr `intersect` rect = [a | Entry a r _ <- entries, r `intersect` rect]
        | otherwise = []
search Node{children, mbr} rect
        | mbr `intersect` rect =
            [a | c <- children, a <- search c rect, getMBR a `intersect` rect]
        | otherwise = []


{--- INSERTION ---------------------------------------------------------------}
        
-- | Insert action result
-- Indicates to the parent node what to do on update of the lower nodes.
data InsertAction a = Update (Tree a)         -- the focus node or leaf is updated
                    | LeafIsFull              -- the focus leaf is full
                    | Split (Tree a) (Tree a) -- too many entries or nodes 
                                              -- causing split

-- | Insert a rectangle object
insert0 :: (HasMBR a) => Tree a -> Entry a -> InsertAction a
insert0 Leaf{entries} _ | length entries == capacity = LeafIsFull
insert0 Leaf{entries, mbr, lhv} entry =
  Update Leaf{entries = entry : entries, mbr = mbr', lhv = lhv'}
    where Entry _ rect hv = entry
          mbr' = bound2 rect mbr
          lhv' = max lhv hv
insert0 node@Node{mbr=nodembr, lhv=nodelhv} entry =
  let Entry _ _ hv = entry
      (left, subnode, right) = focus node hv
      action = insert0 subnode entry
  in case action of
    Update newnode@Leaf{mbr=leafmbr, lhv=leaflhv} ->
      Update Node{  children=left ++ [newnode] ++ right
                  , mbr=bound2 nodembr leafmbr
                  , lhv=max nodelhv leaflhv}
    Update newnode@Node{mbr=nodembr', lhv=nodelhv'} ->
      Update Node{  children=left ++ [newnode] ++ right
                  , mbr=bound2 nodembr nodembr'
                  , lhv=max nodelhv nodelhv'}
    LeafIsFull ->
      -- at this point the type of (left, subnode, right)
      -- must be ([Leaf], Leaf, [Leaf])
      case rebalance (makeLeaf [entry] : children node) of
        (balanced, Nothing) -> Update balanced
        (balanced, Just extra) -> Split balanced extra
    Split balanced extra ->
      -- could possibly do better by not rebalancing if there is room in right
      case rebalance (left ++ [balanced, extra] ++ right) of
        (rebalanced, Nothing) -> Update rebalanced
        (rebalanced, Just otherextra) -> Split rebalanced otherextra
        
-- | Insert an object into the tree and return the tree
insert :: (HasMBR a) => Tree a -> a -> Tree a
insert tree a = 
  let rect = getMBR a
      entry = Entry a rect (hilbert rect)
  in case insert0 tree entry of
    Update updatedtree -> updatedtree
    Split balanced extra -> Node{children=[balanced, extra], mbr=mbr', lhv=lhv'}
      where mbr' = bound2 (mbr balanced) (mbr extra)
            lhv' = max (lhv balanced) (lhv extra)
    _ -> error "inconsistent insert action"

-- | Create a tree from a list of rectangles.
fromList :: (HasMBR a) => [a] -> Tree a
fromList [] = error "cannot create tree from empty list"
fromList (h : t) = foldl insert (single h) t
 
-- | Find the node whose lhv is more or equal than a given hilbert value
-- Return (left, node, right) so that the tree can be reconstructed when
-- node changes.
focus :: (HasMBR a) => Tree a -> HilbertValue -> ([Tree a], Tree a, [Tree a])
focus Node { children } hv =
  let (left, right) = span (\c -> lhv c < hv) children
      (left', node, right') = if null right
                                then (init left, last left, [])
                                else (left, head right, tail right)
  in (left', node, right')
focus _ _ = error "unimplemented" 

-- | Rebalance nodes so that children are evenly distributed.
-- The children are collected, then sorted, 'evenly' split in groups of at most
-- length capacity (less groups is better).
-- The count of nodes created can be beyond capacity in which case the extra
-- node is returned as the second value of the tuple.
-- This recomputes the mbr and lhv as well.
rebalance :: (HasMBR a) => [Tree a] -> (Tree a, Maybe (Tree a))
rebalance nodes =
  let ns = flattenedChildren nodes
      es = flattenedEntries nodes
      -- divup is `div` that rounds up
      divup num denom = (num `div` denom) + (if num `mod` denom == 0 then 0 else 1) 
      sizeNodes = if null ns then length es else length ns
      groupCounts = sizeNodes `divup` capacity

      go 0    0       []   = []
      go s    n       []   = error $ "illegal arguments " ++ show (s,n)
      go size nGroups list = prefix : go (size - count) (nGroups - 1) suffix
        where count = size `divup` nGroups
              (prefix, suffix) = splitAt count list

      trees = if null ns
                then map makeLeaf $ go sizeNodes groupCounts (sortBy (compare `on` \(Entry _ _ h) -> h) es)
                else map makeNode $ go sizeNodes groupCounts (sortBy (compare `on` lhv) ns)
        in case assert (groupCounts == length trees) groupCounts of
       c | c <= capacity -> (makeNode trees, Nothing)
       c | c == capacity + 1 -> (makeNode (init trees), Just (makeNode [last trees]))
       _ -> error "unexpected length"

-- | Return the children nodes or null 
-- This assumes that the first argument is [Node].
flattenedChildren :: (HasMBR a) => [Tree a] -> [Tree a]
flattenedChildren = concatMap sub
  where sub Leaf {} = []
        sub Node { children } = children
                  
-- | Return the children entries or null 
-- This assumes that the first argument is [Leaf].
flattenedEntries :: (HasMBR a) => [Tree a] -> [Entry a]
flattenedEntries = concatMap sub
  where sub Leaf { entries } = entries
        sub Node { } = []
        
-- | Stats on the tree
-- Returns (number of entries, number of nodes, number of leaves, depth)
treeStats :: (HasMBR a) => Tree a -> (Int, Int, Int, Int)
treeStats Leaf{entries} = (length entries, 0, 1, 1)
treeStats Node{children} =
  let stats = assert (length children <= capacity && length children > 0) (map treeStats children)
      reduction = foldl (\(a, b, c, d) (a', b', c', d') -> (a+a', b+b', c+c', d' : d)) (0, 0, 0, []) stats
      (size, nNodes, nLeaves, depths) = reduction
      depth = assert (maximum depths == minimum depths) (minimum depths)
  in (size, nNodes, nLeaves, depth)

                  