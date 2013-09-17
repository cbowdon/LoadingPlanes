{- |
Loading Planes Simulation

The great quest to master Haskell/tackle interesting problems continues. I vaguely remember seeing a news article about simulations showing the most efficient way to load passengers onto a plane. As I recall, letting people on at random is least efficient, loading back-to-front somewhat efficient and loading windows-to-aisles most efficient. This project is a recreation of that simulation. The passengers will step in turn towards their seat and time will be the total number of steps required.

-}
module Main where

import Data.Word (Word)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Test.HUnit hiding (Node, path)

-- * Types
-- | An (x,y) point that can be occupied.
data Node = Node Word Word deriving (Eq, Ord, Show)

-- * Test constants
origin :: Node
origin = Node 0 0

target :: Node
target = Node 3 8

-- | The natural L-shaped path that would be taken if no obstructions were present.
lPath :: [Node]
lPath = [Node 0 0, Node 1 0, Node 2 0, Node 3 0, Node 3 1, Node 3 2, Node 3 3, Node 3 4, Node 3 5, Node 3 6, Node 3 7, Node 3 8]

-- | A set of obstructions.
occupiedNodes :: Set.Set Node
occupiedNodes = Set.fromList [Node 0 7, Node 1 0, Node 1 1, Node 1 2, Node 1 3, Node 1 4, Node 1 7, Node 2 6]

-- | The optimal path around the obstructions.
mPath :: [Node]
mPath = [Node 0 0, Node 0 1, Node 0 2, Node 0 3, Node 0 4, Node 0 5, Node 1 5, Node 2 5, Node 3 5, Node 3 6, Node 3 7, Node 3 8]

-- * Tests
tests :: Test
tests = test [  TestCase (assertEqual "Simple path" lPath (minPath Set.empty origin target)),
                TestCase (assertEqual "Simple path" mPath (minPath occupiedNodes origin target)) ]
-- * Functions
main :: IO ()
main = print "Hello world"

-- | Finds a minimum path around the obstructions from src to dest.
minPath :: Set.Set Node     -- ^ Set of obstructed nodes
        -> Node             -- ^ Source node
        -> Node             -- ^ Destination node
        -> [Node]           -- ^ Minimum path
minPath = undefined

-- | Predicate for whether a node is in the 10x10 world or not.
inWorld :: Node -> Bool
inWorld (Node x y)
    | x > 9     = False
    | y > 9     = False
    | otherwise = True

-- | Make graph of nodes connecting src to dest, with weights corresponding to distance from dest.
makeGraph   :: Set.Set Node         -- ^ Set of obstructed nodes
            -> Node                 -- ^ Source node
            -> Node                 -- ^ Destination node
            -> Map.Map Node Word    -- ^ Weighted graph of nodes
makeGraph = undefined

{-
-- walk by moving to adjacent cell with lowest count
minPath :: Node -> Node -> [Node]
minPath src dest =
    let paths = findPaths src dest
    in  rec' src dest paths [src]

rec' :: Node -> Node -> [Node] -> [Node] -> [Node]
rec' curr dest paths output =
    let next = minimum $ canStep curr paths output
        newOutput = next:output
    in if next == dest then reverse newOutput else rec' next dest paths newOutput

canStep :: Node -> [Node] -> [Node] -> [Node]
canStep curr paths output = filter (\n -> (n `elem` paths) && (n `notElem` output)) (adjacentInSet paths curr)

adjacentInSet :: [Node] -> Node -> [Node]
adjacentInSet set node = filter (`elem` adjacent node) set

nodeClear :: Node -> Bool
nodeClear (Node x y _)
    | x < 0     = False
    | y < 0     = False
    | x > 9     = False
    | y > 9     = False
    | otherwise = True

adjacent :: Node -> [Node]
adjacent (Node x y)  = filter nodeClear [Node (x-1) y, Node (x+1) y, Node x (y-1), Node x (y+1)]

findPaths :: Node -> Node -> [Node]
findPaths src dest = rec src [dest] []

-- for each point in original list
-- calc allowable adjacent nodes and inc
-- add to original list
-- until src is hit
rec :: Node -> [Node] -> [Node] -> [Node]
rec src input output =
    case input of
        []      -> output
        (x:xs)  -> if x == src then x:output else rec src (xs ++ filter (unexplored (xs ++ output)) (adjacent x)) (x:output)
-- TODO why the fuck are we using lists here? Sounds like it's begging for a tree.

unexplored :: [Node] -> Node -> Bool
unexplored explored that = foldl (\b a -> b && diffOrBetter that a) True explored

diffOrBetter :: Node -> Node -> Bool
diffOrBetter (Node x y n) (Node x' y' n') = x /= x' || y /= y' || x == x' && y == y' && n < n'
-}
