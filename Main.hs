-- | Loading Planes Simulation
--
-- The great quest to master Haskell/tackle interesting problems continues. I vaguely remember seeing a news article about simulations showing the most efficient way to load passengers onto a plane. As I recall, letting people on at random is least efficient, loading back-to-front somewhat efficient and loading windows-to-aisles most efficient. This project is a recreation of that simulation. The passengers will step in turn towards their seat and time will be the total number of steps required.
module Main where

import Data.Word (Word)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Test.HUnit hiding (Node, path)

-- * Types
-- | An (x,y) point that can be occupied.
data Node = Node Word Word deriving (Eq, Ord, Show)

type Weight = Word

-- * Test constants
origin :: Node
origin = Node 0 0

target :: Node
target = Node 3 8

-- | The natural L-shaped path that would be taken if no obstructions were present.
lPath :: [Node]
lPath = [Node 0 0, Node 0 1, Node 0 2, Node 0 3, Node 0 4, Node 0 5, Node 0 6, Node 0 7, Node 0 8, Node 1 8, Node 2 8, Node 3 8]

-- | A set of obstructions.
occupiedNodes :: Set.Set Node
occupiedNodes = Set.fromList [Node 0 7, Node 1 0, Node 1 1, Node 1 2, Node 1 3, Node 1 4, Node 1 7, Node 2 6]

-- | The optimal path around the obstructions.
mPath :: [Node]
mPath = [Node 0 0, Node 0 1, Node 0 2, Node 0 3, Node 0 4, Node 0 5, Node 1 5, Node 2 5, Node 3 5, Node 3 6, Node 3 7, Node 3 8]

-- * Tests
tests :: Test
tests = test [  TestCase (assertEqual "Adjacent" [Node 0 1, Node 0 3, Node 1 2] (adjacent $ Node 0 2)),
                TestCase (assertBool "Node clear" (nodeClear Set.empty target)),
                TestCase (assertBool "Node not clear" (not $ nodeClear (Set.fromList [target]) target)),
                TestCase (assertEqual "Insert if better" (Map.singleton target 0) (insertIfBetter target 0 Map.empty)),
                TestCase (assertEqual "Insert if better" (Map.singleton target 0) (insertIfBetter target 0 (Map.singleton target 1))),
                TestCase (assertEqual "Insert not better" (Map.singleton target 0) (insertIfBetter target 1 (Map.singleton target 0))),
                TestCase (assertEqual "Simple path" lPath (minPath Set.empty origin target)),
                TestCase (assertEqual "Complex path" mPath (minPath occupiedNodes origin target)),
                TestCase (assertBool "All squares" $ 100 > Map.size (makeGraph Set.empty origin target)) ]

-- * Functions
main :: IO ()
main = do
    c <- runTestTT tests
    print $ makeGraph Set.empty origin target
    print c

-- | Finds a minimum path around the obstructions from src to dest.
--
-- (1) Make a graph of nodes and weights.
--
-- (2) Starting from src, step onto adjacent node in graph with lowest weight.
--
-- (3) Repeat until destination reached.
minPath :: Set.Set Node     -- ^ Set of obstructed nodes
        -> Node             -- ^ Source node
        -> Node             -- ^ Destination node
        -> [Node]           -- ^ Minimum path
minPath obstructed src dest =
    let graph = makeGraph obstructed src dest
        rec s o
            | s == dest = reverse (s:o)
            | otherwise = rec (foldl minWeight (head $ f s) (f s)) (s:o)
        f s = filter (`Map.member` graph) $ adjacent s
        minWeight b a = let b' = graph Map.! b
                            a' = graph Map.! a
                        in  if b' <= a' then b else a
    in rec src []
-- TODO error handling


-- | Predicate for whether a node is in the 10x10 world or not.
inWorld :: Node -> Bool
inWorld (Node x y)
    | x > 9     = False
    | y > 9     = False
    | otherwise = True

-- | Make graph of nodes connecting src to dest, with weights corresponding to distance from dest.
-- Algorithm is as follows:
--
--  (1) Start with a list of nodes to process, containing just dest.
--
--  (2) Add dest to the graph with a weight of 0.
--
--  (3) For each node to process, find unobstructed adjacent nodes.
--
--  (4) For each of the adjacent nodes, add to graph with weight 1 greater than the processed node, unless it is already in the graph with a lower weight.
--
--  (5) Add each adjacent node that was added to the graph to the list of nodes to process.
--
--  (6) Continue until one of the adjacent nodes is target.
makeGraph   :: Set.Set Node         -- ^ Set of obstructed nodes
            -> Node                 -- ^ Source node
            -> Node                 -- ^ Destination node
            -> Map.Map Node Weight  -- ^ Weighted graph of nodes
makeGraph obstructed src dest = rec [dest] (Map.singleton dest 0)
    where
        rec [] m        = m
        rec (x:xs) m
                | x == src  = m
                | otherwise = rec (xs ++ adjs) (foldl addm m adjs)
            -- TODO ugly nested where, ugly guard
            where
                -- TODO unnecessary (?) extra lookup with insertIfBetter
                w = (m Map.! x) + 1
                adjs = filter (\n -> nodeClear obstructed n && newOrBetter n w m) $ adjacent x
                addm b a = insertIfBetter a w b

-- | Insert (node,weight) into map only if node is not in map or weight is lower than existing.
insertIfBetter :: Node -> Weight -> Map.Map Node Weight -> Map.Map Node Weight
insertIfBetter k a m =
    case Map.lookup k m of
        Nothing -> Map.insert k a m
        Just a' -> if a < a' then Map.insert k a m else m

-- | Is this weight lower than any in the graph so far for this node?
newOrBetter :: Node -> Weight -> Map.Map Node Weight -> Bool
newOrBetter k a m =
    case Map.lookup k m of
        Nothing -> True
        Just a' -> a < a'

-- | Is the node in the set of obstructions?
nodeClear :: Set.Set Node -> Node -> Bool
nodeClear obstructed node = not $ Set.member node obstructed

-- | The adjacent nodes (up to 4).
adjacent :: Node -> [Node]
adjacent (Node x y) = filter inWorld [Node x (y-1), Node x (y+1), Node (x-1) y, Node (x+1) y]

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
