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

-- | Weight of a node in the graph (distance from dest).
type Weight = Word

-- | Graph of weighted nodes.
type Graph = Map.Map Node Weight

-- * Test constants
-- | Origin node (0,0).
origin :: Node
origin = Node 0 0

-- | Arbitrary target node (3,8).
target :: Node
target = Node 3 8

-- | The natural L-shaped path that would be taken if no obstructions were present.
lPath :: [Node]
lPath = [Node 0 0, Node 0 1, Node 0 2, Node 0 3, Node 0 4, Node 0 5, Node 0 6, Node 0 7, Node 0 8, Node 1 8, Node 2 8, Node 3 8]

-- | A set of obstructions.
occupiedNodes :: Set.Set Node
occupiedNodes = Set.fromList [Node 0 7, Node 1 0, Node 1 1, Node 1 2, Node 1 3, Node 1 4, Node 1 7, Node 2 6]

-- | A blocking set of obstructions
wallOfNodes :: Set.Set Node
wallOfNodes = Set.fromList $ map (Node 2) [0..9]

-- | The optimal path around the obstructions.
mPath :: [Node]
mPath = [Node 0 0, Node 0 1, Node 0 2, Node 0 3, Node 0 4, Node 0 5, Node 1 5, Node 2 5, Node 3 5, Node 3 6, Node 3 7, Node 3 8]

-- * Tests
-- | Simple unit tests
tests :: Test
tests = test [  TestCase (assertEqual "Adjacent" [Node 0 1, Node 0 3, Node 1 2] (adjacent $ Node 0 2)),
                TestCase (assertBool "Node clear" (nodeClear Set.empty target)),
                TestCase (assertBool "Node not clear" (not $ nodeClear (Set.fromList [target]) target)),
                TestCase (assertEqual "Trivial path" (Just [origin]) (minPath occupiedNodes origin origin)),
                TestCase (assertEqual "Simple path" (Just lPath) (minPath Set.empty origin target)),
                TestCase (assertEqual "Complex path" (Just mPath) (minPath occupiedNodes origin target)),
                TestCase (assertEqual "Impossible path" Nothing (minPath wallOfNodes origin target)),
                TestCase (assertBool "All squares" $ 100 > Map.size (makeGraph Set.empty origin target)) ]

-- * Functions
-- | Run and print tests.
main :: IO ()
main = do
    c <- runTestTT tests
    print $ makeGraph Set.empty origin target
    print $ minPath occupiedNodes origin target
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
        -> Maybe [Node]     -- ^ Minimum path if exists
minPath obstructed src dest = rec (Just src) []
    where
        graph = makeGraph obstructed src dest
        rec ms o = ms >>= \s ->
            if s == dest
            then Just $ reverse (s:o)
            else rec (nextNode graph s) (s:o)

-- | Select the node with the lower weight from the graph.
lowerWeight :: Graph -> Node -> Node -> Node
lowerWeight graph b a =
        let b' = graph Map.! b
            a' = graph Map.! a
        in  if b' <= a' then b else a

-- | Calculate next node in graph to move to.
nextNode :: Graph -> Node -> Maybe Node
nextNode graph node =
    let possibles = filter (`Map.member` graph) $ adjacent node
    in case possibles of
        []      -> Nothing
        [n]     -> Just n
        (n:ns)  -> Just $ foldl (lowerWeight graph) n ns

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
makeGraph   :: Set.Set Node -- ^ Set of obstructed nodes
            -> Node         -- ^ Source node
            -> Node         -- ^ Destination node
            -> Graph        -- ^ Weighted graph of nodes
makeGraph obstructed src dest = rec [dest] (Map.singleton dest 0)
    where
        rec [] m        = m
        rec (x:xs) m
                | x == src  = m
                | otherwise = rec (xs ++ adjs) (foldl addm m adjs)
            -- TODO ugly nested where, ugly guard
            where
                (adjs, w) = graphable obstructed m x
                addm m' a = Map.insert a w m'

-- | Get permissible adjacent nodes.
graphable   :: Set.Set Node     -- ^ Obstructed nodes
            -> Graph            -- ^ Graph-so-far
            -> Node             -- ^ Node in question
            -> ([Node], Weight) -- ^ Adjacent nodes and their weight
graphable obstructed graph node =
    let w   = (graph Map.! node) + 1
        adj = filter (\n -> nodeClear obstructed n && newOrBetter n w graph) $ adjacent node
    in  (adj, w)

-- | Is this weight lower than any in the graph so far for this node?
newOrBetter :: Node -> Weight -> Graph -> Bool
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
