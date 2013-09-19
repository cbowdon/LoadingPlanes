-- | Simple path-finding functionality using Dijkstra's algorithm.
module Path
( -- * Types
  Node(..)
, Weight
, Graph
, Path
  -- * Functions
, minPath
, makeGraph
, adjacent
, nodeClear
) where

import Data.Word (Word)
import qualified Data.Set as Set
import qualified Data.Map as Map

-- * Types
-- | An (x,y) point that can be occupied.
data Node = Node Word Word deriving (Eq, Ord, Show)

-- | Weight of a node in the graph (distance from dest).
type Weight = Word

-- | Graph of weighted nodes.
type Graph = Map.Map Node Weight

-- | Ordered forward-path through nodes
type Path = [Node]

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
        -> Maybe Path     -- ^ Minimum path if exists
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
            -> (Path, Weight) -- ^ Adjacent nodes and their weight
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
adjacent :: Node -> Path
adjacent (Node x y) = filter inWorld [Node x (y-1), Node x (y+1), Node (x-1) y, Node (x+1) y]
