module Main where

import Data.Word
import Test.HUnit hiding (Node, path)

data Node = Node Int Int Word

instance Show Node where
    show (Node x y n) = show (x,y,n)

instance Eq Node where
    Node x y _ == Node x' y' _ = (x,y) == (x',y')

instance Ord Node where
    compare (Node _ _ n) (Node _ _ n') = compare n n'

target :: Node
target = Node 3 8 0

origin :: Node
origin = Node 0 0 0

path :: [Node]
path = [Node 0 0 0, Node 1 0 10, Node 2 0 9, Node 3 0 8, Node 3 1 7, Node 3 2 6, Node 3 3 5, Node 3 4 4, Node 3 5 3, Node 3 6 2, Node 3 7 1, Node 3 8 0]

tests :: Test
tests = test [  TestCase (assertEqual "Self explored" False (unexplored [target] target)),
                TestCase (assertEqual "Self explored n+1" False (unexplored [target] (Node 3 8 1))),
                TestCase (assertEqual "Self explored multiple" False (unexplored [target, Node 3 7 0, Node 3 1 3] (Node 3 8 1))),
                TestCase (assertEqual "Explored but better" True (unexplored [Node 3 8 1] target)),
                TestCase (assertEqual "Unexplored" True (unexplored [Node 3 7 1] target)),
                TestCase (assertEqual "To origin" 90 (length $ findPaths origin target)),
                TestCase (assertEqual "To origin" origin (head $ findPaths origin target)),
                TestCase (assertEqual "Adjacent in set" [Node 1 0 10, Node 0 1 10] (adjacentInSet [Node 0 0 11, Node 1 0 10, Node 0 1 10, Node 2 0 9] (Node 0 0 11))),
                TestCase (assertEqual "Simple path" path (minPath origin target)) ]

main :: IO ()
main = do
    c <- runTestTT tests
    print c
    let k = findPaths origin target
    print k
    print $ length k
    print $ minPath origin target

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
adjacent (Node x y n)  = filter nodeClear [Node (x-1) y (n+1), Node (x+1) y (n+1), Node x (y-1) (n+1), Node x (y+1) (n+1)]

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
