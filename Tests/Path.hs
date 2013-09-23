-- | Simple unit tests for the Path module.
module Tests.Path where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Test.HUnit hiding (Node, path, Path)
import Path

-- * Test constants
-- | Origin node (0,0).
origin :: Node
origin = Node 0 0

-- | Arbitrary target node (3,8).
target :: Node
target = Node 3 8

-- | The natural L-shaped path that would be taken if no obstructions were present.
lPath :: Path
lPath = [Node 0 0, Node 0 1, Node 0 2, Node 0 3, Node 0 4, Node 0 5, Node 0 6, Node 0 7, Node 0 8, Node 1 8, Node 2 8, Node 3 8]

-- | A set of obstructions.
occupiedNodes :: Set.Set Node
occupiedNodes = Set.fromList [Node 0 7, Node 1 0, Node 1 1, Node 1 2, Node 1 3, Node 1 4, Node 1 7, Node 2 6]

-- | A blocking set of obstructions
wallOfNodes :: Set.Set Node
wallOfNodes = Set.fromList $ map (Node 2) [0..1000]

-- | The optimal path around the obstructions.
mPath :: Path
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
                TestCase (assertBool "Unrestricted graph" $ 1000 >= Map.size (makeGraph Set.empty origin target)) ]
