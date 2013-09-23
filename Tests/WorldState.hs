-- | Simple tests for world state
module Tests.WorldState
( tests
) where

import Control.Monad.State
import Data.Sequence (singleton)
import qualified Data.Set as Set
import Test.HUnit
import Passenger
import Path
import Plane
import WorldState

-- | Set of obstructions after first passenger step
carriage' :: Blocks
carriage' = Set.insert (Node 1 3) carriage

carriage'' :: Blocks
carriage'' = Set.insert (Node 1 2) carriage

-- | Initial passengers
passengers :: Queue
passengers = singleton newPassenger

-- | Passengers after first step (starting from (1,4))
passengers' :: Queue
passengers' = singleton $ newPassenger { location = Node 1 3 }

passengers'' :: Queue
passengers'' = singleton $ newPassenger { location = Node 1 2 }

-- | Test cases for WorldState
tests :: Test
tests = test [  TestCase (assertEqual "0th step" (Just (carriage, passengers)) (runStateT (return carriage) passengers)),
                TestCase (assertEqual "1st step" (Just (carriage', passengers')) (runStateT (step carriage) passengers)),
                TestCase (assertEqual "2nd step" (Just (carriage'', passengers'')) (runStateT (step carriage >>= step) passengers)) ]
