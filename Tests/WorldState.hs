-- | Simple tests for world state
module Tests.WorldState
( tests
) where

import Control.Monad.State
import Data.Sequence (singleton, ViewL(..), viewl, (|>))
import qualified Data.Set as Set
import Test.HUnit
import Passenger
import Path
import Plane
import WorldState

-- | Carriage with no passenger
emptyCarriage :: Blocks
emptyCarriage = carriage

-- | Set of obstructions after zeroth passenger step (i.e. passenger just embarked)
testCarriage :: Blocks
testCarriage = emptyCarriage { people = Set.insert (Node 1 4) $ people emptyCarriage }

testCarriage' :: Blocks
testCarriage' = emptyCarriage { people = Set.insert (Node 1 3) $ people emptyCarriage }

testCarriage'' :: Blocks
testCarriage'' = emptyCarriage { people = Set.insert (Node 1 2) $ people emptyCarriage }

-- | Initial passenger
passenger :: Queue
passenger = singleton newPassenger { onboard = True }

-- | Passengers after first step (starting from (1,4))
passenger' :: Queue
passenger' = singleton $ newPassenger { location = Node 1 3, onboard = True }

passenger'' :: Queue
passenger'' = singleton $ newPassenger { location = Node 1 2, onboard = True }

-- | Passenger not yet onboard plane
offboard :: Queue
offboard = singleton newPassenger

-- | Multiple passengers
passengers :: Queue
passengers = queueFromList $ take 2 [makePassenger x y | x <- ['A'..'E'], y <- [1..20]]

passengers' :: Queue
passengers' = f . viewl $ passengers
    where
        f (p :< ps) = ps |> p { onboard = True }

-- | Test cases for WorldState
tests :: Test
tests = test [
                -- Single passenger, already onboard
                TestCase (assertEqual "No step" (Just (testCarriage, passenger)) (runStateT (return testCarriage) passenger)),
                TestCase (assertEqual "1st step" (Just (testCarriage', passenger')) (runStateT (step testCarriage) passenger)),
                TestCase (assertEqual "2nd step" (Just (testCarriage'', passenger'')) (runStateT (step testCarriage >>= step) passenger)),
                TestCase (assertEqual "2nd step" (Just (testCarriage'', passenger'')) (runStateT (nSteps 2 $ return testCarriage) passenger)),
                -- Single passenger, not yet onboard
                TestCase (assertEqual "Onboarding 0th" (Just (testCarriage, passenger)) (runStateT (step emptyCarriage) offboard)),
                TestCase (assertEqual "Onboarding 1st" (Just (testCarriage', passenger')) (runStateT (nSteps 2 $ return emptyCarriage) offboard)),
                -- Multiple passengers, not yet onboard
                TestCase (assertEqual "Multiple, no step" (Just (emptyCarriage, passengers)) (runStateT (return emptyCarriage) passengers)),
                TestCase (assertEqual "Multiple, 1st step" (Just (testCarriage, passengers')) (runStateT (step emptyCarriage) passengers)) ]
                -- TODO work out correct result for this
                --TestCase (assertEqual "Multiple, 2nd step" (Just (testCarriage, passengers')) (runStateT (nSteps 4 $ return emptyCarriage) passengers)) ]

