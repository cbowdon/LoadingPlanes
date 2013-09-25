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
import qualified Plane
import WorldState

-- | Carriage with no passenger
emptyCarriage :: Plane.Blocks
emptyCarriage = Plane.carriage

-- | Set of obstructions after zeroth passenger step (i.e. passenger just embarked)
carriage :: Plane.Blocks
carriage = Set.insert (Node 1 4) emptyCarriage

carriage' :: Plane.Blocks
carriage' = Set.insert (Node 1 3) emptyCarriage

carriage'' :: Plane.Blocks
carriage'' = Set.insert (Node 1 2) emptyCarriage

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
                TestCase (assertEqual "No step" (Just (carriage, passenger)) (runStateT (return carriage) passenger)),
                TestCase (assertEqual "1st step" (Just (carriage', passenger')) (runStateT (step carriage) passenger)),
                TestCase (assertEqual "2nd step" (Just (carriage'', passenger'')) (runStateT (step carriage >>= step) passenger)),
                TestCase (assertEqual "2nd step" (Just (carriage'', passenger'')) (runStateT (nSteps 2 $ return carriage) passenger)),
                -- Single passenger, not yet onboard
                TestCase (assertEqual "Onboarding 0th" (Just (carriage, passenger)) (runStateT (step emptyCarriage) offboard)),
                TestCase (assertEqual "Onboarding 1st" (Just (carriage', passenger')) (runStateT (nSteps 2 $ return emptyCarriage) offboard)),
                -- Multiple passengers, not yet onboard
                TestCase (assertEqual "Multiple, no step" (Just (emptyCarriage, passengers)) (runStateT (return emptyCarriage) passengers)),
                TestCase (assertEqual "Multiple, 1st step" (Just (carriage, passengers')) (runStateT (step emptyCarriage) passengers)),
                -- TODO work out correct result for this
                --TestCase (assertEqual "Multiple, 2nd step" (Just (carriage, passengers')) (runStateT (nSteps 4 $ return emptyCarriage) passengers)) ]

