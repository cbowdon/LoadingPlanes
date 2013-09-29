-- | Updating the state of the world
module WorldState
( PlaneState
, Queue
, queueFromList
, step
, nSteps
) where

import Control.Monad
import Control.Monad.State
import qualified Data.Set as Set
import Data.Sequence (Seq, ViewL(..), (|>), viewl, fromList)
import Path
import Passenger
import Plane

-- | Passenger processing queue (i.e. whose turn to step)
type Queue = Seq Passenger

-- | Describes the state of the plane (passenger locations)
type PlaneState a = StateT Queue Maybe a

-- | Convenient constructor function for queues
queueFromList :: [Passenger] -> Queue
queueFromList = fromList

-- | Take 1 step (for 1 passenger)
--
-- (1) If not seated, find next step for p.
--
-- (2) If next step is clear, put updated p to back of queue.
--
-- (3) Remove p's old node from obs and add new node.
--
-- (4) If no step possible, nothing: as soon as one person is unable to step, gridlock!
step :: Blocks -> PlaneState Blocks
step obs = StateT $ f . viewl
    where
        f (p :< ps)
            | seated p = return (obs, ps)
            | not (onboard p) && clear obs (location p) = return $ insert obs ps p
            | not (onboard p) = return (obs, ps |> p)
            | otherwise = liftM (update obs ps p) (nextStep obs p)
        f _ = Nothing

-- | Apply n steps to state s
nSteps :: Int -> PlaneState Blocks -> PlaneState Blocks
nSteps n s = iterate (>>=step) s !! n

nextStep :: Blocks -> Passenger -> Maybe Node
nextStep obs p =
    let next = findNext (location p) (seat p)
    in if clear obs next
    then Just next
    else Nothing

findNext :: Node -> Node -> Node
findNext (Node x y) (Node x' y') = Node (inc x x') (inc y y')
    where
        inc k k'
            | k < k'    = k + 1
            | k > k'    = k - 1
            | otherwise = k

update :: Blocks -> Queue -> Passenger -> Node -> (Blocks, Queue)
update obs ps p next =
    let p' = ps |> p { location = next }
        o' = obs { people = Set.insert next . Set.delete (location p) $ people obs }
    in  (o', p')

insert :: Blocks -> Queue -> Passenger -> (Blocks, Queue)
insert obs ps p =
    let p' = ps |> p { onboard = True }
        o' = obs { people = Set.insert (location p) $ people obs }
    in (o', p')
