-- | Updating the state of the world
module WorldState
( PlaneState
, Queue
, queueFromList
, step
) where

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
--
-- Usage examples:
--
-- runStateT (step carriage) [newPassenger]
--
-- runStateT (head $ drop 2 $ iterate (>>=step) $ zeroStep carriage) [newPassenger]
step :: Blocks -> PlaneState Blocks
step obs = StateT $ \q ->
    case viewl q of
    (p :< ps)   ->  if seated p
                    then Just (obs,ps)
                    else nextStep obs p >>= Just . update obs ps p
    _           -> Nothing

nextStep :: Blocks -> Passenger -> Maybe Node
nextStep obs p =
    let next = move (location p) (seat p)
    in if clear obs next
    then Just next
    else Nothing

-- TODO change y logic
move :: Node -> Node -> Node
move (Node x y) (Node x' y') = Node (inc x x') (inc y y')
    where
        inc k k'
            | k < k'    = k + 1
            | k > k'    = k - 1
            | otherwise = k

clear :: Blocks -> Node -> Bool
clear = flip Set.notMember

update :: Blocks -> Queue -> Passenger -> Node -> (Blocks, Queue)
update obs ps p next =
    let p' = ps |> p { location = next }
        o' = Set.insert next . Set.delete (location p) $ obs
    in  (o', p')
