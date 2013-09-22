-- | Updating the state of the world
module WorldState
( PlaneState
, step
, step'
) where

import Control.Monad.State
import qualified Data.Set as Set
import Path
import Passenger
import Plane

-- TODO efficient queue impl
type Queue = [Passenger]

type PlaneState a = StateT Queue Maybe a

-- | Take 1 step (for 1 passenger)
--
-- (1) If not seated, find next step for p.
--
-- (2) If next step is clear, put updated p to back of queue.
--
-- (3) Remove p's old node from obs and add new node.
--
-- (4) If no step possible, nothing.
step :: Blocks -> Queue -> Maybe (Blocks, Queue)
step obs (p:ps) =
    if seated p
    then Just (obs,ps)
    else nextStep obs p >>= Just . update obs ps p
step _ _ = Nothing

step' :: Blocks -> PlaneState Blocks
step' obs = StateT $ \q ->
    case q of
    []      -> Nothing
    (p:ps)  ->  if seated p
                then Just (obs,ps)
                else nextStep obs p >>= Just . update obs ps p

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
    let p' = ps ++ [p { location = next }]
        o' = Set.delete (seat p) . Set.insert next $ obs
    in  (o', p')
