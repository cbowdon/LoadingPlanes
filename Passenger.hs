-- | Describes the types
module Passenger
( -- * Types
Passenger(..)
, Queue
-- * Functions
, seated
, newPassenger
, makePassenger
, queueFromList
) where

import Data.Sequence (Seq, fromList)
import Path
import Plane
import Visualization

-- | Representation of a passenger
data Passenger = Passenger {
    onboard :: Bool,
    location :: Node,
    seat :: Node
} deriving (Show, Eq)

-- | Passenger processing queue (i.e. whose turn to step)
type Queue = Seq Passenger

-- | Default uninitialized passenger
newPassenger :: Passenger
newPassenger = Passenger False start (seatRef 'A' 1)

-- | Make a new passenger for given seat
makePassenger :: Char -> Int -> Passenger
makePassenger c n = newPassenger { seat = seatRef c n }

-- | Convenient constructor function for queues
queueFromList :: [Passenger] -> Queue
queueFromList = fromList

-- | Is the passenger in his seat?
seated :: Passenger -> Bool
seated p = location p == seat p

instance Vis Passenger where
    visualize p = do
        if seated p then rgbColor 0.0 1.0 0.0 else rgbColor 1.0 0.0 0.0
        let Node x y = location p
        unitRect $ scale2d (x,y)
