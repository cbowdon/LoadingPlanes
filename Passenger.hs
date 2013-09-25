-- | Describes the types
module Passenger
( -- * Types
Passenger(..)
, seated
, newPassenger
, makePassenger
) where

import Path
import Plane
import Visualization

-- | Representation of a passenger
data Passenger = Passenger {
    onboard :: Bool,
    location :: Node,
    seat :: Node
} deriving (Show, Eq)

-- | Default uninitialized passenger
newPassenger :: Passenger
newPassenger = Passenger False start (seatRef 'A' 1)

-- | Make a new passenger for given seat
makePassenger :: Char -> Int -> Passenger
makePassenger c n = newPassenger { seat = seatRef c n }

-- | Is the passenger in his seat?
seated :: Passenger -> Bool
seated p = location p == seat p

instance Vis Passenger where
    visualize p = do
        if seated p then rgbColor 0.0 1.0 0.0 else rgbColor 1.0 0.0 0.0
        let Node x y = location p
        unitRect $ scale2d (x,y)
