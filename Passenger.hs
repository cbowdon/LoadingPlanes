-- | Describes the types
module Passenger
( -- * Types
Passenger(..)
) where

import Path
import Visualization

-- | Representation of a passenger
data Passenger = Passenger {
    seated :: Bool,
    location :: Node,
    seat :: Node
}

instance Vis Passenger where
    visualize p = do
        if seated p then rgbColor 0.0 1.0 0.0 else rgbColor 1.0 0.0 0.0
        let Node x y = location p
        unitRect $ scale2d (x,y)
