-- | Extremely simple path-finding
module Path
( -- * Types
  Node(..)
-- * Functions
, moveXThenY
) where

import Visualization

-- * Types
-- | An (x,y) point that can be occupied.
data Node = Node Int Int deriving (Eq, Ord, Show)

instance Vis Node where
    visualize (Node x y) = unitRect $ scale2d (x,y)

-- | Advance 1 step towards the target in an L-shaped path (X first, then Y)
moveXThenY ::   Node -- ^ Current location
            ->  Node -- ^ Target location
            ->  Node -- ^ Calculated next step
moveXThenY (Node x y) (Node x' y')
    | x < x'    = Node (x + 1) y
    | y < y'    = Node x (y + 1)
    | y > y'    = Node x (y - 1)
    | otherwise = Node x y
