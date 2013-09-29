-- | Describes the plane
module Plane
( -- * Constants
carriage
, start
-- * Types
, Blocks(..)
-- * Functions
, seatRef
, clear
) where

import qualified Data.Set as Set
import Path
import Visualization

-- | Set of nodes occupied by parts of the plane
data Blocks = Blocks {
    seats :: Set.Set Node,
    walls :: Set.Set Node,
    people :: Set.Set Node
} deriving (Eq, Show)

instance Vis Blocks where
    visualize b = do
        rgbColor 0.5 0.0 1.0
        mapM_ visualize . Set.toList . walls $ b
        rgbColor 0.5 0.5 0.0
        mapM_ visualize . Set.toList . seats $ b
        -- don't visualize people - passengers will be visualized separately

instance Vis Node where
    visualize (Node x y) = unitRect $ scale2d (x,y)

-- | Default starting node
start :: Node
start = Node 1 4

-- | Is the node clear?
clear :: Blocks -> Node -> Bool
clear b n = all (Set.notMember n) [people b, seats b, walls b]

-- | Convert a seat reference to a node
seatRef :: Char -> Int -> Node
seatRef c n = Node (n * 2 - 1) y
    where
        y = case c of
            'A' -> 1
            'B' -> 2
            'C' -> 3
            'D' -> 5
            _   -> 6

-- | The nodes occupied by parts of the plane
carriage :: Blocks
carriage = Blocks {
    walls = Set.fromList $ [Node x y | x <- [0..41], y <- [0,7]] ++ [Node x y | x <- [0,41], y <- [0..7]],
    seats = Set.fromList [Node (2*x) y | x <- [1..20], y <- [1,2,3,5,6]],
    people = Set.empty
}
