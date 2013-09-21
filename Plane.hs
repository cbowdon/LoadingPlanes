-- | Describes the plane
module Plane
( -- * Constants
carriage
, walls
, seats
, start
-- * Types
, Blocks(..)
, Seats(..)
, Walls(..)
-- * Functions
, findSeat
) where

import Data.Char
import qualified Data.Set as Set
import Path
import Visualization

-- | Set of nodes occupied by parts of the plane
newtype Blocks = Blocks { blocks :: Set.Set Node }
-- TODO types organization

-- | Set of nodes occupied by the seats themselves
newtype Seats = Seats { getSeats :: Set.Set Node }

-- | Set of nodes occupied by the walls of the plane
newtype Walls = Walls { getWalls :: Set.Set Node }

instance Vis Walls where
    visualize w = do
        rgbColor 0.5 0.0 1.0
        mapM_ visualize . Set.toList . getWalls $ w

instance Vis Seats where
    visualize s = do
        rgbColor 0.5 0.5 0.0
        mapM_ visualize . Set.toList . getSeats $ s

instance Vis Node where
    visualize (Node x y) = unitRect $ scale2d (x,y)

start :: Node
start = Node 1 4

findSeat :: Char -> Int -> Node
findSeat c n = Node (n * 2 - 1) y
    where
        y = case c of
            'A' -> 1
            'B' -> 2
            'C' -> 3
            'D' -> 5
            _   -> 6

-- | The nodes occupied by parts of the plane
carriage :: Blocks
carriage = Blocks $ Set.unions [getSeats seats, getWalls walls]

-- | Walls of the plane
walls :: Walls
walls = Walls $ Set.fromList $ [Node x y | x <- [0..41], y <- [0,7]] ++ [Node x y | x <- [0,41], y <- [0..7]]

-- | Seats
seats :: Seats
seats = Seats $ Set.fromList [Node (2*x) y | x <- [1..20], y <- [1,2,3,5,6]]
