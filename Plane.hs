-- | Describes the plane
module Plane
( carriage
, Blocks(..)
) where

import qualified Data.Set as Set
import Path
import Visualization

newtype Blocks = Blocks { blocks :: Set.Set Node }
-- TODO types organization

instance Vis Blocks where
    visualize = mapM_ visualize . Set.toList . blocks

instance Vis Node where
    visualize (Node x y) = do
        rgbColor 1.0 1.0 1.0
        unitRect $ scale2d (x,y)

-- | The nodes occupied by parts of the plane
carriage :: Blocks
carriage = Blocks $ Set.unions $ map blocks [walls, seats]

walls :: Blocks
walls = Blocks $ Set.fromList [Node x y | x <- [0,7], y <- [0..40]]

seats :: Blocks
seats = Blocks $ Set.fromList [Node x y | x <- [0..20], y <- [1,2,3,5,6]]
