-- | Prepare to render an item with OpenGL
module Visualization
( -- * Types
Vis(..)
-- * Functions
, rgbColor
, render
, renderMany
, worldSize
, unitRect
, toGLfloat
, scale2d
) where

import Prelude hiding (mapM_)
import Data.Foldable (Foldable, mapM_)
import Graphics.Rendering.OpenGL

-- | A visualizable (with OpenGL) type
class Vis a where
    visualize :: a -> IO ()
    -- TODO
    -- rgb :: a -> Color3
    -- shape :: a -> [Vertex2]

-- | Set visualization color
rgbColor :: GLfloat -> GLfloat -> GLfloat -> IO ()
rgbColor r g b = color $ Color3 r g b

vertex2d :: GLfloat -> GLfloat -> IO ()
vertex2d x y = vertex $ Vertex2 x y

-- | Render visualizable as quads
render :: Vis a => a -> IO ()
render a = renderPrimitive Quads $ visualize a

-- | Render visualizable sequence as quads
renderMany :: (Vis a, Foldable b) => b a -> IO ()
renderMany = renderPrimitive Quads . mapM_ visualize

-- | Size of the simulation world
worldSize :: (GLfloat, GLfloat)
worldSize = (42.0, 8.0)

tileSize :: (GLfloat, GLfloat)
tileSize = (1/x,1/y)
    where
        (x,y) = worldSize

-- | Utility to convert integrals to GLfloats
toGLfloat :: Integral a => a -> GLfloat
toGLfloat a = fromIntegral a :: GLfloat

-- | Visualize a 1x1 rectangle
unitRect :: (GLfloat,GLfloat) -> IO ()
unitRect (x,y) = do
        let (x',y') = tileSize
        vertex2d (x+0.1*x') (y+0.1*y')
        vertex2d (x+0.1*x') (y+0.9*y')
        vertex2d (x+0.9*x') (y+0.9*y')
        vertex2d (x+0.9*x') (y+0.1*y')

translateOrigin :: (GLfloat, GLfloat) -> (GLfloat, GLfloat)
translateOrigin (x,y) = (x-0.5,y-0.5)

-- | Scale points (x,y) in worldSize to points in (-1 to 1)
scale2d :: Integral a => (a,a) -> (GLfloat,GLfloat)
scale2d (x,y) = translateOrigin (x',y')
    where
        (m,n)       = worldSize
        x'          = toGLfloat x / m
        y'          = toGLfloat y / n
