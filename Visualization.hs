module Visualization
( Vis(..)
, vertex2d
, rgbColor
, render
, worldSize
, unitRect
, toGLfloat
, scale2d
) where

import Graphics.Rendering.OpenGL

class Vis a where
    visualize :: a -> IO ()

rgbColor :: GLfloat -> GLfloat -> GLfloat -> IO ()
rgbColor r g b = color $ Color3 r g b

vertex2d :: GLfloat -> GLfloat -> IO ()
vertex2d x y = vertex $ Vertex3 x y 0

render :: Vis a => a -> IO ()
render a = renderPrimitive Quads $ visualize a

worldSize :: (GLfloat, GLfloat)
worldSize = (40.0, 7.0)

tileSize :: (GLfloat, GLfloat)
tileSize = (1/x,1/y)
    where
        (x,y) = worldSize

toGLfloat :: Integral a => a -> GLfloat
toGLfloat a = fromIntegral a :: GLfloat

unitRect :: (GLfloat,GLfloat) -> IO ()
unitRect (x,y) = do
        let (x',y') = tileSize
        vertex2d x y
        vertex2d x (y+y')
        vertex2d (x+x') (y+y')
        vertex2d (x+x') y

scale2d :: Integral a => (a,a) -> (GLfloat,GLfloat)
scale2d (x,y) = (x',y')
    where
        (m,n)       = worldSize
        x'          = toGLfloat x / m
        y'          = toGLfloat y / n
