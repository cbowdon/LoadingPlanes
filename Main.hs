-- | Loading Planes Simulation.
--
-- The great quest to master Haskell/tackle interesting problems continues. I vaguely remember seeing a news article about simulations showing the most efficient way to load passengers onto a plane. As I recall, letting people on at random is least efficient, loading back-to-front somewhat efficient and loading windows-to-aisles most efficient. This project is a recreation of that simulation. The passengers will step in turn towards their seat and time will be the total number of steps required.
module Main where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Test.HUnit (runTestTT)
import Tests.Path
import Path
import Passenger
import Plane
import Visualization

-- * Functions
-- | Run and print tests.
main :: IO ()
main = do
    c <- runTestTT tests
    print c
    _ <- getArgsAndInitialize
    _ <- createWindow "Hello, world"
    displayCallback $= display
    mainLoop

display :: IO ()
display = do
    clear [ColorBuffer]
    render walls
    render seats
    flush
