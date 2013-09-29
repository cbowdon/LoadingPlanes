-- | Loading Planes Simulation.
--
-- The great quest to master Haskell/tackle interesting problems continues. I vaguely remember seeing a news article about simulations showing the most efficient way to load passengers onto a plane. As I recall, letting people on at random is least efficient, loading back-to-front somewhat efficient and loading windows-to-aisles most efficient. This project is a recreation of that simulation. The passengers will step in turn towards their seat and time will be the total number of steps required.
module Main where

import Control.Monad.State hiding (get)
import Data.IORef
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Test.HUnit (runTestTT)
import qualified Tests.Path
import qualified Tests.WorldState
import Path
import Passenger
import Plane hiding (clear)
import Visualization
import WorldState

passengers :: Queue
passengers = queueFromList [makePassenger a b | a <- ['A'..'E'], b <- [1]]

initialPlane :: PlaneState Blocks
initialPlane = return carriage

-- * Functions
-- | Run and print tests.
main :: IO ()
main = do
    print "Path:"
    _ <- runTestTT Tests.Path.tests
    print "WorldState:"
    _' <- runTestTT Tests.WorldState.tests
    _ <- getArgsAndInitialize
    _ <- createWindow "Hello, world"
    ps <- newIORef initialPlane
    keyboardMouseCallback $= Just (updatePlane ps)
    displayCallback $= display ps
    mainLoop

userInput :: IORef Int -> KeyboardMouseCallback
userInput n (Char ' ') Down _ _ = do
    n $~! (+1)
    postRedisplay Nothing
userInput _ _ _ _ _ = return ()

updatePlane :: IORef (PlaneState Blocks) -> KeyboardMouseCallback
updatePlane ips (Char ' ') Down _ _ = do
    ips $~! (>>=step)
    postRedisplay Nothing
updatePlane _ _ _ _ _ = return ()

display :: IORef (PlaneState Blocks) -> IO ()
display ips = do
    clear [ColorBuffer]
    render carriage
    ps <- get ips
    case runStateT ps passengers of
        Just (_, ppl)   -> renderMany ppl
        Nothing         -> print "Nothing!"
    flush
