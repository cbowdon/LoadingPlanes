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
import Passenger
import Plane hiding (clear)
import Visualization
import WorldState

-- | Passengers, in order of boarding plane
passengers :: Queue
passengers = queueFromList [makePassenger a b | a <- ['A'..'E'], b <- [1..20]]

-- | Empty plane
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
    _ <- createWindow "Loading Planes (space to step)"
    ps <- newIORef initialPlane -- I know this defeats the point of the State monad. Blame OpenGL.
    n <- newIORef 0
    keyboardMouseCallback $= Just (updatePlane n ps)
    displayCallback $= display n ps
    mainLoop

-- | Callback to update plane state
updatePlane :: IORef Int -> IORef (PlaneState Blocks) -> KeyboardMouseCallback
updatePlane n ps (Char ' ') Down _ _ = do
    n $~! (+1)
    ps $~! (>>=step)
    postRedisplay Nothing
updatePlane _ _ _ _ _ _ = return ()

-- | Callback to display plane state
display :: IORef Int -> IORef (PlaneState Blocks) -> IO ()
display n ps = do
    clear [ColorBuffer]
    render carriage
    ps' <- get ps
    n' <- get n
    case runStateT ps' passengers of
        Just (_, ppl)   -> print n' >> renderMany ppl
        Nothing         -> print "Nothing!"
    flush
