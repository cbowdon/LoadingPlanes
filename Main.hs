-- | Loading Planes Simulation.
--
-- The great quest to master Haskell/tackle interesting problems continues. I vaguely remember seeing a news article about simulations showing the most efficient way to load passengers onto a plane. As I recall, letting people on at random is least efficient, loading back-to-front somewhat efficient and loading windows-to-aisles most efficient. This project is a recreation of that simulation. The passengers will step in turn towards their seat and time will be the total number of steps required.
module Main where

import Prelude hiding (mapM_)
import Control.Monad.State hiding (mapM_)
import Data.Foldable (mapM_)
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
    displayCallback $= display
    mainLoop

display :: IO ()
display = do
    clear [ColorBuffer]
    render carriage
    let k = runStateT (nSteps 3 $ step carriage) $ queueFromList [newPassenger]
    case k of
        Just (_, ppl)   -> renderPrimitive Quads $ mapM_ visualize ppl
        Nothing         -> print "Nothing"
    flush
