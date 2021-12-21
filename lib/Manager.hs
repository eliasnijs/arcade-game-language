module Manager where

import Data.List
import Data.Tuple
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random (StdGen, getStdGen, randomR)

import Config
import Engine
import Renderer

scene :: Int -> Int -> StdGen -> Scene
scene = undefined

move :: Event -> Scene -> Scene
move e s = undefined

update :: Float -> Scene -> Scene
update fl scene = undefined

initialise :: IO ()
initialise = do
  stdGen <- getStdGen
  play
    (InWindow "PRIMEVAL GAMES" (500, 900) (10, 10))
    backgroundColor
    2
    (head $ scenes stdGen)
    renderframe
    move
    update
