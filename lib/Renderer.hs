module Renderer where

import Config
import Data.List
import Data.Tuple
import Engine
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Types

gridToviewCoords :: (Int, Int) -> (Float, Float)
gridToviewCoords (x, y) =
  (fromIntegral x * dblock * fscale, fromIntegral y * dblock * fscale)

drawPixel :: Color -> StateValue -> Picture
drawPixel c (StateList p) =
  case (head p, p !! 1) of
    (StateVar p1, StateVar p2) ->
      translate vc1 vc2 (scale (fscale * dwidth) (fscale * dwidth) pixel)
      where (vc1, vc2) = gridToviewCoords (p1, p2)
            pixel =
              Color c $
              pictures
                [ rectangleWire 1 1
                , rectangleSolid (dinner / dwidth) (dinner / dwidth)
                ]
    _ -> pictures []
drawPixel c _ = undefined

drawfield :: Picture
drawfield =
  pictures $
  drawPixel emptyColor <$>
  [ StateList [StateVar x, StateVar y]
  | x <- [left .. right]
  , y <- [bottom .. top]
  ]
