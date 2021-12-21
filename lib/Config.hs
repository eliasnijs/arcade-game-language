module Config where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- configure field
width, height :: Float
width = 10

height = 20

-- configure pixel
dblock, dwidth, dinner, fscale :: Float
dblock = 12

dwidth = 10

dinner = 3

fscale = 3

-- configure colors (color format: r g b a)
backgroundColor, emptyColor, playerColor, friendlyColor, enemyColor :: Color
backgroundColor = makeColor 0.0 0.0 0.0 1.0

emptyColor = makeColor 0.2 0.2 0.2 1.0

playerColor = makeColor 0.0 0.7 0.8 1.0

friendlyColor = makeColor 0.0 0.7 0.8 1.0

enemyColor = makeColor 0.8 0.3 0.0 1.0
