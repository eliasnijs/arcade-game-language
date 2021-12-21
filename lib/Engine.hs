module Engine where

import Data.List
import Data.Tuple
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random (StdGen, getStdGen, randomR)
import Types

import Config

bottom, top, left, right :: Int
bottom = floor $ -height / 2

top = floor $ (height + 1) / 2

left = floor $ -width / 2

right = floor $ (width + 1) / 2

type Vec2 = (Int, Int)

data Scene 
  = Playing State
  | Endscreen

----- Gloss ------------------------------------------
onBoard, atBottom, atTop, atLeft, atRight :: Vec2 -> Bool
onBoard (x, y) = x >= left && x <= right && y >= bottom && y <= top

atBottom (x, y) = y == bottom

atTop (x, y) = y == top

atLeft (x, y) = x == left 

atRight (x, y) = x == right

collide :: [Vec2] -> [Vec2] -> ([Vec2], [Vec2])
collide c1 c2 = (c1 \\ c2, c2 \\ c1)

decBound, incBound :: (Ord a, Num a) => a -> a -> a
decBound x b = max b (x - 1)

incBound x b = min b (x + 1)

----- LIST ------------------------------------------
getl :: StateValue -> Int -> StateValue 
getl (StateList a) i = a !! i 
getl a _ = error "invalid argument"

addl :: StateValue -> StateValue -> StateValue 
addl (StateList a) v = StateList $ a ++ [v]
addl a _ = error "invalid argument"

insertl :: StateValue -> Int -> StateValue -> StateValue 
insertl (StateList a) i v = StateList $ p1 ++ [v] ++ p2
  where (p1,p2) = splitAt i a
insertl a _ _ = error "invalid argument!"

removel :: StateValue -> Int -> StateValue 
removel (StateList a) i = StateList $ p1 ++ tail p2
  where (p1,p2) = splitAt i a
removel a _ = error "invalid argument!"

setl :: StateValue -> Int -> StateValue -> StateValue
setl (StateList a) i v = StateList $ p1 ++ [v] ++ tail p2
  where (p1,p2) = splitAt i a
setl a _ _ = error "invalid argument!"
