module Engine where

import Data.List as List
import Data.Tuple
import Data.Map as Map
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
onBoard, atBottom, atTop, atLeft, atRight ::
     StateValue -> StateValue -> StateValue
onBoard (StateVar x) (StateVar y) = StateVar $ fromEnum $ x >= left && x <= right && y >= bottom && y <= top
onBoard _ _ = error "invalid argument!"

atBottom (StateVar x) (StateVar y) = StateVar $ fromEnum $ y == bottom
atBottom _ _ = error "invalid argument!"

atTop (StateVar x) (StateVar y) = StateVar $ fromEnum $ y == top
atTop _ _ = error "invalid argument!"

atLeft (StateVar x) (StateVar y) = StateVar $ fromEnum $ x == left
atLeft _ _ = error "invalid argument!"

atRight (StateVar x) (StateVar y) = StateVar $ fromEnum $ x == right
atRight _ _ = error "invalid argument!"

collide :: StateValue -> StateValue -> StateValue
collide (StateList x) (StateList b) = StateList $ x List.\\ b
collide _ _ = error "invalid argument!"

decBound, incBound :: StateValue -> StateValue -> StateValue
decBound (StateVar x) (StateVar b) = StateVar $ max b (x - 1)
decBound _ _ = error "invalid argument!"

incBound (StateVar x) (StateVar b) = StateVar $ min b (x + 1)
incBound _ _ = error "invalid argument!"

----- LIST ------------------------------------------
sizel :: StateValue -> StateValue
sizel (StateList a) = StateVar $ length a
sizel _ = error "invalid argument!"

getl :: StateValue -> StateValue -> StateValue
getl (StateList a) (StateVar i) = a !! i
getl a _ = error "invalid argument!"

addl :: StateValue -> StateValue -> StateValue
addl (StateList a) v = StateList $ a ++ [v]
addl a _ = error "invalid argument!"

insertl :: StateValue -> StateValue -> StateValue -> StateValue
insertl (StateList a) (StateVar i) v = StateList $ p1 ++ [v] ++ p2
  where (p1, p2) = List.splitAt i a
insertl a _ _ = error "invalid argument!"

removel :: StateValue -> StateValue -> StateValue
removel (StateList a) (StateVar i) = StateList $ p1 ++ tail p2
  where
    (p1, p2) = List.splitAt i a
removel a _ = error "invalid argument!"

setl :: StateValue -> StateValue -> StateValue -> StateValue
setl (StateList a) (StateVar i) v = StateList $ p1 ++ [v] ++ tail p2
  where (p1, p2) = List.splitAt i a
setl a _ _ = error "invalid argument!"

----- UNION ------------------------------------------
getStdlib :: Map Identifier StateProc
getStdlib = fromList [ ("&getl", StateStdProc $ AC3 getl)
                     , ("&removel", StateStdProc $ AC3 removel)
                     , ("&addl", StateStdProc $ AC3 addl)
                     , ("&insertl", StateStdProc $ AC4 insertl)
                     , ("&setl", StateStdProc $ AC4 setl)
                     , ("&onBoard", StateStdProc $ AC3 onBoard)
                     , ("&atTop", StateStdProc $ AC3 atTop)
                     , ("&atBottom", StateStdProc $ AC3 atBottom)
                     , ("&atRight", StateStdProc $ AC3 atRight)
                     , ("&atLeft", StateStdProc $ AC3 atLeft)
                     , ("&decBound", StateStdProc $ AC3 decBound)
                     , ("&incBound", StateStdProc $ AC3 incBound)
                     , ("&collide", StateStdProc $ AC3 collide)
                   ]
