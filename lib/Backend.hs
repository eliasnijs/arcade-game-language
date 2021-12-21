module Backend where

import Config
import Data.List
import Data.Map as Map
import Data.Maybe
import Data.Tuple
import Engine
import EvaluatorImplementation
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Renderer
import System.Random (StdGen, getStdGen, randomR)
import Types

initialise :: Maybe State -> IO ()
initialise s = do
  case s of
    Nothing -> putStrLn "failed to evaluate!"
    Just state -> do
      playIO
        (InWindow "PRIMEVAL GAMES" (500, 900) (10, 10))
        backgroundColor
        2
        (Playing state)
        renderframe
        move
        updatestate

move :: Event -> Scene -> IO Scene
move (EventKey (SpecialKey KeyLeft) Down _ _) (Playing state) =
  Playing <$> (snd <$> getProcFromStack "&key_left" [] state)
move (EventKey (SpecialKey KeyRight) Down _ _) (Playing state) =
  Playing <$> (snd <$> getProcFromStack "&key_right" [] state)
move (EventKey (SpecialKey KeyUp) Down _ _) (Playing state) =
  Playing <$> (snd <$> getProcFromStack "&key_up" [] state)
move (EventKey (SpecialKey KeyDown) Down _ _) (Playing state) =
  Playing <$> (snd <$> getProcFromStack "&key_down" [] state)
move (EventKey (SpecialKey KeySpace) Down _ _) (Playing state) =
  Playing <$> (snd <$> getProcFromStack "&key_space" [] state)
move _ scene = do
  return scene

updatestate :: Float -> Scene -> IO Scene
updatestate _ (Playing state) = do
  Playing <$> (snd <$> getProcFromStack "&update" [] state)
updatestate _ game = do
  return game

renderframe :: Scene -> IO Picture
renderframe (Playing s) = do 
  fpxl <- do
    fpos <- fst <$> getVarFromStack "&friendlies" s
    case fpos of 
      StateList fpos' -> return $ pictures $ drawPixel friendlyColor <$> fpos'
      _ -> undefined
  epxl <- do
    epos <- fst <$> getVarFromStack "&enemies" s
    case epos of 
      StateList epos' -> return $ pictures $ drawPixel enemyColor <$> epos'
      _ -> undefined
  (px,py) <- (,) <$> (fst <$> getVarFromStack "&px" s) <*> (fst <$> getVarFromStack "&py" s)
  return $ pictures [drawfield, epxl, fpxl, drawPixel playerColor (StateList [px, py])]
renderframe game = do
  return drawfield

getProcFromStack :: String -> [SekellExpr] -> State -> IO (StateValue, State)
getProcFromStack k a = evalExpr (CallProc (k, a))

getVarFromStack :: String -> State -> IO (StateValue, State)
getVarFromStack k = evalExpr (CallVar k)
