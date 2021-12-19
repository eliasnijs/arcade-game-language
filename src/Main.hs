
-- Sekell language parser (c) Elias Nijs 2021

import Parser
import ParserImplementation
import Evaluator
import EvaluatorImplementation
import Types
import Data.Maybe
import System.Environment (getArgs)

main :: IO ()
main = do
  stmt <- parseFile "tests/simple_test1.skll" sekellScope
  print stmt
  putStrLn ""
  case stmt of
    Nothing -> do {putStrLn "failed to parse! quitting program..."} 
    Just v  -> fst $ evalStmt v []
  -- evaluateStmt stmt 

parseFile :: FilePath -> Parser a -> IO (Maybe a)
parseFile path parser = do
  str <- readFile path
  return (snd <$> parse parser str) 

-- evaluateStmt :: Maybe [SekellStmt] -> Evaluator a -> Maybe (a, Stack)
-- evaluateStmt stmt evaluator =
--   case stmt of
--     Nothing -> Nothing
--     Just t -> do
--       evalStmt evaluator t











