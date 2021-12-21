-- Sekell language parser (c) Elias Nijs 2021
import Data.Maybe
import EvaluatorImplementation
import Keywords
import Parser
import ParserImplementation
import Types
import System.Environment (getArgs)
import Data.Map as Map
import Backend (initialise)

main :: IO ()
main = do
  args <- do return ["games/test.skll"] -- TODO: Change to getArgs when everything is done
  case args of
    [] -> putStrLn "no input files!"
    _ -> do
      stmt <- parseFile (head args) sekellScope
      print stmt
      state <- interpret stmt
      initialise state
      return ()

parseFile :: FilePath -> Parser a -> IO (Maybe a)
parseFile path parser = do
  str <- readFile path
  return (snd <$> parse parser str)

interpret :: Maybe SekellStmt -> IO (Maybe State)
interpret s =
  case s of
    Nothing -> do
      putStrLn "failed to parse!"
      return Nothing 
    Just v -> do
      Just <$> evalStmt v (Map.empty, Map.empty)
