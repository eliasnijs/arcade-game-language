import EvaluatorImplementation
import Keywords
import Parser
import ParserImplementation
import Types
import Engine
import Backend (initialise)
import Data.Maybe
import System.Environment (getArgs)
import Data.Map as Map
import System.Random (StdGen, getStdGen, randomR)

main :: IO ()
main = do
  args <- getArgs 
  case args of
    [] -> putStrLn "no input files!"
    _ -> do
      stmt <- parseFile (head args) sekellFileScope
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
      stdg <- getStdGen
      Just <$> evalStmt v (getStdVarlib, getStdProclib, stdg)
