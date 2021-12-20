module EvaluatorImplementation where

import Data.Maybe
import Data.Map as Map
import Control.Monad
import Evaluator
import Types
import qualified Control.Arrow as Map

----- EXPR ------------------------------------------
evalExpr :: SekellExpr -> State -> IO (Int, State)

evalExpr (TpInt i) s = 
  do return (i, s)

evalExpr (OpPLUS (e1, e2)) s = evalBinOpExpr e1 e2 s (+)

evalExpr (OpMIN (e1, e2)) s = evalBinOpExpr e1 e2 s (-)

evalExpr (OpMULT (e1, e2)) s = evalBinOpExpr e1 e1 s (*)

evalExpr (OpDIV (e1, e2)) s = evalBinOpExpr e1 e1 s div

evalExpr (CmpGT (e1, e2)) s = evalBinOpExpr e1 e1 s (\x y -> fromEnum (x > y))

evalExpr (CmpLT (e1, e2)) s = evalBinOpExpr e1 e1 s (\x y -> fromEnum (x < y))

evalExpr (CmpGE (e1, e2)) s = evalBinOpExpr e1 e1 s (\x y -> fromEnum (x >= y))

evalExpr (CmpLE (e1, e2)) s = evalBinOpExpr e1 e1 s (\x y -> fromEnum (x <= y))

evalExpr (CmpEQ (e1, e2)) s = evalBinOpExpr e1 e1 s (\x y -> fromEnum (x == y))

evalExpr (CmpAnd (e1, e2)) s = evalBinOpExpr e1 e1 s (\x y -> fromEnum (fromEnum x == 1 && fromEnum y == 1 ))

evalExpr (CmpOr (e1, e2)) s =  evalBinOpExpr e1 e1 s (\x y -> fromEnum (fromEnum x == 1 || fromEnum y == 1 ))

evalExpr (CallVar k) (e,p) = do 
  let i' = fromMaybe undefined (Map.lookup k e)
  return (i',(e,p)) 

evalExpr (CallProc (k, a)) (e,p) = do
  st' <- evalStmt (fromMaybe undefined (Map.lookup k p)) (e,p)
  return (0, st') 
                       
evalExpr _ _ = undefined

evalBinOpExpr :: SekellExpr -> SekellExpr -> State -> (Int -> Int -> Int) -> IO (Int, State)
evalBinOpExpr e1 e2 s f = 
  do (n1,st') <- evalExpr e1 s
     (n2,st'') <- evalExpr e2 st'
     return (f n1 n2, st'')

----- STMT ------------------------------------------
evalStmt :: SekellStmt -> State -> IO State

evalStmt (StmtScope x) s = do runEval x s
  where runEval :: [SekellStmt] -> State -> IO State
        runEval [] st = do return st
        runEval [x] st = do evalStmt x st
        runEval (x:xs) st = do
          st' <- evalStmt x st
          runEval xs st' 

evalStmt (StmtPrint x) s = do 
  (str,st') <- evalToString x s
  putStrLn str
  return st'
    where evalToString (TpString str) s = do return (str, s)
          evalToString a s = do (i,st') <- evalExpr a s
                                return (show i, st')


evalStmt (StmtIf (r, stmt)) s = do 
  (t, st') <- evalExpr r s
  case t of 
    0 -> do {return st'}
    _ -> do {evalStmt stmt st'}

evalStmt (StmtWhile (r, stmt)) s = do 
  (t, st') <- evalExpr r s
  case t of 
    0 -> do {return s}
    _ -> do st'' <- evalStmt stmt st'
            evalStmt (StmtWhile (r, stmt)) st''

evalStmt (StmtProc (k, args, stmt)) (e,p) = do 
  return (e, insert k stmt p)

evalStmt (StmtAssignVar (k, v)) (e,p) = do 
  (i,(e',p')) <- evalExpr v (e,p)
  return (insert k i e', p')

evalStmt (StmtDoExpr e) s = do
  snd <$> evalExpr e s

evalStmt _ _ = undefined






