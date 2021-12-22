module EvaluatorImplementation where

import Data.Maybe
import Data.Map as Map
import Control.Monad
import Types
import Engine
import qualified Control.Arrow as Map

----- EXPR ------------------------------------------
evalExpr :: SekellExpr -> State -> IO (StateValue, State)

evalExpr (TpInt i) s = 
  do return (StateVar i, s)

evalExpr (TpList i) s = do runEval i s
  where runEval :: [SekellExpr] -> State -> IO (StateValue, State)
        runEval [] st = do return (StateList [], st)
        runEval (x:xs) st = do
          (i', st') <- runEval xs st
          (v, st'') <- evalExpr x st'
          case (v, i') of 
            (StateList v', StateList i') -> return (StateList (StateList v': i'), st'')
            (StateVar v',  StateList i') -> return (StateList (v: i'), st'')
            (_, _)  -> return (StateNULL, st'')

evalExpr (OpPLUS (e1, e2)) s = evalBinOpExpr e1 e2 s (+)

evalExpr (OpMIN (e1, e2)) s = evalBinOpExpr e1 e2 s (-)

evalExpr (OpMULT (e1, e2)) s = evalBinOpExpr e1 e2 s (*)

evalExpr (OpDIV (e1, e2)) s = evalBinOpExpr e1 e2 s div

evalExpr (CmpGT (e1, e2)) s = evalBinOpExpr e1 e2 s (\x y -> fromEnum (x > y))

evalExpr (CmpLT (e1, e2)) s = evalBinOpExpr e1 e2 s (\x y -> fromEnum (x < y))

evalExpr (CmpGE (e1, e2)) s = evalBinOpExpr e1 e2 s (\x y -> fromEnum (x >= y))

evalExpr (CmpLE (e1, e2)) s = evalBinOpExpr e1 e2 s (\x y -> fromEnum (x <= y))

evalExpr (CmpEQ (e1, e2)) s = evalBinOpExpr e1 e2 s (\x y -> fromEnum (x == y))

evalExpr (CmpAnd (e1, e2)) s = evalBinOpExpr e1 e2 s (\x y -> fromEnum (fromEnum x == 1 && fromEnum y == 1 ))

evalExpr (CmpOr (e1, e2)) s =  evalBinOpExpr e1 e2 s (\x y -> fromEnum (fromEnum x == 1 || fromEnum y == 1 ))

evalExpr (CallVar k) (e,p) = do 
  let i' = fromMaybe StateNULL (Map.lookup k e)
  return (i',(e,p)) 

evalExpr (CallProc (k,a)) (e,p) = do
  let f = fromMaybe StateProcNULL (Map.lookup k p)
  (e',p') <- case f of
    StateNormProc (aid, stmt) -> 
      do argstmt <- do return $ StmtAssignVar <$> zip aid a
         stmt' <- do return $ StmtScope $ (++) argstmt stmt
         evalStmt stmt' (e,p)
    StateStdProc g -> evalStdProc g a (e,p)
    StateProcNULL  -> do print ("no procudure `" ++ k ++ "` was found!")
                         error ""
  let r = fromMaybe StateNULL (Map.lookup returnId e')
  e'' <- do return $ delete returnId e' 
  return (r, (Map.intersection e'' e,p)) 

evalExpr _ s = do
  putStrLn "Error: invalid expression!"
  return (StateNULL ,s);

evalBinOpExpr :: SekellExpr -> SekellExpr -> State -> (Int -> Int -> Int) -> IO (StateValue, State)
evalBinOpExpr e1 e2 s f = 
  do (n1,st') <- evalExpr e1 s
     (n2,st'') <- evalExpr e2 st'
     case (n1, n2) of
       (StateVar n1', StateVar n2') -> return (StateVar (f n1' n2'), st'')
       (_, _) -> return (StateNULL, st'')

evalStdProc :: StdProc -> [SekellExpr] -> State -> IO State
evalStdProc (AC1 f) [] (e,p) = do
  let v = f
  return (insert returnId v e, p)
evalStdProc (AC2 f) [arg1] (e,p) = do
  (a1,(e',p')) <- evalExpr arg1 (e,p)
  let v = f a1
  return (insert returnId v e', p')
evalStdProc (AC3 f) [arg1, arg2] (e,p) = do
  (a1,s) <- evalExpr arg1 (e,p)
  (a2,(e',p')) <- evalExpr arg2 s
  let v = f a1 a2
  return (insert returnId v e', p')
evalStdProc (AC4 f) [arg1, arg2, arg3] (e,p) = do
  (a1,s) <- evalExpr arg1 (e,p)
  (a2,s') <- evalExpr arg2 s
  (a3,(e',p')) <- evalExpr arg3 s'
  let v = f a1 a2 a3 
  return (insert returnId v e, p)
evalStdProc _ _ s = return s
    
----- STMT ------------------------------------------
evalStmt :: SekellStmt -> State -> IO State

evalStmt (StmtScope x) (e,p) = do runEval x (e,p)
  where runEval :: [SekellStmt] -> State -> IO State
        runEval [] st = do return st
        runEval (x:xs) (e',p') = do
          let r = fromMaybe StateNULL (Map.lookup returnId e')
          xs' <- case x of
            StmtReturn x' -> do return []
            _ -> do return xs
          (e''',p''') <- case x of
            StmtDoExpr x' -> do evalStmt x (Map.intersection e' e, p')
            _             -> do (e'',p'') <- evalStmt x (e',p')
                                return (e'',p'')
          runEval xs' (e''',p''')

evalStmt (StmtFileScope x) s = do runEval x s
  where runEval :: [SekellStmt] -> State -> IO State
        runEval [] st = do return st
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

evalStmt (StmtIf (r, stmt)) (e,p) = do 
  (t, st') <- evalExpr r (e,p)
  case t of 
    StateVar 0 -> return st'
    _ -> do (e',p') <- evalStmt stmt st'
            return (Map.intersection e' e, p')

evalStmt (StmtWhile (r, stmt)) (e,p) = do 
  (t, st') <- evalExpr r (e,p)
  case t of 
    StateVar 0 -> do {return st'}
    _ -> do (e',p') <- evalStmt stmt st'
            evalStmt (StmtWhile (r, stmt)) (Map.intersection e' e, p')

evalStmt (StmtReturn v) (e,p) = do 
  v <- case v of
    TpNull _ -> return (TpInt 0)
    _ -> return v
  (i,(e',p')) <- evalExpr v (e,p)
  return (insert returnId i e', p')

evalStmt (StmtProc (k, (a, StmtScope s))) (e,p) = do 
  return (e, insert k (StateNormProc (a,s)) p)

evalStmt (StmtAssignVar (k, v)) (e,p) = do 
  (i,(e',p')) <- evalExpr v (e,p)
  return (insert k i e', p')

evalStmt (StmtDoExpr e) s = do
  snd <$> evalExpr e s

evalStmt _ _ = undefined

----- HELP ------------------------------------------
returnId :: Identifier 
returnId = "#RTRN"




