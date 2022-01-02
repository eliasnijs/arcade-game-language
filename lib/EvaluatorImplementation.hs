module EvaluatorImplementation where

import Data.Maybe
import Data.Map as Map
import Control.Monad
import Types
import Engine
import qualified Control.Arrow as Map
import System.Random (StdGen, getStdGen, randomR)

----- EXPR ------------------------------------------
-- StateT State IO StateValue kon hier goed gebruikt worden
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

evalExpr (CallVar k) (e,p,rd) = do 
  let i' = fromMaybe StateNULL (Map.lookup k e)
  return (i',(e,p,rd)) 

evalExpr (CallProc (k,a)) (e,p,rd) = do
  let f = fromMaybe StateProcNULL (Map.lookup k p)
  (e',p',rd) <- case f of
    StateNormProc (aid, stmt) -> 
      do argstmt <- do return $ StmtAssignVar <$> zip aid a
         stmt' <- do return $ StmtScope $ (++) argstmt stmt
         evalStmt stmt' (e,p,rd)
    StateStdProc g -> evalStdProc g a (e,p,rd)
    StateProcNULL  -> do print ("no procudure `" ++ k ++ "` was found!")
                         error ""
  let r = fromMaybe StateNULL (Map.lookup returnId e')
  e'' <- do return $ delete returnId e' 
  return (r, (Map.intersection e'' e,p,rd)) 

evalExpr _ s = do
  error "Error: invalid expression!"

evalBinOpExpr :: SekellExpr -> SekellExpr -> State -> (Int -> Int -> Int) -> IO (StateValue, State)
evalBinOpExpr e1 e2 s f = 
  do (n1,st') <- evalExpr e1 s
     (n2,st'') <- evalExpr e2 st'
     case (n1, n2) of
       (StateVar n1', StateVar n2') -> return (StateVar (f n1' n2'), st'')
       (_, _) -> return (StateNULL, st'')

evalStdProc :: StdProc -> [SekellExpr] -> State -> IO State
evalStdProc (AC1 f) [] (e,p,rd) = do
  let v = f
  return (insert returnId v e, p, rd)
evalStdProc (AC2 f) [arg1] (e,p,rd) = do
  (a1,(e',p',rd)) <- evalExpr arg1 (e,p,rd)
  let v = f a1
  return (insert returnId v e', p',rd)
evalStdProc (AC3 f) [arg1, arg2] (e,p,rd) = do
  (a1,s) <- evalExpr arg1 (e,p,rd)
  (a2,(e',p',rd)) <- evalExpr arg2 s
  let v = f a1 a2
  return (insert returnId v e', p',rd)
evalStdProc (AC4 f) [arg1, arg2, arg3] (e,p,rd) = do
  (a1,s) <- evalExpr arg1 (e,p,rd)
  (a2,s') <- evalExpr arg2 s
  (a3,(e',p',rd)) <- evalExpr arg3 s'
  let v = f a1 a2 a3 
  return (insert returnId v e, p, rd)
evalStdProc RD4 [arg1, arg2] (e,p,rd) = do
  (a1,s) <- evalExpr arg1 (e,p,rd)
  (a2,(e',p',rd)) <- evalExpr arg2 s
  (v',rd'') <- case (a1,a2) of 
    (StateVar a, StateVar b) -> do let (v,rd') = randomR (a, b) rd
                                   return (StateVar v, rd')
    (_,_) -> error "invalid arguments for random!" 
  return (insert returnId v' e, p, rd'')
evalStdProc _ _ s = return s
    
----- STMT ------------------------------------------
evalStmt :: SekellStmt -> State -> IO State

evalStmt (StmtScope x) (e,p,rd) = do runEval x (e,p,rd)
  where runEval :: [SekellStmt] -> State -> IO State
        runEval [] st = do return st
        runEval (x:xs) (e',p',rd) = do
          let r = fromMaybe StateNULL (Map.lookup returnId e')
          xs' <- case x of
            StmtReturn x' -> do return []
            _ -> do return xs
          (e''',p''',rd) <- case x of
            StmtDoExpr x' -> do evalStmt x (Map.intersection e' e, p',rd)
            _             -> do (e'',p'',rd) <- evalStmt x (e',p',rd)
                                return (e'',p'',rd)
          runEval xs' (e''',p''',rd)

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

evalStmt (StmtIf (r, stmt)) (e,p,rd) = do 
  (t, st') <- evalExpr r (e,p,rd)
  case t of 
    StateVar 0 -> return st'
    _ -> do (e',p',rd) <- evalStmt stmt st'
            return (Map.intersection e' e, p',rd)

evalStmt (StmtWhile (r, stmt)) (e,p,rd) = do 
  (t, st') <- evalExpr r (e,p,rd)
  case t of 
    StateVar 0 -> do {return st'}
    _ -> do (e',p',rd) <- evalStmt stmt st'
            evalStmt (StmtWhile (r, stmt)) (Map.intersection e' e, p',rd)

evalStmt (StmtReturn v) (e,p,r) = do 
  v <- case v of
    TpNull _ -> return (TpInt 0)
    _ -> return v
  (i,(e',p',r)) <- evalExpr v (e,p,r)
  return (insert returnId i e', p', r)

evalStmt (StmtProc (k, (a, StmtScope s))) (e,p, r) = do 
  return (e, insert k (StateNormProc (a,s)) p, r)

evalStmt (StmtAssignVar (k, v)) (e,p, r) = do 
  (i,(e',p', r)) <- evalExpr v (e,p, r)
  return (insert k i e', p', r)

evalStmt (StmtDoExpr e) s = do
  snd <$> evalExpr e s

evalStmt _ _ = error "Error: invalid statement!"

----- HELP ------------------------------------------
returnId :: Identifier 
returnId = "#RTRN"




