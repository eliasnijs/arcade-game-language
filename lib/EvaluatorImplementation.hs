module EvaluatorImplementation where

import Control.Monad
import Types
import Evaluator

-- Primitive types: 
--   [ ] Null
--   [ ] Bool 
--   [ ] Integer
--   [ ] Float
--   [ ] Char
--   [ ] String
--   [ ] Lists

-- Control flow:
--   [ ] create var
--   [ ] assign var
--   [ ] call var 
--   [ ] if
--   [ ] while
--   [ ] functions
--   [ ] call function
--   [ ] function return

-- Feedback:
--   [ ] print

-- Expr
evalString :: SekellExpr -> State -> String
evalString (TpString str) s = str
evalString _  s = undefined 

evalExpr :: SekellExpr -> State -> Int 

evalExpr _  s = undefined 

-- Stmt
evalStmt :: SekellStmt -> State -> (IO(), State)

evalStmt (StmtScope l) s = (do{return ()}, ns)
    where checkIfEnter :: SekellStateEle -> Bool 
          checkIfEnter SEEnterScope = True
          checkIfEnter _ = False
          runScopeEval :: [SekellStmt] -> State -> State
          runScopeEval [e] stck = tail $ takeWhile checkIfEnter $ snd $ evalStmt e stck
          runScopeEval (l:ls) stck = runScopeEval ls nstck
            where nstck = tail $ takeWhile checkIfEnter $ snd $ evalStmt l stck
          runScopeEval _ _ = undefined
          ns = runScopeEval l (s ++ [SEEnterScope])

evalStmt (StmtIf (r, stmt)) s = (do {when (evalExpr r s > 0) $ do fst $ evalStmt stmt s}, s)

evalStmt (StmtPrint x) s = (do {putStrLn (evalString x s)}, s)

evalStmt (StmtCreateVar (i, v)) s = (do {return ()}, s ++ [SEVar (i, evalExpr v s)])

evalStmt (StmtAssignVar (i, v)) s = (do {return ()}, s ++ [SEVar (i, evalExpr v s)])

evalStmt _ _ = undefined








