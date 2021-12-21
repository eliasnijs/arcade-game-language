module Types where

import Data.Map

type Identifier = String

data SekellExpr
  = TpNull      SekellExpr
  | TpInt       Int
  | TpString    String
  | TpList      [SekellExpr]
  | CallVar     Identifier
  | CallProc    (Identifier, [SekellExpr])
  | CallProcArg Identifier
  | OpPLUS      (SekellExpr, SekellExpr)
  | OpMIN       (SekellExpr, SekellExpr)
  | OpMULT      (SekellExpr, SekellExpr)
  | OpDIV       (SekellExpr, SekellExpr)
  | CmpGT       (SekellExpr, SekellExpr)
  | CmpGE       (SekellExpr, SekellExpr)
  | CmpEQ       (SekellExpr, SekellExpr)
  | CmpLT       (SekellExpr, SekellExpr)
  | CmpLE       (SekellExpr, SekellExpr)
  | CmpAnd      (SekellExpr, SekellExpr)
  | CmpOr       (SekellExpr, SekellExpr)
  deriving (Show, Eq)

data SekellStmt
  = StmtPrint     SekellExpr
  | StmtAssignVar (Identifier, SekellExpr)
  | StmtReturn    SekellExpr
  | StmtIf        (SekellExpr, SekellStmt)
  | StmtWhile     (SekellExpr, SekellStmt)
  | StmtProc      (Identifier, ([Identifier], SekellStmt))
  | StmtDoExpr    SekellExpr
  | StmtScope     [SekellStmt]
  | StmtFileScope [SekellStmt]
  deriving (Show, Eq)


data StateValue 
  = StateList [StateValue]
  | StateVar Int
  | StateNULL
  deriving (Show, Eq)

data StdProc
  = AC2         (StateValue -> StateValue)
  | AC3         (StateValue -> StateValue -> StateValue)
  | AC4         (StateValue -> StateValue -> StateValue -> StateValue)

data StateProc
  = StateNormProc ([Identifier], [SekellStmt])
  | StateStdProc StdProc

type State = (Map Identifier StateValue, Map Identifier StateProc) 






