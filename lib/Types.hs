module Types where

import Data.Map

type Identifier = String

data SekellExpr
  = TpInt Int
  | TpString String
  | TpList [SekellExpr]
  | CallVar Identifier
  | CallProc (Identifier, [SekellExpr])
  | OpPLUS (SekellExpr, SekellExpr)
  | OpMIN (SekellExpr, SekellExpr)
  | OpMULT (SekellExpr, SekellExpr)
  | OpDIV (SekellExpr, SekellExpr)
  | CmpGT (SekellExpr, SekellExpr)
  | CmpGE (SekellExpr, SekellExpr)
  | CmpEQ (SekellExpr, SekellExpr)
  | CmpLT (SekellExpr, SekellExpr)
  | CmpLE (SekellExpr, SekellExpr)
  | CmpAnd (SekellExpr, SekellExpr)
  | CmpOr (SekellExpr, SekellExpr)
  | StmtCallVar Identifier
  deriving (Show, Eq)

data SekellStmt
  = StmtPrint SekellExpr
  | StmtAssignVar (Identifier, SekellExpr)
  | StmtReturn SekellExpr
  | StmtIf (SekellExpr, SekellStmt)
  | StmtWhile (SekellExpr, SekellStmt)
  | StmtProc (Identifier, [SekellStmt], SekellStmt)
  | StmtDoExpr SekellExpr
  | StmtScope [SekellStmt]
  deriving (Show, Eq)


data StateValue 
  = StateList [StateValue]
  | StateVar Int
  | StateNULL
  deriving (Show, Eq)

type State = (Map Identifier StateValue, Map Identifier SekellStmt) 







