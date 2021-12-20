module Types where

import Data.Map

type Identifier = String

-- TODO
data SekellExpr
  = TpInt Int
  | TpString String
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

type State = (Map Identifier Int, Map Identifier SekellStmt) 




