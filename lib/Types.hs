module Types where

type Identifier = String

-- TODO
data SekellExpr
  = TpNull
  | TpBool Bool
  | TpInteger Int
  | TpFloat Float
  | TpChar Char
  | TpString String
  | CmpGT (SekellStmt, SekellStmt)
  | CmpGE (SekellStmt, SekellStmt)   
  | CmpEQ (SekellStmt, SekellStmt)  
  | CmpLT (SekellStmt, SekellStmt) 
  | CmpLE (SekellStmt, SekellStmt) 
  | CmpAnd [SekellExpr]
  | CmpOr [SekellExpr]
  | StmtCallVar Identifier
  deriving (Show, Eq)

data SekellStmt 
  = StmtPrint SekellExpr 
  | StmtAssignVar (Identifier, SekellExpr)
  | StmtCreateVar (Identifier, SekellExpr)
  | StmtReturn SekellExpr
  | StmtIf (SekellExpr, SekellStmt)
  | StmtWhile (SekellExpr, SekellStmt)
  | StmtFunction SekellStmt 
  | StmtScope [SekellStmt]
  deriving (Show, Eq)



data SekellStateEle 
  = SEVar (Identifier, Int)
  | SEEnterScope  

type State = [SekellStateEle]








