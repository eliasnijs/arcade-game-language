module ParserImplementation where

import Data.List (isSubsequenceOf)
import Data.Char (isDigit)
import Control.Applicative
import Parser
import Types
import Keywords

-- Expr 
--   [x] Null
--   [x] Bool 
--   [x] Integer
--   [x] Float
--   [x] Char
--   [x] String
--   [ ] Lists
--   [ ] call var 
--   [ ] call function

-- Stmt 
--   [x] create var
--   [x] assign var
--   [x] if
--   [x] while
--   [ ] return

-- Expr 
sekellNull, sekellBool, sekellInteger, sekellFloat, sekellChar, sekellString, sekellGT, sekellGE, sekellLE, sekellLT, sekellEQ, sekellAnd, sekellOr :: Parser SekellExpr 
sekellNull = TpNull <$ pString nullKey

sekellBool = TpBool . isSubsequenceOf trueKey <$> (pString trueKey <|> pString falseKey)

sekellInteger = TpInteger <$> (read <$> notNull (pSpan isDigit))

sekellFloat = TpFloat <$> (read <$> notNull ((++) <$> ((++) <$> pSpan isDigit <*> ((: []) <$> pChar '.')) <*> pSpan isDigit))

sekellChar = TpChar <$> (pString charDelim *> content <* pString charDelim)
  where content = Parser $ \(t:ts) -> Just (ts, t)

sekellString = TpString <$> (pString strDelim *> content <* pString strDelim)
  where content = pSpan (/= head strDelim) 

sekellGT = CmpGT <$> ((\v1 _ v2 -> (v1,v2)) <$> sekellStmt <*> (blank *> pString gtSep <* blank) <*> sekellStmt)
sekellGE = CmpGE <$> ((\v1 _ v2 -> (v1,v2)) <$> sekellStmt <*> (blank *> pString geSep <* blank) <*> sekellStmt)
sekellEQ = CmpEQ <$> ((\v1 _ v2 -> (v1,v2)) <$> sekellStmt <*> (blank *> pString eqSep <* blank) <*> sekellStmt)
sekellLT = CmpLT <$> ((\v1 _ v2 -> (v1,v2)) <$> sekellStmt <*> (blank *> pString ltSep <* blank) <*> sekellStmt)
sekellLE = CmpLE <$> ((\v1 _ v2 -> (v1,v2)) <$> sekellStmt <*> (blank *> pString leSep <* blank) <*> sekellStmt)

sekellAnd = CmpAnd <$> endWidth (blank *> pString andSep <* blank) sekellExpr 
sekellOr = CmpOr <$> seperateBy (blank *> pString orSep <* blank) sekellExpr 

sekellExpr = sekellNull <|> sekellChar <|> sekellString <|> sekellFloat <|> sekellInteger <|> sekellAnd <|> sekellOr <|> sekellGT <|> sekellGE <|> sekellEQ <|> sekellLE <|> sekellLT

-- Stmt 
sekellAssignVar, sekellCreateVar, sekellIf, sekellWhile, sekellReturn, sekellFunction, sekellStmt, sekellPrint :: Parser SekellStmt  
sekellAssignVar = StmtAssignVar <$> ((\k _ v -> (k, v)) <$> pSpan (/= ' ') <*> sep <*> sekellExpr)
  where sep = blank *> pString asgnDelim <* blank

sekellCreateVar = StmtCreateVar <$> (pString varKey *> blank *> content)
  where content = (\k _ v -> (k, v)) <$> pSpan (/= ' ') <*> sep <*> sekellExpr
        sep = blank *> pString asgnDelim <* blank

sekellIf = StmtIf <$> (pString ifKey *> blank *> ((,) <$> compare <*> scope))
  where compare = blank *> pString stmtInDelim *> sekellExpr <* blank <* pString stmtOutDelim
        scope = StmtScope <$> (blank *> pString scpInDelim *> blank *> endWidth (blank *> pString stmtDelim <* blank) sekellStmt <* blank <* pString scpOutDelim)

sekellWhile = StmtWhile <$> (pString loopKey *> blank *> ((,) <$> compare <*> scope))
  where compare = blank *> pString stmtInDelim *> sekellExpr <* blank <* pString stmtOutDelim
        scope = StmtScope <$> (blank *> pString scpInDelim *> blank *> endWidth (blank *> pString stmtDelim <* blank) sekellStmt <* blank <* pString scpOutDelim)

sekellFunction = StmtFunction <$> (pString procKey *> sBlank *> pString scpInDelim  *> blank *> content <* blank <* pString scpOutDelim)
  where content = StmtScope <$> endWidth (blank *> pString stmtDelim <* blank) sekellStmt

sekellScope = StmtScope <$> endWidth (blank *> pString stmtDelim <* blank) sekellStmt

sekellPrint = StmtPrint <$> (pString printKey *> blank *> pString stmtInDelim *> sekellExpr <* blank <* pString stmtOutDelim)

sekellReturn = StmtReturn <$> (pString returnKey *> blank *> sekellExpr <* blank)

sekellStmt = sekellFunction 
         <|> sekellAssignVar 
         <|> sekellCreateVar 
         <|> sekellIf 
         <|> sekellWhile 
         <|> sekellPrint 
         <|> sekellReturn







