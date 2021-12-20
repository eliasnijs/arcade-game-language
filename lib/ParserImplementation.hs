module ParserImplementation where

import Data.List (isSubsequenceOf)
import Data.Char (isDigit, isAlpha)
import Control.Applicative
import Parser
import Types
import Keywords

----- EXPR ------------------------------------------
sekellInt, sekellString, sekellCallVar, sekellCallProc, sekellGT, sekellGE, sekellLE, sekellLT, sekellEQ, sekellAnd, sekellOr, sekellPLUS, sekellMIN, sekellMULT, sekellDIV  :: Parser SekellExpr 

sekellNat = TpInt <$> (read <$> notNull (pSpan isDigit))

sekellNeg = TpInt . ((-1) *) <$> (pChar '-' *> (read <$> notNull (pSpan isDigit)))

sekellInt = sekellNeg <|> sekellNat 

sekellString = TpString <$> (pString strDelim *> pSpan (/= head strDelim) <* pString strDelim)

sekellCallVar = CallVar <$> (blank *> notNull (pSpan (\x -> isAlpha x || isDigit x || x == '_')) <* blank)

sekellCallProc = CallProc <$> ((,) <$> id <*> args)
  where id   = blank *> pSpan (\x -> isAlpha x || isDigit x || x == '_') <* blank
        args = blank *>  pString stmtInDelim *> blank *> seperateBy (blank *> pString argDelim <* blank) sekellExpr <* blank <* pString stmtOutDelim <* blank

sekellPLUS = OpPLUS <$> (pString stmtInDelim *> blank *> ((\v1 _ v2 -> (v1,v2)) <$> sekellExpr <*> (blank *> pString plusSep  <* blank) <*> sekellExpr) <* pString stmtOutDelim)

sekellMIN = OpMIN <$> (pString stmtInDelim *> blank *> ((\v1 _ v2 -> (v1,v2)) <$> sekellExpr <*> (blank *> pString minSep <* blank) <*> sekellExpr) <* pString stmtOutDelim)

sekellMULT = OpMULT <$> (pString stmtInDelim *> blank *> ((\v1 _ v2 -> (v1,v2)) <$> sekellExpr <*> (blank *> pString multSep <* blank) <*> sekellExpr) <* pString stmtOutDelim)

sekellDIV = OpDIV <$> (pString stmtInDelim *> blank *> ((\v1 _ v2 -> (v1,v2)) <$> sekellExpr <*> (blank *> pString divSep <* blank) <*> sekellExpr) <* pString stmtOutDelim)

sekellGT = CmpGT <$> (pString stmtInDelim *> blank *> ((\v1 _ v2 -> (v1,v2)) <$> sekellExpr <*> (blank *> pString gtSep <* blank) <*> sekellExpr) <* pString stmtOutDelim)

sekellGE = CmpGE <$> (pString stmtInDelim *> blank *> ((\v1 _ v2 -> (v1,v2)) <$> sekellExpr <*> (blank *> pString geSep <* blank) <*> sekellExpr) <* pString stmtOutDelim)

sekellEQ = CmpEQ <$> (pString stmtInDelim *> blank *> ((\v1 _ v2 -> (v1,v2)) <$> sekellExpr <*> (blank *> pString eqSep <* blank) <*> sekellExpr) <* pString stmtOutDelim)

sekellLT = CmpLT <$> (pString stmtInDelim *> blank *> ((\v1 _ v2 -> (v1,v2)) <$> sekellExpr <*> (blank *> pString ltSep <* blank) <*> sekellExpr) <* pString stmtOutDelim)

sekellLE = CmpLE <$> (pString stmtInDelim *> blank *> ((\v1 _ v2 -> (v1,v2)) <$> sekellExpr <*> (blank *> pString leSep <* blank) <*> sekellExpr) <* pString stmtOutDelim)

sekellAnd = CmpAnd <$> (pString stmtInDelim *> blank *> ((\v1 _ v2 -> (v1,v2)) <$> sekellExpr <*> (blank *> pString orSep <* blank) <*> sekellExpr) <* pString stmtOutDelim)

sekellOr = CmpOr <$> (pString stmtInDelim *> blank *> ((\v1 _ v2 -> (v1,v2)) <$> sekellExpr <*> (blank *> pString orSep <* blank) <*> sekellExpr) <* pString stmtOutDelim)

sekellExpr = sekellAnd <|> sekellOr <|> sekellGT <|> sekellGE <|> sekellEQ <|> sekellLT <|> sekellLE <|> sekellPLUS <|> sekellMIN <|> sekellMULT <|> sekellDIV <|> sekellString  <|> sekellInt  <|> sekellCallProc <|> sekellCallVar

----- STMT ------------------------------------------
sekellAssignVar, sekellIf, sekellWhile, sekellReturn, sekellProc, sekellStmt, sekellPrint, stmtDoExpr :: Parser SekellStmt  
sekellAssignVar = StmtAssignVar <$> ((\k _ v -> (k, v)) <$> pSpan (\x -> isAlpha x || isDigit x || x == '_') <*> (blank *> pString asgnDelim <* blank) <*> sekellExpr)

sekellIf = StmtIf <$> (pString ifKey *> blank *> ((,) <$> compare <*> scope))
  where compare = blank *>  pString stmtInDelim *> blank *> sekellExpr <* blank <* pString stmtOutDelim
        scope = blank *> pString scpInDelim *> blank *> sekellScope <* blank <* pString scpOutDelim

sekellWhile = StmtWhile <$> (pString loopKey *> blank *> ((,) <$> compare <*> scope))
  where compare = blank *>  pString stmtInDelim *> blank *> sekellExpr <* blank <* pString stmtOutDelim
        scope = blank *> pString scpInDelim *> blank *> sekellScope <* blank <* pString scpOutDelim

sekellProc = StmtProc <$> (pString procKey *> ((,,) <$> id <*> args <*> scope))
  where id = blank *> pSpan (\x -> isAlpha x || isDigit x || x == '_') <* blank
        args = blank *>  pString stmtInDelim *> blank *> seperateBy (blank *> pString argDelim <* blank) sekellStmt <* blank <* pString stmtOutDelim <* blank
        scope = blank *> pString scpInDelim *> blank *> sekellScope <* blank <* pString scpOutDelim

sekellScope = StmtScope <$> endWidth (blank *> pString stmtDelim <* blank) sekellStmt

sekellPrint = StmtPrint <$> (pString printKey *> blank *> pString stmtInDelim *> sekellExpr <* blank <* pString stmtOutDelim)

sekellReturn = StmtReturn <$> (pString returnKey *> blank *> sekellExpr <* blank)

stmtDoExpr = StmtDoExpr <$> sekellExpr

sekellStmt = sekellProc <|> sekellIf <|> sekellAssignVar <|> sekellWhile  <|> sekellPrint <|> sekellReturn <|> stmtDoExpr
