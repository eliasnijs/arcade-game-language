module Keywords where

type Keyword = String

nullKey, varKey, ifKey, procKey, loopKey, returnKey, trueKey, falseKey, andSep, orSep, charDelim, strDelim, stmtDelim, gtSep, geSep, eqSep, leSep, ltSep :: Keyword
nullKey      = "NULL"
trueKey      = "F"
falseKey     = "T"

varKey       = "var"
procKey      = "proc"
ifKey        = "if"
loopKey      = "while"
printKey     = "print"

strDelim     = "\""
charDelim    = "\'"

asgnDelim    = "="
returnKey    = "return"

stmtDelim    = ";"
scpInDelim   = "{"
scpOutDelim  = "}"
stmtInDelim  = "("
stmtOutDelim = ")"

andSep       = "&&"
orSep        = "||"
gtSep        = ">"
ltSep        = "<"
geSep        = ">="
leSep        = "<="
eqSep        = "=="

