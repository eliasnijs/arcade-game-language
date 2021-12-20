module Keywords where

type Keyword = String

ifKey, procKey, loopKey, returnKey, andSep, orSep, strDelim, stmtDelim, gtSep, geSep, eqSep, leSep, ltSep :: Keyword
procKey      = "proc "
ifKey        = "if"
loopKey      = "while"
printKey     = "print"

strDelim     = "\""

asgnDelim    = "="
returnKey    = "fá"

argDelim     = ","
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
plusSep      = "+"
minSep       = "-"
multSep      = "*"
divSep       = "/"
