module Keywords where

type Keyword = String

ifKey, procKey, loopKey, returnKey, andSep, orSep, strDelim, stmtSep, gtSep, geSep, eqSep, leSep, ltSep ::
     Keyword
procKey = "proc "

ifKey = "if"

loopKey = "while"

printKey = "print"

strDelim = "\""

asgnDelim = "="

returnKey = "return"

listInDelim = "["

listOutDelim = "]"

scpInDelim = "{"

scpOutDelim = "}"

stmtInDelim = "("

stmtOutDelim = ")"

listSep = ","

argSep = ","

stmtSep = ";"

andSep = "&&"

orSep = "||"

gtSep = ">"

ltSep = "<"

geSep = ">="

leSep = "<="

eqSep = "=="

plusSep = "+"

minSep = "-"

multSep = "*"

divSep = "/"
