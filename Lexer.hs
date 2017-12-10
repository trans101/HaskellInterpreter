module Lexer where 

import Text.Megaparsec -- the main combinator library
import Text.Megaparsec.Char -- various basic parsers for characters
import qualified Text.Megaparsec.Char.Lexer as L -- This avoids name clashes with Prelude.


type Parser = Parsec () String

-- space1 is a parser from Text.Megaparsec.Char that will consume one or more whitespaces
-- L.space is a combinator that combines its first argument to consume space characters and its 2nd and 3rd arguments to specify how comments are formed.
-- L.skipLineComment matches its 1st argument then ignores everything to end of line
-- L.skipblockComment matches begin- and end-comment strings and ignores everything between

-- Takes out spaces, newlines, and comments
spaceConsumer :: Parser ()
spaceConsumer = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

-- Define a wrapper that uses the spaceConsumer 
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

-- These are the tokens being returned 
data MoBettaToken = Identifier String 
	| Constant Integer 
	| Symbol String 
	| Keyword String
	| Compare String
	| StringLiteral String
	| LParen
	| RParen
	| LBrack
	| RBrack
	| LBrace
	| RBrace
	deriving (Show, Eq)


-- Symbol parsing 
symbol :: String -> Parser String
symbol = L.symbol spaceConsumer  

-- Keywords
keywords = ["while", "if", "then", "else", "print", "message", "read", "true", "false"]

ifState :: Parser String
ifState = symbol "if"

whileState :: Parser String
whileState = symbol "while"

thenState :: Parser String
thenState = symbol "then"

elseState :: Parser String
elseState = symbol "else"

printState :: Parser String
printState = symbol "print"

messageState :: Parser String
messageState = symbol "message"

readState :: Parser String
readState = symbol "read"

trueState :: Parser String
trueState = symbol "true"

falseState :: Parser String
falseState = symbol "false"

ifState' :: Parser MoBettaToken
ifState' = fmap Keyword ifState

whileState' :: Parser MoBettaToken
whileState' = fmap Keyword whileState

thenState' :: Parser MoBettaToken
thenState' = fmap Keyword thenState

elseState' :: Parser MoBettaToken
elseState' = fmap Keyword elseState

printState' :: Parser MoBettaToken
printState' = fmap Keyword printState

messageState' :: Parser MoBettaToken
messageState' = fmap Keyword messageState

readState' :: Parser MoBettaToken
readState' = fmap Keyword readState

trueState' :: Parser MoBettaToken
trueState' = fmap Keyword trueState

falseState' :: Parser MoBettaToken
falseState' = fmap Keyword falseState

--Comparisons
compare = ["==", "!=", ">", "<", ">=", "<="]

equalCompare :: Parser String
equalCompare = symbol "=="

notEqCompare :: Parser String
notEqCompare = symbol "!="

greaterCompare :: Parser String
greaterCompare = symbol ">"

lessthanComapre :: Parser String
lessthanComapre = symbol "<"

greaterEqCompare :: Parser String
greaterEqCompare = symbol ">="

lessthanEqComapre :: Parser String
lessthanEqComapre = symbol "<="

equalCompare' :: Parser MoBettaToken
equalCompare' = fmap Compare equalCompare

notEqCompare' :: Parser MoBettaToken
notEqCompare' = fmap Compare notEqCompare

greaterCompare' :: Parser MoBettaToken
greaterCompare' = fmap Compare greaterCompare

lessthanComapre' :: Parser MoBettaToken
lessthanComapre' = fmap Compare lessthanComapre

greaterEqCompare' :: Parser MoBettaToken
greaterEqCompare' = fmap Compare greaterEqCompare

lessthanEqComapre' :: Parser MoBettaToken
lessthanEqComapre' = fmap Compare lessthanEqComapre


-- Operations
plus :: Parser String
plus = symbol "+"

minus :: Parser String
minus = symbol "-"

mult :: Parser String 
mult = symbol "*"

divi :: Parser String
divi = symbol "/"

equal :: Parser String 
equal = symbol "="

plus' :: Parser MoBettaToken
plus' = fmap Symbol plus 

minus' :: Parser MoBettaToken
minus' = fmap Symbol minus

mult' :: Parser MoBettaToken
mult' = fmap Symbol mult

divi' :: Parser MoBettaToken
divi' = fmap Symbol divi

equal' :: Parser MoBettaToken
equal' = fmap Symbol equal


identifier :: Parser String
identifier = (lexeme . try) p
	where 
		p = (:) <$> letterChar <*> many alphaNumChar

identifier' :: Parser MoBettaToken
identifier' = fmap Identifier identifier

intConst :: Parser Integer
intConst = (lexeme . try) ic
  where
    ic = do
      x <- L.decimal -- parse a literal
      notFollowedBy letterChar -- fail if followed by a letter
      return x -- return the  result if we haven't failed

intConst' :: Parser MoBettaToken
intConst' = fmap Constant intConst

-- deliminers
stringLiteral :: Parser String
stringLiteral = char '"' *> manyTill L.charLiteral (char '"')

stringLiteral' :: Parser MoBettaToken
stringLiteral' = fmap StringLiteral stringLiteral

lparen :: Parser Char
lparen = (lexeme . try ) (char '(')

lparen' :: Parser MoBettaToken
lparen' = lparen *> return LParen

rparen :: Parser Char
rparen = (lexeme . try) (char ')')

rparen' :: Parser MoBettaToken
rparen' = rparen *> return RParen

lbrack :: Parser Char
lbrack = (lexeme . try ) (char '[')

lbrack' :: Parser MoBettaToken
lbrack' = lbrack *> return LBrack

rbrack :: Parser Char
rbrack = (lexeme . try) (char ']')

rbrack' :: Parser MoBettaToken
rbrack' = rbrack *> return RBrack

lbrace :: Parser Char
lbrace = (lexeme . try ) (char '{')

lbrace' :: Parser MoBettaToken
lbrace' = lbrace *> return LBrace

rbrace :: Parser Char
rbrace = (lexeme . try) (char '}')

rbrace' :: Parser MoBettaToken
rbrace' = rbrace *> return RBrace




doItAll :: Parser [MoBettaToken]
doItAll = many(ifState' 
	<|> whileState' 
	<|> thenState' 
	<|> elseState' 
	<|> printState' 
	<|> messageState' 
	<|> readState'
	<|> trueState
	<|> falseState 
	<|> plus'
	<|> minus'
	<|> mult'
	<|> divi'
	<|> identifier'
	<|> intConst'
	<|> stringLiteral'
	<|> lparen'
	<|> rparen'
	<|> lbrack'
	<|> rbrack'
	<|> lbrace'
	<|> rbrace'
	<|> equal'
	<|> equalCompare'
	<|> notEqCompare'
	<|> greaterCompare'
	<|> lessthanComapre'
	<|> greaterEqCompare'
	<|> lessthanEqComapre'


