module Parser ( parseProgram ) where

import AST

import qualified Text.Parsec.Token as T
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Combinator

-- the parser
parseProgram = do
  program <- scope
  eof
  return program

scope :: GenParser Char st Stmt
scope = do
  whiteSpace -- ignore comments and whitespace
  body <- between (reserved "begin") (reserved "end") statements
  return Scope { body = body }

-- statements
statements :: GenParser Char st [Stmt]
statements = many statement

statement :: GenParser Char st Stmt
statement =
      assignStmt
  <|> ifThenStmt
  <|> whileDoStmt
  <|> loopStmt
  <|> exitStmt
  <|> returnStmt
  <|> scope
  <|> declStmt

assignStmt :: GenParser Char st Stmt
assignStmt = do
  left <- expression
  operator "<="
  right <- expression
  return AssignStmt { left = left, right = right }

ifThenStmt :: GenParser Char st Stmt
ifThenStmt = do
  -- if <expression> then <body> end
  cond <- between (reserved "if") (reserved "then") expression

  -- body
  body <- statements

  -- optional else
  elseBody <- choice
    -- try to do the else first
    [ between (reserved "else") (reserved "end") statements,
    -- if that doesn't work, get the end
      do { reserved "end"; return [] } ]

  return IfThenStmt { condition = cond, trueBody = body, falseBody = elseBody }

whileDoStmt :: GenParser Char st Stmt
whileDoStmt = do
  -- while <expression> do <statement> end
  reserved "while"
  cond <- expression
  body <- between (reserved "do") (reserved "end") statements

  return WhileDoStmt { condition = cond, body = body }

loopStmt :: GenParser Char st Stmt
loopStmt = do
  -- loop <statement> end
  body <- between (reserved "loop") (reserved "end") statements

  return LoopStmt { body = body }

exitStmt :: GenParser Char st Stmt
exitStmt = do
  reserved "exit"

  expn <- option NullExpn $ do { reserved "when"; expn <- expression; return expn }

  return ExitStmt { stmtExpn = expn }

returnStmt :: GenParser Char st Stmt
returnStmt = do
  reserved "return"

  expn <- option NullExpn $ parens expression

  return ExitStmt { stmtExpn = expn }

-- declarations
declStmt :: GenParser Char st Stmt
declStmt = do
  declType <- choice [ symbol "integer", symbol "boolean" ]

  declarations <- commaSep1 $ declaration $ stringToDecl declType
  return DeclStmt { decls = declarations }

declaration :: LangType -> GenParser Char st Decl
declaration lang =
  scalarDecl lang

scalarDecl :: LangType -> GenParser Char st Decl
scalarDecl lang = do
  i <- ident
  return ScalarDecl { declType = lang, declIdent = i }

-- expressions
expression :: GenParser Char st Expn
expression =
      identExpn
  <|> intConstExpn
  <|> boolConstExpn
  <|> parens expression

intConstExpn :: GenParser Char st Expn
intConstExpn = do
  int <- parseInt

  return IntConstExpn { int = int }

boolConstExpn :: GenParser Char st Expn
boolConstExpn = do
  sym <- symbol "true" <|> symbol "false"

  return BoolConstExpn { bool = (sym == "true") }

identExpn :: GenParser Char st Expn
identExpn = do
  i <- ident
  return IdentExpn { identifier = i }

-- the lexer
langDef :: T.LanguageDef st
langDef = T.LanguageDef {
  T.commentStart="", T.commentEnd="",
  T.commentLine="%", T.nestedComments=False,
  T.identStart = letter,
  T.identLetter = alphaNum <|> char '_',
  T.opStart = oneOf "",
  T.opLetter = oneOf "",
  T.reservedNames=["begin", "end", "if", "then", "end", "else", "while", "do", "loop", "exit", "when", "return", "put", "get", "integer", "boolean", "function", "procedure", "skip", "true", "false", "yields"],
  T.reservedOpNames=["<=", "..", "-", "+", "*", "/", "!", "&", "|", "=", "!=", "<", ">", ">="],
  T.caseSensitive=True
}

lexer = T.makeTokenParser langDef
reserved = T.reserved lexer
operator = T.reservedOp lexer
ident = T.identifier lexer
parens = T.parens lexer
whiteSpace = T.whiteSpace lexer
parseInt = T.integer lexer
symbol = T.symbol lexer
commaSep1 = T.commaSep1 lexer
