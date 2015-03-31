module Parser ( parseProgram ) where

import AST

import qualified Text.Parsec.Token as T
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Combinator

beginEndBlock = between (reserved "begin") (reserved "end")
types = choice [ symbol "integer", symbol "boolean" ]

-- the parser
parseProgram :: GenParser Char st Program
parseProgram = do
  whiteSpace -- ignore comments and whitespace
  body <- beginEndBlock statements
  eof
  return Program { programBody = body }

-- statements
statements :: GenParser Char st [Stmt]
statements = many statement

statement :: GenParser Char st Stmt
statement =
      choice [ assignStmt, procedureCallStmt ]
  <|> ifThenStmt
  <|> whileDoStmt
  <|> loopStmt
  <|> exitStmt
  <|> returnStmt
  <|> putStmt
  <|> getStmt
  <|> scope
  <|> (try functionDeclStmt <|> variableDeclStmt <|> procedureDeclStmt )

-- <expn> <= <expn>
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

  return ReturnStmt { stmtExpn = expn }

putStmt :: GenParser Char st Stmt
putStmt = do
  reserved "put"

  outputs <- commaSep1 $ expression

  return PutStmt { outputs = outputs }

getStmt :: GenParser Char st Stmt
getStmt = do
  reserved "get"

  inputs <- commaSep1 $ expression

  return GetStmt { inputs = inputs }

procedureCallStmt :: GenParser Char st Stmt
procedureCallStmt = do
  name <- ident

  stmtArgs <- option [] $ parens $ commaSep expression

  return ProcedureCallStmt { name = name, stmtArgs = stmtArgs }

-- embedded scope statement
scope :: GenParser Char st Stmt
scope = do
  body <- beginEndBlock statements
  return Scope { body = body }

-- declarations
variableDeclStmt :: GenParser Char st Stmt
variableDeclStmt = do
  declType <- types

  declarations <- commaSep $ identDecl $ langType declType
  return DeclStmt { decls = declarations }

identDecl :: LangType -> GenParser Char st Decl
identDecl lang =  do
  i <- ident
  return ScalarDecl { declType = lang, declIdent = i }

paramDecl :: GenParser Char st Decl
paramDecl = do
  declType <- types
  i <- ident
  return ScalarDecl { declType = langType declType, declIdent = i }

-- <type> function (<params>) begin <body> end
functionDeclStmt :: GenParser Char st Stmt
functionDeclStmt = do
  -- parse type
  funcType <- types
  reserved "function"

  -- parse the name
  name <- ident

  -- get the parameters
  params <- option [] $ parens $ commaSep paramDecl

  body <- beginEndBlock statements

  return FunctionDecl { name  = name, params = params, routineBody = body, functionType = langType funcType }

procedureDeclStmt :: GenParser Char st Stmt
procedureDeclStmt = do
  reserved "procedure"

  -- parse the name
  name <- ident

  -- get the parameters
  params <- option [] $ parens $ commaSep paramDecl

  body <- beginEndBlock statements

  return ProcedureDecl { name  = name, params = params, routineBody = body }

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
commaSep = T.commaSep lexer
