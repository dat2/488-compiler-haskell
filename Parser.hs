module Parser ( parse488 ) where

import AST

import qualified Text.Parsec.Token as T
import Text.ParserCombinators.Parsec

-- the parser
parse488 = scope

scope :: GenParser Char st AST
scope = do
  reserved "begin"
  body <- statementsAndEnd
  eof
  return Scope { body = body }

-- statements
statementsAndEnd :: GenParser Char st [AST]
statementsAndEnd =
  manyTill statement $ reserved "end"

statement :: GenParser Char st AST
statement =
  try $ (assignStmt <|> ifThenStmt)

assignStmt :: GenParser Char st AST
assignStmt = do
  id <- ident
  operator "<="
  expr <- expression
  return AssignStmt { lhs = id, rhs = expr }

ifThenStmt :: GenParser Char st AST
ifThenStmt = do
  reserved "if"
  cond <- expression
  reserved "then"
  body <- statementsAndEnd
  return IfThenStmt { condition = cond, body = body }

-- expressions
expression :: GenParser Char st AST
expression = identExpr

identExpr :: GenParser Char st AST
identExpr = do
  i <- ident
  return IdentExpr { identifier = i }

-- the lexer
langDef :: T.LanguageDef st
langDef = T.LanguageDef {
  T.commentStart="", T.commentEnd="",
  T.commentLine="%", T.nestedComments=False,
  T.identStart = letter,
  T.identLetter = alphaNum <|> char '_',
  T.opStart = oneOf "<.-+*/!&&|=>",
  T.opLetter = oneOf "=.",
  T.reservedNames=["begin", "end", "if", "then", "end", "else", "while", "do", "loop", "exit", "when", "return", "put", "get", "integer", "boolean", "function", "procedure", "skip", "true", "false", "yields"],
  T.reservedOpNames=["<=", "..", "-", "+", "*", "/", "!", "&", "|", "=", "!=", "<", ">", ">="],
  T.caseSensitive=True
}

lexer = T.makeTokenParser langDef
reserved = T.reserved lexer
operator = T.reservedOp lexer
ident = T.identifier lexer
