module Parser ( parse488 ) where

import Text.Parsec.Token
import Text.ParserCombinators.Parsec

-- the parser
parse488 = scope

scope :: GenParser Char st String
scope = do
  keyword "begin"
  keyword "end"
  eof
  return "Worked!"

-- the lexer
langDef :: LanguageDef st
langDef = LanguageDef {
  commentStart="", commentEnd="",
  commentLine="%", nestedComments=False,
  identStart = letter,
  identLetter = alphaNum <|> char '_',
  opStart = oneOf "<.-+*/!&&|=>",
  opLetter = oneOf "=.",
  reservedNames=["begin", "end", "if", "then", "end", "else", "while", "do", "loop", "exit", "when", "return", "put", "get", "integer", "boolean", "function", "procedure", "skip", "true", "false", "yields"],
  reservedOpNames=["<=", "..", "-", "+", "*", "/", "!", "&", "|", "=", "!=", "<", ">", ">="],
  caseSensitive=True
}

lexer = makeTokenParser langDef
keyword = reserved lexer
