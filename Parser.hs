module Parser ( scope ) where

import Text.Parsec
import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Token as P
import qualified Text.ParserCombinators.Parsec.Char as C

-- the parser
-- parse488 :: String -> String
-- parse488 input = case (parse scope input) of
--   Left err -> "err"
--   Right x -> "no error"

-- scope :: GenParser Char st String
scope = do
  reserved "begin"
  reserved "end"
  eof
  return True


-- the lexer
langDef :: P.LanguageDef st
langDef = P.LanguageDef {
  P.commentStart="", P.commentEnd="",
  P.commentLine="%", P.nestedComments=False,
  P.identStart = C.letter,
  P.identLetter = C.alphaNum <|> char '_',
  P.opStart = C.oneOf "<.-+*/!&&|=>",
  P.opLetter = C.oneOf "=.",
  P.reservedNames=["begin", "end", "if", "then", "end", "else", "while", "do", "loop", "exit", "when", "return", "put", "get", "integer", "boolean", "function", "procedure", "skip", "true", "false", "yields"],
  P.reservedOpNames=["<=", "..", "-", "+", "*", "/", "!", "&", "|", "=", "!=", "<", ">", ">="],
  P.caseSensitive=True
}

lexer = P.makeTokenParser langDef
reserved = P.reserved lexer
