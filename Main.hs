import Parser

import Text.Parsec (parseTest)

main =
  parseTest scope "begin end"
