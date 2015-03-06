import Parser
import AST
import Text.Parsec (Parsec, ParseError, parse, runParser)
import Data.Text (splitOn, pack, unpack)
import Data.List (intercalate)

-- pretty print
enumerate :: [a] -> [(Int, a)]
enumerate = zip [1..]

printLineNumbers :: String -> String
printLineNumbers input =
  let
    lines = enumerate $ splitOn (pack "\n") (pack input)
    printLine (lineNo, str) = show lineNo ++ "|" ++ unpack str
  in
    intercalate "\n" $ map printLine lines

parseFromFile :: Parsec String () Stmt -> String -> IO (Either ParseError Stmt)
parseFromFile p fname = do
  source <- readFile fname

  putStrLn "Input"
  putStrLn "========================="
  putStrLn $ printLineNumbers source
  putStrLn ""

  return (runParser p () fname source)

main =
  do
    either <- parseFromFile parseProgram "test.488"
    putStrLn "AST"
    putStrLn "========================="
    putStrLn $ show either
