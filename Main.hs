import Parser
import Text.Parsec (parse)
import Data.Text (splitOn, pack, unpack)
import Data.List (intercalate)

enumerate :: [a] -> [(Int, a)]
enumerate = zip [1..]

printLineNumbers :: String -> String
printLineNumbers input =
  let
    lines = enumerate $ splitOn (pack "\n") (pack input)
    printLine (lineNo, str) = show lineNo ++ "|" ++ unpack str
  in
    intercalate "\n" $ map printLine lines

main =
  let
    sourceName = "test.488"
    source = "begin \n  x <= x\n  y <= y\n  if z then\n    x <= x\n  else\n    z <= y\n  end\nend"

    parser input = case (parse parse488 sourceName input) of
      Left err -> show err
      Right x -> show x
  in
    do
      putStrLn "Input"
      putStrLn "========================="
      putStrLn $ printLineNumbers source
      putStrLn ""
      putStrLn "AST"
      putStrLn "========================="
      putStrLn $ parser source
