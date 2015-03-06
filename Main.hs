import Parser
import Text.Parsec (parse)

parser input = case (parse parse488 "" input) of
  Left err -> show err
  Right x -> "Result: " ++ x

main =
  putStrLn $ parser "begin end"
