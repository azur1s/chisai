#load "Parser.fsx"
open Parser

let testParser = spaces1 &> int <& spaces1 &> char '+' <& spaces1 &> int

run testParser "  123 + 456 "
|> printResult

run testParser "  -123 + 456 "
|> printResult

run testParser " 123 + A"
|> printResult