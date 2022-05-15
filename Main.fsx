#load "Combinator.fsx"
open Combinator

#load "Parser.fsx"
open Parser

run literal "\"Hello\""
|> printResult