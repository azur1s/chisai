#load "Combinator.fsx"
open Combinator

#load "Parser.fsx"
open Parser

let esc = "\x1b"
let green = esc + "[32m"
let reset = esc + "[0m"

let test p input =
    printfn "%sRunning parser %s with input %s:%s" green (getLabel p) input reset
    run p input
    |> printResult

test literalNode  "\"Hello\""
test operaterNode "¬"
test operaterNode "_"
test consNode     "(1 2 +)@."
test listNode     "[1 2 3 4 5]"
test parseNodes   "1 2 +@(10 *)d@."
test parseNodes   "[1 2 3 4 5]∑."