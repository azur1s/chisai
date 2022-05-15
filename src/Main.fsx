#load "Parser.fsx"
open Parser

#load "Eval.fsx"
open Eval

open System

let usage exitCode =
    printfn "Usage: chisai [options] file"
    printfn "Options:"
    printfn "    h   Print this help message and exit"
    exit exitCode

[<EntryPoint>]
let main args =

    // Handle arguments
    if args.Length < 1 then
        usage 1

    let mutable path = ""
    let mutable opts = ""
    match args.Length with
        | 1 -> path <- args.[0] // No option provided
        | 2 -> // With options
            opts <- args.[0]
            path <- args.[1]
        | _ -> usage 1

    let mutable debug = false

    if opts.Contains("h") then
        usage 0
    if opts.Contains("d") then
        debug <- true

    // Run file
    let content = IO.File.ReadAllText path

    match parse content with
        | Parser.Success nodes ->
            if debug then printfn "%A" nodes

            let mutable evaluator = initEvaluator

            evalAll evaluator nodes

            exit 0
        | Parser.Failure err ->
            printfn "%s" err
            exit 1

    0