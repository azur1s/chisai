#load "Combinator.fsx"
open Combinator

type Node =
    | Unit
    | Int    of int
    | Float  of float
    | Bool   of bool
    | String of string
    | Quote   of Node list
    | List   of Node list
    | Nil    of char
    | Single of char
    | Double of char
    | Triple of char

/// Parse a single literal
let literalNode =
    let unitNode =
        word "u" >| (fun _ -> Unit)

    let intNode =
        int
        >| (fun i -> Int i)
        <?> "integer"

    let floatNode =
        float
        >| (fun f -> Float f)
        <?> "float"

    let boolNode =
        (word "t"  >| (fun _ -> Bool true))
        <|> (word "f" >| (fun _ -> Bool false))
        <?> "boolean"

    let unescapeChar = satisfy (fun ch -> ch <> '\\' && ch <> '\"') "character"

    let escapedChar =
        [
        ("\\\"", '\"')
        ("\\\\", '\\')
        ("\\/" , '/' )
        ("\\b" , '\b')
        ("\\f" , '\f')
        ("\\n" , '\n')
        ("\\r" , '\r')
        ("\\t" , '\t')
        ]
        |> List.map (fun (m, r) -> word m >| (fun _ -> r))
        |> choice
        <?> "escaped character"

    let stringNode =
        char '\"' &> manyChars (unescapeChar <|> escapedChar) <& char '\"'
        >| (fun s -> String s)

    choice [unitNode; intNode; floatNode; boolNode; stringNode]
    <?> "literal"

let operaterNode =
    let nil    = anyOf (stringToChars "c") >| (fun op -> Nil op)
    let single = anyOf (stringToChars "¬d.@*∑") >| (fun op -> Single op)
    let double = anyOf (stringToChars "+-⋅÷") >| (fun op -> Double op)
    let triple = anyOf (stringToChars "?") >| (fun op -> Triple op)

    choice [nil; single; double; triple]
    <?> "operator"

let QuoteNode =
    char '(' &> sepBy (literalNode <|> operaterNode) spaces <& char ')'
    >| (fun l -> Quote l)
    <?> "quote"

let listNode =
    char '[' &> sepBy1 literalNode spaces <& char ']'
    >| (fun l -> List l)
    <?> "list"

let parseNode =
    choice [literalNode; operaterNode; QuoteNode; listNode]
    <?> "node"
let parseNodes =
    sepBy1 parseNode spaces

type ParseResult =
    | Success of Node list
    | Failure of string

let parse input =
    match run parseNodes input with
        | Combinator.Success (value, _) ->
            Success value
        | Combinator.Failure (label, error, pos) ->
            let esc = "\x1b"
            let red = esc + "[31m"
            let reset = esc + "[0m"

            let header = sprintf "Error parsing %s (Line %i, Column %i):" label pos.line pos.column
            let preview = sprintf "    %s\n    %*s╰─ %s" pos.currentLine pos.column "" error
            Failure (sprintf "%s\n\n%s\n" (sprintf "%s%s%s" red header reset) preview)