#load "Combinator.fsx"
open Combinator

type Node =
    | Int    of int
    | Float  of float
    | Bool   of bool
    | String of string
    | List   of Node list
    | Nil    of char
    | Single of char
    | Double of char
    | Triple of char

/// Parse a single literal
let literal =
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

    intNode <|> floatNode <|> boolNode <|> stringNode