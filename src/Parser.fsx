#load "Combinator.fsx"
open Combinator

type Node =
    | Int    of int
    | Float  of float
    | Bool   of bool
    | String of string
    | Cons   of Node list
    | List   of Node list
    | Nil    of char
    | Single of char
    | Double of char
    | Triple of char

/// Parse a single literal
let literalNode =
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
    <?> "literal"

let operaterNode =
    let nil =
        anyOf (stringToChars "c")
        >| (fun c -> Nil c)
    let single =
        anyOf (stringToChars "¬d@.∑")
        >| (fun c -> Single c)
    let double =
        anyOf (stringToChars "+-*/")
        >| (fun c -> Double c)
    let triple =
        anyOf (stringToChars "?")
        >| (fun c -> Triple c)

    nil <|> single <|> double <|> triple
    <?> "operator"

let consNode =
    char '(' &> sepBy (literalNode <|> operaterNode) spaces <& char ')'
    >| (fun l -> Cons l)
    <?> "cons"

let listNode =
    char '[' &> sepBy1 literalNode spaces <& char ']'
    >| (fun l -> List l)
    <?> "list"

let parseNode =
    literalNode <|> operaterNode <|> consNode <|> listNode
let parseNodes =
    sepBy parseNode spaces