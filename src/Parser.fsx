#r "nuget: FParsec,1.1.1"
open FParsec

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
        pstring "u"
        |>> (fun _ -> Unit)

    let intNode =
        puint64
        |>> (fun i -> Int (int i))
        <?> "integer"

    let floatNode =
        pfloat
        |>> (fun f -> Float f)
        <?> "float"

    let boolNode =
        (pstring "t" |>> (fun _ -> Bool true))
        <|> (pstring "f" |>> (fun _ -> Bool false))
        <?> "boolean"

    let stringNode =
        let normal = satisfy (fun c -> c <> '\\' && c <> '"')
        let unescape c =
            match c with
            | 'n' -> '\n'
            | 'r' -> '\r'
            | 't' -> '\t'
            | c   -> c
        let escaped = pstring "\\" >>. (anyOf "\\nrt\"" |>> unescape)

        (many (normal <|> escaped))
        |> between (pstring "\"") (pstring "\"") 
        |>> (fun chars -> chars |> Array.ofList |> System.String |> String)

    choice [unitNode; intNode; floatNode; boolNode; stringNode]
    <?> "literal"

let opNode =
    let charsFrom s = Seq.toList s
    let nil    = anyOf (charsFrom "!")      |>> (fun op -> Nil op)
    let single = anyOf (charsFrom "¬$.@*∑") |>> (fun op -> Single op)
    let double = anyOf (charsFrom "+-⋅÷:")  |>> (fun op -> Double op)
    let triple = anyOf (charsFrom "?")      |>> (fun op -> Triple op)

    choice [nil; single; double; triple]
    <?> "operator"

let quoteNode =
    pstring "(" >>. sepBy (literalNode <|> opNode) spaces .>> pstring ")"
    |>> (fun l -> Quote l)
    <?> "quote"

let listNode =
    pstring "[" >>. sepBy1 literalNode spaces .>> pstring "]"
    |>> (fun l -> List l)
    <?> "list"

let parseNodes =
    let nodes = sepEndBy1 (choice [literalNode; opNode; quoteNode; listNode]) spaces

    spaces >>. nodes .>> spaces .>> eof
    <?> "nodes"

type ParseResult =
    | PSuccess of Node list
    | PFailure of string

let parse input =
    match run parseNodes input with
    | Success (value, _, _) -> PSuccess value
    | Failure (msg, _, _) -> PFailure msg
