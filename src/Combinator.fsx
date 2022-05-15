open System

// ==============================================
// Parser Types
// ==============================================

type Position = { line : int; column : int }
let initialPosition = { line = 0; column = 0 }
let incrementPosition pos = { pos with column = pos.column + 1 }
let incrementLine     pos = { line = pos.line + 1; column = 0 }

type InputState = {
    lines : string[]
    pos   : Position
}

module InputState =
    let fromStr str =
        if String.IsNullOrEmpty(str) then
            { lines = [||]; pos = initialPosition }
        else
            let sep = [| "\r\n"; "\n" |]
            let lines = str.Split(sep, StringSplitOptions.None)
            { lines = lines; pos = initialPosition}

    /// Return current line
    let currentLine input =
        let linePos = input.pos.line
        if linePos < input.lines.Length then
            input.lines.[linePos]
        else
            "end of file"

    let next input =
        let linePos = input.pos.line
        let colPos = input.pos.column

        if linePos >= input.lines.Length then
            input, None
        else
            let currentLine = currentLine input
            if colPos < currentLine.Length then
                let char = currentLine.[colPos]
                let newPos = incrementPosition input.pos
                let newState = { input with pos = newPos }
                newState, Some char
            else
                let newPos = incrementLine input.pos
                let newState = {input with pos = newPos }
                newState, Some '\n'

type ParserPosition = {
    currentLine : string
    line        : int
    column      : int
}

let posFromInputState input = {
    currentLine = InputState.currentLine input
    line = input.pos.line
    column = input.pos.column
}

type ParserLabel = string
type ParserError = string

type ParseResult<'a> =
    | Success of 'a
    | Failure of ParserLabel * ParserError * ParserPosition

type Parser<'a> = {
    fn    : (InputState -> ParseResult<'a * InputState>)
    label : ParserLabel
}

/// Get a label from a parser
let getLabel p = p.label
/// Set a label on a parser
let setLabel p label =
    let fn input =
        let result = p.fn input
        match result with
        | Success s -> Success s
        | Failure (_, err, pos) -> Failure (label, err, pos)
    { fn = fn; label = label }
let ( <?> ) = setLabel

/// Run the parser with InputState input
let runInput p input =
    p.fn input

/// Run a parser with string input
let run p input =
    runInput p (InputState.fromStr input)

/// Print the result of a parser
let printResult result =
    match result with
        | Success (value, _) ->
            printf "%A\n" value
        | Failure (label, error, pos) ->
            let esc = string (char 0x1B)
            let red = esc + "[31m"
            let reset = esc + "[0m"

            let header = sprintf "Error parsing %s (Line %i, Column %i):" label pos.line pos.column
            let preview = sprintf "    %s\n    %*s╰─ %s" pos.currentLine pos.column "" error
            printfn "%s\n\n%s" (sprintf "%s%s%s" red header reset) preview

// ==============================================
// Parser Combinators
// ==============================================

/// Match an input token if the predicate is satisfied
let satisfy pred label =
    let fn input =
        let remaining, maybeChar = InputState.next input
        match maybeChar with
        | None ->
            Failure (label, "No more input", posFromInputState input)
        | Some first ->
            if pred first then
                Success (first, remaining)
            else
                Failure (label, sprintf "Unexpected '%c'" first, posFromInputState input)
    { fn = fn; label = label }

/// Takes a function that produces parser (f) and a parser (p) and passes
/// the output of parser (p) into the function (f) to create a new parser
let bindParser f p =
    let fn input =
        let result = runInput p input
        match result with
        | Failure (label, err, pos) -> Failure (label, err, pos)
        | Success (value, remaining) ->
            let p2 = f value
            runInput p2 remaining
    { fn = fn; label = "unknown" }
let ( >>= ) p f = bindParser f p

/// Lift normal value to a parser
let returnParser a =
    let fn (input : 'b) = Success (a, input)
    { fn = fn; label = sprintf "%A" a }

/// Apply a function to the value inside a parser
let mapParser f = bindParser (f >> returnParser)
let ( <.> ) = mapParser

/// Piping version of mapParser
let ( >| ) p f = f <.> p

/// Applies two parsers in sequence and returns result in tuple
let andThen p1 p2 =
    p1 >>= (fun r1 ->
        p2 >>= (fun r2 ->
            returnParser (r1, r2)))
    <?> sprintf "%s then %s" (getLabel p1) (getLabel p2)
let ( <&> ) = andThen

/// Apply a wrapped function to a wrapped value
let applyParser fp ap =
    fp >>= (fun f ->
        ap >>= (fun a ->
            returnParser (f a)))
let ( <*> ) = applyParser

/// (helper)
let rec zeroOrMore p input =
    let firstResult = runInput p input
    match firstResult with
    | Failure _ -> ([], input)
    | Success (firstValue, remaining) ->
        let (rest, lastRemaining) = zeroOrMore p remaining
        (firstValue::rest, lastRemaining)

/// Match zero or more occurrences of the parser
let many p =
    { fn = (fun input -> Success (zeroOrMore p input)); label = sprintf "zero or more of (%s)" (getLabel p) }

/// Match one or more occurrences of the parser
let many1 p =
    p >>= (fun head ->
        many p >>= (fun tail ->
            returnParser (head::tail)))
    <?> sprintf "one or more of (%s)" (getLabel p)

/// Run first parser and if it fails, run second parser
let orElse p1 p2 =
    let label = sprintf "%s or %s" (getLabel p1) (getLabel p2)
    let fn input =
        let result1 = runInput p1 input
        match result1 with
        | Success _ -> result1
        | Failure _ ->
            let result2 = runInput p2 input
            match result2 with
            | Success _ -> result2
            | Failure (_, err, pos) -> Failure (label, err, pos)
    { fn = fn; label = label }
let ( <|> ) = orElse

/// Choose any of the parsers
let choice ps =
    List.reduce ( <|> ) ps

/// Lift a 2 parameter function to a parser
let lift2 f ap bp =
    returnParser f <*> ap <*> bp

let rec sequence parsers =
    let cons head tail = head::tail

    let consP = lift2 cons

    match parsers with
    | [] -> returnParser []
    | head::tail -> consP head (sequence tail)

let optional p =
    let some = p >| Some
    let none = returnParser None
    some <|> none

/// Keep the result of the left parser
let ( <& ) p1 p2 = p1 <&> p2 |> mapParser (fun (a, _) -> a)
/// Keep the result of the right parser
let ( &> ) p1 p2 = p1 <&> p2 |> mapParser (fun (_, b) -> b)

/// Parses one or more occurrences of the parser separated by the separator (allowed trailing separator)
let sepBy1 p sep =
    let sepThenP = sep &> p
    p <&> many sepThenP
    >| fun (p, pList) -> p::pList

/// Parses zero or more occurrences of the parser separated by the separator (allowed trailing separator)
let sepBy p sep = sepBy1 p sep <|> returnParser []

// ==============================================
// Standard Parsers
// ==============================================

/// Parse a single character
let char c =
    let pred ch = (ch = c)
    satisfy pred (sprintf "%c" c)

let charsToString chars =
    chars |> List.toArray |> System.String

let stringToChars str =
    str |> Seq.toList

/// Parse a sequence of zero of more characters and return a string
let manyChars cs = many cs >| charsToString
let many1Chars cs = many1 cs >| charsToString

/// Choose any of a list of characters
let anyOf chars =
    chars
    |> List.map char
    |> choice
    <?> sprintf "any of %A" chars

/// Parse a specific string
let word s =
    s
    |> List.ofSeq
    |> List.map char
    |> sequence
    |> mapParser charsToString
    <?> sprintf "word `%s`" s

/// Parse a single whitespace char
let whitespace = satisfy Char.IsWhiteSpace "whitespace"
/// Parse zero or more whitespace characters
let spaces = many whitespace
/// Parse one or more whitespace characters
let spaces1 = many1 whitespace

/// Parse a single digit
let digit = satisfy Char.IsDigit "digit"
/// Parse an integer (positive only)
let uint =
    let resultToInt digits =
        digits |> int

    let digits = many1Chars digit

    digits
    |> mapParser resultToInt
    <?> "positive integer"
/// Parse an integer (negative allowed)
let int =
    let resultToInt (neg, digits) =
        let i = digits |> int
        match neg with
        | Some _ -> -i
        | None -> i

    let digits = many1Chars digit

    optional (char '-') <&> digits
    |> mapParser resultToInt
    <?> "integer"
/// Parse a float
let float =
    let resultToFloat (((neg, digits1), _), digits2) =
        let fl = sprintf "%s.%s" digits1 digits2 |> float
        match neg with
        | Some _ -> -fl
        | None -> fl

    let digits = many1Chars digit

    optional (char '-') <&> digits <&> char '.' <&> digits
    |> mapParser resultToFloat
    <?> "float"