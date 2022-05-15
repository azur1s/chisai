#load "Parser.fsx"
open Parser

type StackNode =
    | Unit
    | Int    of int
    | Float  of float
    | Bool   of bool
    | String of string
    | Quote  of StackNode list
    | List   of StackNode list
    | Nil    of char
    | Single of char
    | Double of char
    | Triple of char

type EvalResult<'a> =
    | Success of 'a
    | Failure of string

type Evaluator = {
    mutable stack : StackNode list
}

let push evaluator node =
    evaluator.stack <- node :: evaluator.stack
    Success Unit
let pop evaluator =
    if evaluator.stack.Length = 0 then
        Failure "Stack is empty"
    else
        let mutable node = evaluator.stack.[0]
        evaluator.stack <- evaluator.stack.[1..]
        Success node
let dup evaluator =
    if evaluator.stack.Length = 0 then
        Failure "Stack is empty"
    else
        let mutable node = evaluator.stack.[0]
        evaluator.stack <- node :: evaluator.stack
        Success Unit

let doubleOps =
    [
        ('+', fun evaluator a b ->
            match (a, b) with
                | (Int a, Int b)       -> push evaluator (Int (a + b))
                | (Float a, Float b)   -> push evaluator (Float (a + b))
                | (Int a, Float b)     -> push evaluator (Float ((float a) + b))
                | (Float a, Int b)     -> push evaluator (Float (a + (float b)))
                | (String a, String b) -> push evaluator (String (a + b))
                | (a, b) -> Failure (sprintf "Cannot apply (+) on %A and %A" a b))
        ('-', fun evaluator a b ->
            match (a, b) with
                | (Int a, Int b)     -> push evaluator (Int (a - b))
                | (Float a, Float b) -> push evaluator (Float (a - b))
                | (Int a, Float b)   -> push evaluator (Float ((float a) - b))
                | (Float a, Int b)   -> push evaluator (Float (a - (float b)))
                | (a, b) -> Failure (sprintf "Cannot apply (-) on %A and %A" a b))
        ('⋅', fun evaluator a b ->
            match (a, b) with
                | (Int a, Int b)     -> push evaluator (Int (a * b))
                | (Float a, Float b) -> push evaluator (Float (a * b))
                | (Int a, Float b)   -> push evaluator (Float ((float a) * b))
                | (Float a, Int b)   -> push evaluator (Float (a * (float b)))
                | (String a, Int b)  -> push evaluator (String (String.replicate b a))
                | (Int a, String b)  -> push evaluator (String (String.replicate a b))
                | (a, b) -> Failure (sprintf "Cannot apply (*) on %A and %A" a b))
        ('÷', fun evaluator a b ->
            match (a, b) with
                | (Int a, Int b)     -> push evaluator (Int (a / b))
                | (Float a, Float b) -> push evaluator (Float (a / b))
                | (Int a, Float b)   -> push evaluator (Float ((float a) / b))
                | (Float a, Int b)   -> push evaluator (Float (a / (float b)))
                | (a, b) -> Failure (sprintf "Cannot apply (/) on %A and %A" a b))
    ]

let rec eval evaluator node =
    match node with
        | Node.Unit     -> Success Unit
        | Node.Int i    -> push evaluator (Int i)
        | Node.Float f  -> push evaluator (Float f)
        | Node.Bool b   -> push evaluator (Bool b)
        | Node.String s -> push evaluator (String s)
        | Node.Quote nodes ->
            let newEvaluator = { stack = [] }

            let rec quoteEval e node =
                match node with
                    | Node.Nil ch    -> push e (Nil ch)
                    | Node.Single ch -> push e (Single ch)
                    | Node.Double ch -> push e (Double ch)
                    | Node.Triple ch -> push e (Triple ch)
                    | a -> eval e node

            let result =
                nodes
                |> List.map (quoteEval newEvaluator)

            let failures =
                result
                // Get all the Failure's error message
                |> List.map (fun er ->
                    match er with
                        | Success node -> None
                        | Failure err -> Some err)
                // Eliminate all None
                |> List.choose id

            if failures.Length > 0 then
                Failure (failures |> String.concat "\n")
            else
                evaluator.stack <- (Quote newEvaluator.stack) :: evaluator.stack
                Success Unit
        | Node.List nodes ->
            nodes
            |> List.map (eval evaluator)
            |> List.map (fun er ->
                    match er with
                        | Success node -> None
                        | Failure err -> Some err)
            |> List.choose id
            |> function
                | [] -> Success Unit
                | errs -> Failure (errs |> String.concat "\n")
        | Node.Single op ->
            match op with
            | '.' ->
                pop evaluator
                |> function
                    | Success a ->
                        printfn "%A" a
                        Success Unit
                    | Failure err -> Failure err
            | 'd' -> dup evaluator
            | '*' ->
                pop evaluator
                |> function
                    | Success a ->
                        match a with
                            | Quote nodes ->
                                evaluator.stack <- nodes @ evaluator.stack
                                Success Unit
                            | a -> Failure (sprintf "Cannot apply (*) on %A\nPlease use (⋅) for multiplication" a)
                    | Failure err -> Failure err
            | '@' ->
                pop evaluator
                |> function
                    | Success a ->
                        match a with
                            | Nil    ch -> eval evaluator (Node.Nil ch)
                            | Single ch -> eval evaluator (Node.Single ch)
                            | Double ch -> eval evaluator (Node.Double ch)
                            | Triple ch -> eval evaluator (Node.Triple ch)
                            | a -> Failure (sprintf "Cannot apply (@) on %A" a)
                    | Failure err -> Failure err
            | op -> Failure (sprintf "Unknown operator %A" op)
        | Node.Double op ->
            let mutable a = pop evaluator
            let mutable b = pop evaluator
            match (a, b) with
                | (Success a, Success b) ->
                    doubleOps
                    |> List.tryFind (fun (s, _) -> s = op)
                    |> function
                        | Some (_, fn) -> fn evaluator a b
                        | None -> Failure (sprintf "Unknown operator %A" op)
                | (Failure a, _) -> Failure a
                | (_, Failure b) -> Failure b
        | node -> Failure (sprintf "Unknown node %A" node)

let evalAll (evaluator : Evaluator) (nodes : Node list) debug =
    for node in nodes do
        eval evaluator node
        |> function
            | Success a ->
                if debug then printfn "%A" evaluator.stack
                ()
            | Failure err ->
                printfn "Evaluation error: %s" err
                exit 1
