#load "Parser.fsx"
open Parser

type StackNode =
    | Unit
    | Int    of int
    | Float  of float
    | Bool   of bool
    | String of string
    | Cons   of Node list
    | List   of Node list
    | Op     of char * (StackNode list -> StackNode)

type EvalResult<'a> =
    | Success of 'a
    | Failure of string

type Evaluator = {
    mutable stack : StackNode list
}
let initEvaluator = { stack = [] }

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
                | (Int a, Int b) -> push evaluator (Int (a + b))
                | (Float a, Float b) -> push evaluator (Float (a + b))
                | (Int a, Float b) -> push evaluator (Float ((float a) + b))
                | (Float a, Int b) -> push evaluator (Float (a + (float b)))
                | (String a, String b) -> push evaluator (String (a + b))
                | (a, b) -> Failure (sprintf "Cannot apply (+) on %A and %A" a b))
        ('-', fun evaluator a b ->
            match (a, b) with
                | (Int a, Int b) -> push evaluator (Int (a - b))
                | (Float a, Float b) -> push evaluator (Float (a - b))
                | (Int a, Float b) -> push evaluator (Float ((float a) - b))
                | (Float a, Int b) -> push evaluator (Float (a - (float b)))
                | (a, b) -> Failure (sprintf "Cannot apply (-) on %A and %A" a b))
        ('*', fun evaluator a b ->
            match (a, b) with
                | (Int a, Int b) -> push evaluator (Int (a * b))
                | (Float a, Float b) -> push evaluator (Float (a * b))
                | (Int a, Float b) -> push evaluator (Float ((float a) * b))
                | (Float a, Int b) -> push evaluator (Float (a * (float b)))
                | (String a, Int b) -> push evaluator (String (String.replicate b a))
                | (Int a, String b) -> push evaluator (String (String.replicate a b))
                | (a, b) -> Failure (sprintf "Cannot apply (*) on %A and %A" a b))
        ('/', fun evaluator a b ->
            match (a, b) with
                | (Int a, Int b) -> push evaluator (Int (a / b))
                | (Float a, Float b) -> push evaluator (Float (a / b))
                | (Int a, Float b) -> push evaluator (Float ((float a) / b))
                | (Float a, Int b) -> push evaluator (Float (a / (float b)))
                | (a, b) -> Failure (sprintf "Cannot apply (/) on %A and %A" a b))
    ]

let eval evaluator node =
    match node with
        | Node.Unit -> Success Unit
        | Node.Int i -> push evaluator (Int i)
        | Node.Float f -> push evaluator (Float f)
        | Node.Bool b -> push evaluator (Bool b)
        | Node.String s -> push evaluator (String s)
        | Node.List nodes -> push evaluator (List nodes)
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