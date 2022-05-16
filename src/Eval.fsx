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

let singleOps =
    [
        ('@', fun eval evaluator a ->
            match a with
            | Nil    ch -> eval evaluator (Node.Nil ch)
            | Single ch -> eval evaluator (Node.Single ch)
            | Double ch -> eval evaluator (Node.Double ch)
            | Triple ch -> eval evaluator (Node.Triple ch)
            | a -> Failure (sprintf "Cannot apply (@) on %A" a))
        ('*', fun eval evaluator a ->
            match a with
            | Quote nodes ->
                evaluator.stack <- nodes @ evaluator.stack
                Success Unit
            | a -> Failure (sprintf "Cannot apply (*) on %A\nPlease use (⋅) for multiplication" a))
        ('.', fun _ _ a ->
            printfn "%A" a
            Success Unit)
        ('$', fun eval evaluator a ->
            push evaluator a
            |> function
                | Success _ ->
                    push evaluator a
                    |> function
                        | Success _   -> Success Unit
                        | Failure err -> Failure err
                | Failure err -> Failure err)
        ('∑', fun eval evaluator a ->
            let rec sum total list fail =
                match list with
                | [] -> total
                | x :: xs ->
                    match x with
                    | Int m ->
                        match total with
                        | Int n   -> sum (Int (m + n)) xs fail
                        | Float n -> sum (Float (float m + n)) xs fail
                        | m -> sum total list (Some (Failure (sprintf "Cannot apply (∑) on %A of %A" m a)))
                    | Float m ->
                        match total with
                        | Int n   -> sum (Float (m + float n)) xs fail
                        | Float n -> sum (Float (m + n)) xs fail
                        | m -> sum total list (Some (Failure (sprintf "Cannot apply (∑) on %A of %A" m a)))
                    | m -> sum total list (Some (Failure (sprintf "Cannot apply (∑) on %A of %A" m a)))

            match a with
            | List nodes ->
                push evaluator (sum (Int 0) nodes None)
            | a -> Failure (sprintf "Cannot apply (∑) on %A" a))
    ]

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
        (':', fun evaluator a b ->
            match (a, b) with
            | (Int a, Int b) ->
                [a..b]
                // Wrap everything in (Int ..)
                |> List.map (fun i -> Int i)
                // Wrap everything in List (..)
                |> StackNode.List
                |> push evaluator
            | (a, b) -> Failure (sprintf "Cannot apply (:) on %A and %A" a b))
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

        let failures =
            nodes
            |> List.map (quoteEval newEvaluator)
            // Get all the Failure's error message
            |> List.map (fun er ->
                match er with
                    | Success node -> None
                    | Failure err -> Some err)
            // Eliminate all None
            |> List.choose id

        match failures with
        | [] ->
            evaluator.stack <- (Quote newEvaluator.stack) :: evaluator.stack
            Success Unit
            | _ -> Failure (failures |> String.concat "\n")
    | Node.List nodes ->
        let newEvaluator = { stack = [] }

        let failures =
            nodes
            |> List.map (eval newEvaluator)
            |> List.map (fun er ->
                match er with
                | Success _ -> None
                | Failure err -> Some err)
            |> List.choose id

        match failures with
        | [] ->
            evaluator.stack <- (List newEvaluator.stack) :: evaluator.stack
            Success Unit
        | _ -> Failure (failures |> String.concat "\n")
    | Node.Single op ->
        let mutable a = pop evaluator
        match a with
        | Success a ->
            singleOps
            |> List.tryFind (fun (s, _) -> s = op)
            |> function
                | Some (_, fn) -> fn eval evaluator a
                | None -> Failure (sprintf "Unknown operator %A" op)
        | Failure err -> Failure err
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
