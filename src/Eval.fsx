#load "Parser.fsx"
open Parser

type StackNode =
    | Int    of int
    | Float  of float
    | Bool   of bool
    | String of string
    | Cons   of Node list
    | List   of Node list
    | Op     of char * (StackNode list -> StackNode)

type StackResult<'a> =
    | Success of 'a
    | Failure of string

type Evaluator = {
    mutable stack : StackNode list
}

let initEvaluator = {
    stack = []
}

let push (e : byref<Evaluator>) (snode : StackNode) =
    let newStack = snode :: e.stack
    e <- { stack = newStack }
    Success ()

/// Reverse a list
let rev list =
    List.fold (fun acc elem -> elem::acc) [] list

let pop (e : byref<Evaluator>) =
    match e.stack with
        | [] -> Failure "Stack underflow"
        | _ ->
            // [1, 2, 3]
            // [1, 2] -> 3
            let revStack = rev e.stack
            let elem = revStack.Head
            let newStack = rev revStack.Tail

            e <- { stack = newStack }
            Success elem

let rec eval (e : byref<Evaluator>) (node : Node) =
    match node with
        | Node.Int i       -> push &e (StackNode.Int i)
        | Node.Float f   -> push &e (StackNode.Float f)
        | Node.Bool b     -> push &e (StackNode.Bool b)
        | Node.String s -> push &e (StackNode.String s)

        | Node.Op opchar ->
            match opchar with
            | '+' ->
                let a = pop &e
                let b = pop &e
                match (a, b) with
                | (Success (StackNode.Int a), Success (StackNode.Int b)) -> push &e (StackNode.Int (a + b))
                | (Failure err, _) -> Failure err
                | (_, Failure err) -> Failure err // Maybe unreachable?
                | _ -> Failure "Invalid operands for +"
            | '.' ->
                let a = pop &e
                printfn "%A" a
                Success ()
            | op -> Failure (sprintf "Unknown operator: %A" op)
        | a -> Failure (sprintf "todo: %A" a)

let evalNodes (e : byref<Evaluator>) (nodes : Node list) =
    for node in nodes do
        let result = eval &e node
        match result with
            | Success _ -> ()
            | Failure err ->
                printfn "Eval error: %s" err
                exit 1
