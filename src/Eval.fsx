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

type StackResult =
    | Success
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
    Success

let rec eval (e : byref<Evaluator>) (node : Node) =
    match node with
        | Node.Int i ->
            push &e (StackNode.Int i)
        | a -> Failure (sprintf "todo: %A" a)

let evalNodes (e : byref<Evaluator>) (nodes : Node list) =
    for node in nodes do
        let result = eval &e node
        match result with
            | Success _ -> ()
            | Failure err ->
                printfn "Eval error: %s" err
                exit 1
