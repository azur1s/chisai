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

let eval evaluator node =
    match node with
        | Unit -> Success Unit
        | Int i -> push evaluator (Int i)
        | Float f -> push evaluator (Float f)
        | Bool b -> push evaluator (Bool b)
        | String s -> push evaluator (String s)