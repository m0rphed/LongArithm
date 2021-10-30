namespace LongArithm.Interpreter

open LongArithm.Parser.AST
open LongArithm.Interpreter.Types
open LongArithm.Interpreter.BinOperators

module Expressions =
    let lookupVariable state name =
        state.VariableTable
        |> List.filter (fun (n, _) -> n = name)
        |> List.map snd
        |> List.head

    let printValue value =
        match value with
        | Int bigInt -> printfn $"%A{bigInt}"
        | Bool b    -> printfn $"%b{b}"
        | Str s  -> printfn $"%s{s}"

    let interpretConditionalValue = function
        | Bool b    -> b
        | _         -> true
    
    let interpretIntegerValue = function
        | Int i -> i
        | _     -> 0I
        
    let rec applyOperator state op (args: list<Expr>) =
        let reduceToValue exp =
            match exp with
            | Literal value         -> value
            | Variable name         -> lookupVariable state name
            | BinaryOp (x, op', y) -> applyOperator state op' [x; y]
            | _ -> failwith "todo"
        let e1, e2 = args.[0], args.[1]
        mapBinOperator op (reduceToValue e1) (reduceToValue e2)

    let rec evaluateExpression state = function
        | Literal value -> value
        | Variable name -> lookupVariable state name
        | BinaryOp (e1, op, e2) -> applyOperator state op [e1; e2]
        | _ -> failwith "todo"

    let evaluateCondition cond state =
        cond |> evaluateExpression state |> interpretConditionalValue