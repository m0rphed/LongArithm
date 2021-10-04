namespace LongArithm.Interpreter

open LongArithm.Parser
open LongArithm.Interpreter.Types
open LongArithm.Interpreter.Operators

module Expressions =
    let lookupVariable state name =
        state.VariableTable
        |> List.filter (fun (n, _) -> n = name)
        |> List.map snd
        |> List.head

    let printValue value =
        match value with
        | Int i -> printfn $"%i{i}"
        | Bool b    -> printfn $"%b{b}"
        | Str s  -> printfn $"%s{s}"

    let interpretConditionalValue = function
        | Bool b    -> b
        | _         -> true
    
    let interpretIntegerValue = function
        | Int i -> i
        | _         -> 0

    let rec applyOperator state op e1 e2 =
        let reduceToValue exp =
            match exp with
            | Literal value         -> value
            | Variable name         -> lookupVariable state name
            | Operation (x, op', y) -> applyOperator state op' x y
        
        mapOperator op (reduceToValue e1) (reduceToValue e2)

    let rec evaluateExpression state = function
        | Literal value -> value
        | Variable name -> lookupVariable state name
        | Operation (e1, op, e2) -> applyOperator state op e1 e2

    let evaluateCondition cond state =
        cond |> evaluateExpression state |> interpretConditionalValue