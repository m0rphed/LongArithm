namespace LongArithm.Interpreter

open LongArithm.Parser.AST
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
        | Int bigInt -> printfn $"%A{bigInt}"
        | Bool b    -> printfn $"%b{b}"
        | Str s  -> printfn $"%s{s}"

    let interpretConditionalValue = function
        | Bool b    -> b
        | _         -> true
    
    let interpretIntegerValue = function
        | Int i -> i
        | _     -> 0I

    let rec evaluateExpr state expr =
        let applyBinOp state op first second =
            mapBinOperator op
                (evaluateExpr state first)
                (evaluateExpr state second)
        
        let applyUnaryOp state op expr =
            mapUnaryOperator op (evaluateExpr state expr)

        match expr with
        | Literal value         -> value
        | Variable name         -> lookupVariable state name
        | BinaryOp (e1, op, e2) -> applyBinOp state op e1 e2
        | UnaryOp (op', x)      -> applyUnaryOp state op' x

    let evaluateCondition cond state =
        cond |> evaluateExpr state |> interpretConditionalValue