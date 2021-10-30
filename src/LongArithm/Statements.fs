namespace LongArithm.Interpreter

open System
open LongArithm
open LongArithm.Parser.AST
open LongArithm.Interpreter.Types
open LongArithm.Interpreter.Expressions

module Statements =
    let display exp state =
        printValue (evaluateExpr state exp)
        state

    let set name exp state =
        let eValue = evaluateExpr state exp
        {state with VariableTable = (name, eValue) :: state.VariableTable}
    
    let read name state =
        let input = Console.ReadLine()
            
        let (|Int|_|) input =
           match BigInt.tryParseBigInt (input: string) with
           | true, bigInteger -> Some(bigInteger)
           | _ -> None

        let (|Bool|_|) input =
           match Boolean.TryParse (input: string) with
           | true, bool -> Some(bool)
           | _ -> None
        
        let value = match input with
                    | Int i     -> Int i
                    | Bool b    -> Bool b
                    | _         -> Value.Str input
        
        {state with VariableTable = (name, value) :: state.VariableTable}

    let compute name exp state =
        let eValue = evaluateExpr state exp
        {state with VariableTable = (name, eValue) :: state.VariableTable}
    
    let rec conditional cond block elseBlockOption state =
        if (evaluateCondition cond state) then
            runStatements block state
        else
            match elseBlockOption with
            | Some elseBlock  -> runStatements elseBlock state
            | None              -> state

    and whileLoop cond block state =
        if (evaluateCondition cond state) then
            let rec innerLoop innerState =
                if (cond |> evaluateExpr innerState |> interpretConditionalValue)
                then runStatements block innerState |> innerLoop
                else innerState
            innerLoop state
        else state
    
    and runStatement state s =
        state
        |>  match s with
            | Print exp                             -> display exp
            | Set (name, exp)                       -> set name exp
            | If (cond, block, elseBlockOption)     -> conditional cond block elseBlockOption
            | While (cond, block)                   -> whileLoop cond block

    and runStatements statements state =
        match statements with
        | []        ->  state
        | [s]       ->  runStatement state s
        | h::t      ->  runStatement state h
                        |> runStatements t
