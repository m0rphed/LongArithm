namespace LongArithm.Interpreter

open System
open LongArithm.BigInt
open LongArithm.Parser.AST
open LongArithm.Interpreter
open LongArithm.Interpreter.Expressions

module Statements =
    /// Gets exact value of specified literal value
    let private exactValueStr literal =
        match literal with
        | Int myBigInt -> $"%A{myBigInt}"
        | Bool bool -> $"%b{bool}"
        | Str str -> str
    
    let display exp state =
        let evalResult = evaluateExpr state exp
        // save value to print buffer
        evalResult
        |> exactValueStr
        |> state.OutputBuffer.Enqueue
        // print to std out
        printValue evalResult
        state

    let set name exp state =
        let eValue = evaluateExpr state exp
        {state with VariableTable = (name, eValue) :: state.VariableTable}
    
    /// Reads input line from console
    let read name state =
        let input = Console.ReadLine()
            
        let (|Int|_|) input =
           match BigInt.tryParseBigInt (input: string) with
           | Ok bigInteger -> Some(bigInteger)
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
