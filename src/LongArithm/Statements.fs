namespace LongArithm.Interpreter

open LongArithm.Parser
open LongArithm.Interpreter.Types
open LongArithm.Interpreter.Operators
open LongArithm.Interpreter.Expressions

open System

module Statements =
    let display exp state =
        printValue (evaluateExpression state exp)
        state

    let set name exp state =
        let eValue = evaluateExpression state exp
        {state with VariableTable = (name, eValue) :: state.VariableTable}
    
    let read name state =
        let input = Console.ReadLine()
            
        let (|Int|_|) input =
           match Int32.TryParse(input:string) with
           | true,int -> Some(int)
           | _ -> None

        let (|Bool|_|) input =
           match Boolean.TryParse(input:string) with
           | true, bool -> Some(bool)
           | _ -> None
        
        let value = match input with
                    | Int i     -> Int i
                    | Bool b    -> Bool b
                    | _         -> Value.Str input
        
        {state with VariableTable = (name, value) :: state.VariableTable}

    let compute name exp state =
        let eValue = evaluateExpression state exp
        {state with VariableTable = (name, eValue) :: state.VariableTable}
    
    let rec conditional cond block elseBlockOption state =
        if (evaluateCondition cond state) then
            runStatements block state
        else
            match elseBlockOption with
            | Some elseBlock  -> runStatements elseBlock state
            | None              -> state

    and whileloop cond block state =
        if (evaluateCondition cond state) then
            let rec innerLoop innerState =
                if (cond |> evaluateExpression innerState |> interpretConditionalValue) then 
                    runStatements block innerState |> innerLoop
                else
                    innerState
                
            innerLoop state
        else
            state
    
    and dowhile cond block state =
        let updatedState = runStatements block state
        
        if  (evaluateCondition cond updatedState) then
            let rec innerLoop innerState =
                if (evaluateCondition cond innerState) then 
                    runStatements block innerState
                    |> innerLoop
                else
                    innerState
                
            innerLoop updatedState 
        else
            updatedState
    
    and repeat n block state =
        let n' = interpretIntegerValue n
        let rec loop acc state' =
            match (acc, state') with
            | count, s when count = n'    -> runStatements block s
            | _, s                        -> runStatements block s |> loop (acc + 1)
        
        loop 0 state

    and runStatement state s =
        state
        |>  match s with
            | Print exp                             -> display exp
            | Set (name, exp)                       -> set name exp
            | If (cond, block, elseBlockOption)     -> conditional cond block elseBlockOption
            | While (cond, block)                   -> whileloop cond block
//            | READ name                             -> read name
//            | COMPUTE (name, exp)                   -> compute name exp
//            | DOWHILE (block, cond)                 -> dowhile cond block
//            | REPEAT (block, count)                 -> repeat (evaluateExpression state count) block
//            | HALT                                  -> id

    and runStatements statements state =
        match statements with
        | []        ->  state
        | [s]       ->  runStatement state s
        | h::t      ->  runStatement state h
                        |> runStatements t
