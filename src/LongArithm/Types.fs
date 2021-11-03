namespace LongArithm.Interpreter

open LongArithm.Parser.AST
open System.Collections.Generic
open System

type ProgramState =
    { VariableTable: (Name * Value) list
      OutputBuffer: Queue<string> }

/// Error should be thrown when parsing fails
exception InterpreterParsingError of string

/// Error should be thrown when running of expression or statement fails
exception InterpreterRuntimeError of string

/// Basic highlighting
module Highlighting =
    let magenta = ConsoleColor.Magenta
    let green = ConsoleColor.Green
    let darkGreen = ConsoleColor.DarkRed
    let cyan = ConsoleColor.Cyan
    let yellow = ConsoleColor.Yellow
    let red = ConsoleColor.Red
    
    let logOK header result =
        Console.ForegroundColor <- magenta
        printfn $"=> %s{header}\n"
        Console.ForegroundColor <- green
        printfn "=> Done:"
        Console.ForegroundColor <- cyan
        printfn $"%s{result.ToString()}"
        Console.ResetColor()
        
    let logError header msg footer =
        Console.ForegroundColor <- darkGreen
        printf "=> Failed: "
        Console.ForegroundColor <- red
        printfn $"%s{header}"
        Console.ResetColor()
        printfn $"%s{msg}"
        Console.ForegroundColor <- yellow
        printfn $"=> %s{footer}"
        Console.ResetColor()