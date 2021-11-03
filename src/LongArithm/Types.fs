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
    /// Simply logs colorized text to console
    let log =
        let lockObj = obj()
        fun color str ->
            lock lockObj (fun _ ->
                Console.ForegroundColor <- color
                printfn $"%s{str}"
                Console.ResetColor())
    
    (* define aliases for bright colors *)
    let red = ConsoleColor.Red
    let yellow = ConsoleColor.Yellow
    let green = ConsoleColor.Green

    (* define aliases for darker console colors *)
    let darkRed = ConsoleColor.DarkRed
    let darkYellow = ConsoleColor.DarkYellow
    let darkMagenta = ConsoleColor.DarkMagenta
    let darkCyan = ConsoleColor.DarkCyan
    
    /// Logs highlighted result to console
    let logOk header result =
        log darkMagenta $"=> %s{header}\n" 
        log green "=> Done:"
        log darkCyan $"%s{result.ToString()}"
        
    /// Logs highlighted error to console
    let logError header msg footer =
        log darkRed "=> Failed: "
        log red $"%s{header}"
        log darkYellow $"%s{msg}"
        log yellow $"=> %s{footer}"