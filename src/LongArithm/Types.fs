namespace LongArithm.Interpreter

open LongArithm.Parser.AST
open System.Collections.Generic
open System

type ProgramState =
    { VariableTable: (Name * Value) list
      OutputBuffer: Queue<string> }

module Events =
    let dataToConsole = Event<string>()
    let printed = dataToConsole.Publish

/// Error should be thrown when parsing fails
exception InterpreterParsingError of string

/// Error should be thrown when running of expression or statement fails
exception InterpreterRuntimeError of string

/// Basic highlighting
module Highlighting =
    /// Simply logs colorized text to console
    let consoleLog =
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
        consoleLog darkMagenta $"=> %s{header}\n" 
        consoleLog green "=> Done:"
        consoleLog darkCyan $"%s{result.ToString()}"
        
    /// Logs highlighted error to console
    let logError header msg footer =
        consoleLog darkRed "=> Failed: "
        consoleLog red $"%s{header}"
        consoleLog darkYellow $"%s{msg}"
        consoleLog yellow $"=> %s{footer}"
        