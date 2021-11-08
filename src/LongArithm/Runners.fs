namespace LongArithm.Interpreter

open System.Collections.Generic
open LongArithm.Parser
open LongArithm.Interpreter
open LongArithm.Interpreter.Statements

module Runners =
    /// Parses AST from input string, runs interpreter with initial empty state.
    /// Returns final program state if succeeded 
    let runTryCatchErrors input =
        let _input = $"\n_________\n{input}\n_________\n"
        try
            let ast = parseString input
            // log parsing results
            Highlighting.logOk
                $"Finished parsing of text: {_input}"
                $"Abstract Syntax Tree:\n\t%A{ast}"

            let initialState = { VariableTable = [] ; OutputBuffer = Queue<_>()}
            printfn "\nRunning ...\n___\n"

            let res = runStatements ast initialState
            Highlighting.logOk "Succeeded!" $"Result: {res}"
            // return computed result
            Ok res
        with
        | InterpreterParsingError msg ->
            Highlighting.logError "Parsing error" msg $"while parsing {_input}"
            Error $"=> Failed: Parsing error\n%s{msg}\n=> while parsing %s{_input}"
        | InterpreterRuntimeError msg -> 
            Highlighting.logError "Runtime error" msg $"while running {_input}"
            Error $"=> Failed: Runtime error\n%s{msg}\n=> while parsing %s{_input}"
 
    /// Simply runs the interpreter
    let run input =
        let _input = $"\n_________\n{input}\n_________\n"
        let ast = parseString input
        // log parsing results
        Highlighting.logOk
            $"Finished parsing of text: {_input}"
            $"Abstract Syntax Tree:\n\t%A{ast}"

        let initialState = { VariableTable = [] ; OutputBuffer = Queue<_>()}
        printfn "\nRunning ...\n___\n"

        let res = runStatements ast initialState
        Highlighting.logOk "Succeeded!" $"Result {res}"
        // return computed result
        res
    