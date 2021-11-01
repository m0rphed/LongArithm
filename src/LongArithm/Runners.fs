namespace LongArithm.Interpreter

open System.Collections.Generic
open LongArithm.Parser
open LongArithm.Interpreter
open LongArithm.Interpreter.Statements

module Runners =
    /// Parses AST from input string, runs interpreter with initial empty state.
    /// Returns final program state if succeeded 
    let run input =
        let ast = parseString input
        let initialState = { VariableTable = [] ; OutputBuffer = Queue<_>()}
        runStatements ast initialState