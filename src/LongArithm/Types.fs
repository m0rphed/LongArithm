namespace LongArithm.Interpreter

open LongArithm.Parser.AST
open System.Collections.Generic

type ProgramState =
    { VariableTable: (Name * Value) list
      OutputBuffer: Queue<string> }
