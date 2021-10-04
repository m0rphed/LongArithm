namespace LongArithm.Interpreter

open LongArithm.Parser

module Types =
    type ProgramState = {VariableTable: (Name * Value) list}