namespace LongArithm.Interpreter

open LongArithm.Parser.AST

module Types =
    type ProgramState = {VariableTable: (Name * Value) list}