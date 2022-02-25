namespace LongArithm.Parser

open LongArithm.BigInt

module AST =
    type Name = string

    type Value =
        | Int of MyBigInt
        | Str of string
        | Bool of bool
    
    type BinOperator =
        | Add // Arithmetic operators
        | Sub
        | Mult
        | Pow
        | Div
        | Mod
        | Gt // Comparison operators
        | Lt
        | Gte
        | Lte
        | Eq // Equality operators
        | Neq
        | And // Boolean operators
        | Or
        | StrConcat // String concatenation
    
    type UnaryOperator =
        | Abs
        | Negate
        
    type Expr =
        | Literal of Value
        | Variable of name: Name
        // The Expr type is recursive, as operations
        // can consist of expressions
        | BinaryOp of (Expr * BinOperator * Expr)
        | UnaryOp of (UnaryOperator * Expr)

    type Statement =
        | Print of Expr
        | Set of name: Name * value: Expr
        | If of condition: Expr * body: Block * Else: Block option
        | While of condition: Expr * body: Block

    and Block = Statement list