namespace LongArithm.Interpreter

open System
open LongArithm
open LongArithm.BigInt
open LongArithm.Parser.AST

module Operators =
    (* Binary operators functions *)
    let binaryIntOperation op x y =
        match x, y with
        | Int a, Int b -> Int (op a b)
        | xy ->
            $"Operator {op} expected 'Int' type; instead got: {xy}"
            |> InterpreterRuntimeError
            |> raise
    
    /// Addition of two integers
    let add         = binaryIntOperation (+)
    let subtract    = binaryIntOperation (-)
    let modulus     = binaryIntOperation (%)
    let multiply    = binaryIntOperation (*)
    let pow         = binaryIntOperation (^^)
    let divide      = binaryIntOperation (/)
    
    let compare comparison x y =
        match x, y with
        | Int a, Int b -> Bool (comparison a b)
        | xy ->
            $"Comparison expected 'Int' type; instead got:{xy}"
            |> InterpreterRuntimeError
            |> raise

    let eq  = compare BigInt.equal          // (=)
    let neq = compare BigInt.notEqual       // (<>)
    let gt  = compare BigInt.greater        // (>)
    let lt  = compare BigInt.less           // (<)
    let gte = compare BigInt.greaterOrEqual // (>=)
    let lte = compare BigInt.lessOrEqual    // (<=)
    
    let junction func x y =
        match x, y with
        | Bool a, Bool b -> Bool (func a b)
        | xy ->
            $"Junction expected 'Bool' type; instead got:{xy}"
            |> InterpreterRuntimeError
            |> raise

    let conjunction = junction (&&)
    let disjunction = junction (||)
    
    let mapBinOperator = function
        | Add       -> add
        | Sub       -> subtract
        | Mult      -> multiply
        | Div       -> divide
        | Mod       -> modulus
        | Gt        -> gt
        | Lt        -> lt
        | Gte       -> gte
        | Lte       -> lte
        | Eq        -> eq
        | Neq       -> neq
        | And       -> conjunction
        | Or        -> disjunction
        | Pow       -> pow
        | op -> $"No such operator {op}" |> InterpreterRuntimeError |> raise 
    
    (* Unary operators: Abs, Negate *)
    
    let unaryIntOperation op x =
        match x with
        | Int num -> Int (op num)
        | other ->
            $"Operator {op} expected 'Int' type; instead got: {other}"
            |> InterpreterRuntimeError
            |> raise
        
    let abs = unaryIntOperation BigInt.abs
    let negate = unaryIntOperation BigInt.negate

    let mapUnaryOperator = function
        | Abs       -> abs
        | Negate    -> negate