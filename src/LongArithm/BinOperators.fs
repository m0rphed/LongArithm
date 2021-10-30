namespace LongArithm.Interpreter

open LongArithm.Parser.AST

module BinOperators =
    let binaryIntOperation op x y =
        match x, y with
        | Int a, Int b -> Int (op a b)
        | xy -> failwith $"Operator {op} expected 'Int' type; instead got: {xy}"
    
    /// Addition of two integers
    let add         = binaryIntOperation (+)
    let subtract    = binaryIntOperation (-)
    let modulus     = binaryIntOperation (%)
    let multiply    = binaryIntOperation (*)
    let divide      = binaryIntOperation (/)
    
    let compare comparison x y =
        match x, y with
        | Int a, Int b -> Bool (comparison a b)
        | xy -> failwith $"Comparison expected 'Int' type; instead got:{xy}"

    let eq  = compare (=)
    let neq = compare (<>)
    let gt  = compare (>)
    let lt  = compare (<)
    let gte = compare (>=)
    let lte = compare (<=)
    
    let junction func x y =
        match x, y with
        | Bool a, Bool b -> Bool (func a b)
        | xy -> failwith $"Junction expected 'Bool' type; instead got:{xy}"

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
        | StrConcat -> failwith "Operator 'String Concat' currently not implement :("
    
    let unaryIntOperation op x =
        match x with
        | Int num -> Int (op num)
        | other -> failwith $"Operator {op} expected 'Int' type; instead got: {other}"
        
    let abs = unaryIntOperation System.Numerics.BigInteger.Abs
    let negate = unaryIntOperation System.Numerics.BigInteger.Negate

    let mapOperator = function
        | Abs       -> abs
        | Negate       -> negate