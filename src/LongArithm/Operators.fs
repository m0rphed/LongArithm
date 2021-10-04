namespace LongArithm.Interpreter

open LongArithm.Parser

module Operators =
    let intOperation op x y =
        match x, y with
        | Int a, Int b -> Int (op a b)
        | _ as xy -> failwith $"Operator expected 'Integer' type; instead got:{xy}"

    let add         = intOperation (+)
    let subtract    = intOperation (-)
    let modulus     = intOperation (%)
    let multiply    = intOperation (*)
    let divide      = intOperation (/)

    let compare comparison x y =
        match x, y with
        | Int a, Int b -> Bool (comparison a b)
        | _ as xy -> failwith $"Comparison expected 'Integer' type; instead got:{xy}"

    let junction f x y =
        match x, y with
        | Bool a, Bool b -> Bool (f a b)
        | _ as xy -> failwith $"Junction expected 'Bool' type; instead got:{xy}"

    let conjunction = junction (&&)
    let disjunction = junction (||)

    let gt  = compare (>)
    let lt  = compare (<)
    let gte = compare (>=)
    let lte = compare (<=)
    let eq  = compare (=)
    let neq = compare (<>)

    let mapOperator = function
        | Add       -> add
        | Sub  -> subtract
        | Mult  -> multiply
        | Div    -> divide
        | Mod   -> modulus
        | Gt        -> gt
        | Lt        -> lt
        | Gte       -> gte
        | Lte       -> lte
        | Eq    -> eq
        | Neq -> neq
        | And       -> conjunction
        | Or        -> disjunction
        | Not       -> failwith "Operator 'NOT' currently not implement :("
        | Sconcat -> failwith "Operator 'String Concat' currently not implement :("