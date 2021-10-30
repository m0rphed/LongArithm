module LongArithm.Cli.Main

open LongArithm

// BigInt on List -- tests
open BigInt

let testBigInt () =
    let bigInt1 = "228" |> stringToBigInt
    let bigInt2 = "-1337" |> stringToBigInt
    
    printfn $"Sum: {sum bigInt1 bigInt2 |> bigIntToString}"
    printfn $"Sub: {sub bigInt1 bigInt2 |> bigIntToString}"
    printfn $"Mul: {mul bigInt1 bigInt2 |> bigIntToString}"
    printfn $"Div: {div bigInt1 bigInt2 |> bigIntToString}"

// New Parser -- tests
open LongArithm.Parser
open FParsec

let parseSourceFileTests () =
    let fpath = "/home/morph/Desktop/3_oct/PCParser/src/Interpreter/data/sample.txt"
    match runParserOnFile (many pStatement) () fpath System.Text.Encoding.UTF8 with
    | Success (result, _, _) -> result
    | Failure (error, _, _) -> failwith $"Error: %s{error}"
    

// New Interpreter -- tests
open LongArithm.Interpreter.Statements
open LongArithm.Interpreter.Types

let testRunStatements statements =
    let initialState = { VariableTable = [] }
    runStatements statements initialState

[<EntryPoint>]
let main argv =
    printfn "GO!!!\n"
    let ast = parseSourceFileTests ()
    printfn $"AST: %A{ast}"
    ast |> testRunStatements |> printfn "Res VTable as List: %A"
    0 // return an integer exit code
