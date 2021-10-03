module LongArithm.Cli.Main

open LongArithm
open BigInt

let calcTest (input: string) =
    printfn $"Input: %s{input}"
    printfn $"ToPostfix: %A{Parser.parseInfixNotation input}"
    printfn $"=> Result: %A{RPN.calculateRPN (Parser.parseInfixNotation input)}\n"

[<EntryPoint>]
let main argv =
    printfn "GO!!!\n"
    let data = ["(1 + 1) * 2" ; "1 + 1 * 2"]
    for x in data do
        calcTest x
        
    let bigInt1 = "228" |> stringToBigInt
    let bigInt2 = "-1337" |> stringToBigInt
    
    printfn $"Sum: {sum bigInt1 bigInt2 |> bigIntToString}"
    printfn $"Sub: {sub bigInt1 bigInt2 |> bigIntToString}"
    printfn $"Mul: {mul bigInt1 bigInt2 |> bigIntToString}"
    printfn $"Div: {div bigInt1 bigInt2 |> bigIntToString}"
        
    0 // return an integer exit code
