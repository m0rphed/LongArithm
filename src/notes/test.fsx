#r "nuget: FParsec"

open FParsec

open System
open System.Numerics

type Numbers = BigInt of BigInteger

let testBigIntParsing strBigInt =
    let toInt (s: string) =
        match System.Numerics.BigInteger.TryParse(s) with
        | (true, n) -> preturn n
        | _ -> fail "Error parsing Big Int"

    let naturalNum =
        many1Chars digit >>= toInt <?> "natural number"

    match run naturalNum strBigInt with
    | Failure (msg, _, _) -> failwith msg
    | Success (res, _, _) -> res


let finallyABigInt = "1844674407370955161510" |> testBigIntParsing

printfn "Parsed Value: %A; Type: %A" (finallyABigInt) (finallyABigInt.GetType())
