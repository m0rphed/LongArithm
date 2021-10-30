namespace Tests

module ``BigInt Tests`` =
    open Xunit
    open FsUnit.Xunit
    open LongArithm
    open System

    let a1 = 33343435L
    let a2 = 5065657577L
    let b1 = "33343435" |> BigInt.parseBigInt
    let b2 = "5065657577" |> BigInt.parseBigInt
    
    let stopWatch = System.Diagnostics.Stopwatch()

    [<Fact>]
    let ``Sum test`` () =
         (a1 + a2).ToString() |> should equal ((BigInt.sum b1 b2).ToString())

    [<Fact>]
    let ``Mul test`` () =
        stopWatch.Restart()
        (a1 * a2).ToString() |> should equal ((BigInt.mul b1 b2).ToString())
        Console.ForegroundColor <- ConsoleColor.Yellow
        printfn $"Simple: {stopWatch.ElapsedTicks}"
        Console.ResetColor()

    [<Fact>]
    let ``Sub test`` () =
        (a1 - a2).ToString() |> should equal
            ((BigInt.sub b1 b2).ToString())
    
    [<Fact>]
    let ``Div test`` () =
        (a1 / a2).ToString() |> should equal
            ((BigInt.div b1 b2).ToString())