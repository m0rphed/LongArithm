namespace Tests

module ``BigInt Tests`` =
    open Xunit
    open FsUnit.Xunit
    open LongArithm.BigInt
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
        printfn $"Simple multiplication: {stopWatch.ElapsedTicks} in ticks"
        Console.ResetColor()

    [<Fact>]
    let ``Sub test`` () =
        (a1 - a2).ToString() |> should equal
            ((BigInt.sub b1 b2).ToString())
    
    [<Fact>]
    let ``Div test`` () =
        (a1 / a2).ToString() |> should equal
            ((BigInt.div b1 b2).ToString())
            
module ``Interpreter BigInt Tests`` =
    open Xunit
    open FsUnit.Xunit
    open LongArithm.Interpreter.Runners
    
    [<Fact>]
    let ``11 ^ 22 -- 'A in power of B' operator test`` () =
        let program =
            "x = 3
            print (x)
            x = 11
            y = 22
            z = x ^ y
            print (z)"
        
        let res = run program
        "3" |> should equal (res.OutputBuffer.Dequeue())
        "81402749386839761113321" |> should equal (res.OutputBuffer.Dequeue())
        
    [<Fact>]
    let ``Sum test`` () =
        let program = "x = 1 + 2 + 3 + 10 \nprint (x == 16)"
        let res = run program
        "true" |> should equal (res.OutputBuffer.Dequeue())
    
    [<Fact>]
    let ``Mul test`` () =
        let program = "x = 5 * 3 * 10 \nprint (x == 150)"
        let res = run program
        "true" |> should equal (res.OutputBuffer.Dequeue())
    
    [<Fact>]    
    let ``Mul test 2`` () =
        let program = "x = 10 * 4 * 10 \nprint (x)"
        let res = run program
        "400" |> should equal (res.OutputBuffer.Dequeue())
        
    [<Fact>]    
    let ``Div test`` () =
        let program = "x = (10 * 4) / 10 \nprint (x)"
        let res = run program
        "4" |> should equal (res.OutputBuffer.Dequeue())