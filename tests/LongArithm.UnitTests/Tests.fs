module Tests
open Expecto
open LongArithm
open BigInt

let tests =
    testSequenced <| testList "BigInt tests" [
        let a1 = 33343435L
        let a2 = 5065657577L
        let b1 = "33343435" |> parseBigInt
        let b2 = "5065657577" |> parseBigInt
        
        let stopWatch = System.Diagnostics.Stopwatch()
        test "Sum test" {           
            Expect.equal ((a1 + a2).ToString()) ((sum b1 b2).ToString()) ""
        }
        test "Karatsuba test" {
            stopWatch.Start()
            Expect.equal ((a1 * a2).ToString()) ((karatsuba b1 b2).ToString()) ""
            printfn $"Karatsuba: {stopWatch.ElapsedTicks}"
        }
        test "Mul test" {
            stopWatch.Restart()
            Expect.equal ((a1 * a2).ToString()) ((mul b1 b2).ToString()) ""
            printfn $"Simple: {stopWatch.ElapsedTicks}"
        }
        test "Sub test" {           
            Expect.equal ((a1 - a2).ToString()) ((sub b1 b2).ToString()) ""
        }
        test "Div test" {           
            Expect.equal ((a1 / a2).ToString()) ((div b1 b2).ToString()) ""
        }
//        test "Pov test" {           
//            Expect.equal ((float a1 ** float a2).ToString()) ((power b1 b2) |> bigIntToString) ""
//        }
    ]
