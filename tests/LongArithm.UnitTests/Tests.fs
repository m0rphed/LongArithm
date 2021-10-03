module Tests
open Expecto
open LongArithm
open BigInt

let tests =
    testList "BigInt tests" [
        let a1 = 5
        let a2 = 10
        let b1 = "5" |> stringToBigInt
        let b2 = "10" |> stringToBigInt
        
        test "Sum test" {           
            Expect.equal ((a1 + a2).ToString()) ((sum b1 b2) |> bigIntToString) ""
        }
        test "Mul test" {           
            Expect.equal ((a1 * a2).ToString()) ((mul b1 b2) |> bigIntToString) ""
        }
        test "Sub test" {           
            Expect.equal ((a1 - a2).ToString()) ((sub b1 b2) |> bigIntToString) ""
        }
        test "Div test" {           
            Expect.equal ((a1 / a2).ToString()) ((div b1 b2) |> bigIntToString) ""
        }
        test "Pov test" {           
            Expect.equal ((float a1 ** float a2).ToString()) ((power b1 b2) |> bigIntToString) ""
        }
    ]
