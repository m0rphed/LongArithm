namespace LongArithm.BigInt

open System
open System.Numerics
open LongArithm
open MyList

/// Contains functionality
/// indirectly related to big integer implementation, such as:
/// - system Int32, and BigInteger conversions
/// - definitions of `BigInt` exceptions
module BigIntUtils =
    /// Exception should be thrown when an attempt to apply
    /// an operator/operation on the `BigInt` type fails
    exception BigIntOperationError of string
    
    /// Exception should be thrown when parsing of `BigInt` fails
    exception BigIntParseError of string
    
    /// Checks if specified integer represents single digit (from 0 to 9)
    let isDigit n = n >= 0 && n <= 9
    
    /// Checks if specified `System.Numerics.BigInteger`
    /// represents single digits (from 0 to 9)
    let isDigit' (n: BigInteger) = n >= 0I && n <= 9I
    
    /// Checks that every integer value        
    /// represents a digit (0 <= INTEGER <= 9)
    let checkDigits myList =
        let mutable digitBuffer = ""
        let _msg = "is out of range (0 <= digits <= 9) in number"
        myList
        |> MyList.iter
            (fun n ->
                if isDigit n
                then digitBuffer <- digitBuffer + (string n)
                else BigIntParseError($"Digit: %i{n} {_msg}: %s{digitBuffer} <- %i{n}")
                    |> raise)

    let listOfDigits number =
        let rec loop n remainder =
            if n = 0 then remainder
            else loop (n / 10) (Nodes(n % 10, remainder))
        
        let number = abs number
        loop (number / 10) (number % 10 |> Single)
    
    let listOfDigits' bigNum =
        let rec loop n remainder =
            if n = 0I then remainder
            else loop (n / 10I) (Nodes(n % 10I, remainder))
        
        let number = BigInteger.Abs bigNum
        loop (number / 10I) (number % 10I |> Single)
    
    /// Converts character to digit (int);
    /// fails if character is not a digit
    let charToInt (ch: char) = Int32.Parse (string ch)
        
    /// Tries to convert character to digit (int)
    let tryConvertToInt ch =
        match Int32.TryParse(string ch) with
        | true, res -> res
        | false, _ -> BigIntParseError($"Expected digit, got: %c{ch}") |> raise
    
/// Representation of a sign
/// for a BigInt structure
type Sign =
    | Positive
    | Negative
    static member (*) (first, second) =
        match first, second with
        | Positive, Positive
        | Negative, Negative -> Positive
        | Negative, Positive 
        | Positive, Negative -> Negative
        
open BigIntUtils

/// Big Integer structure
/// the structure stores the sign and numbers separately 
[<Struct>]
type MyBigInt =
    val Sign: Sign
    val Digits: MyList<int>

    new(sign, digits) =
        let sign =
            if digits = Single 0
            then Positive else sign
        checkDigits digits
        { Sign = sign
          Digits = digits}
        
    override this.ToString() =
        let result =
            this.Digits
            |> MyList.fold (fun acc x -> acc + string x) ""

        if this.Sign = Negative
        then "-" + result
        else result

module BigInt =
    let createPositive number = (Positive, listOfDigits number) |> MyBigInt
    let createNegative number = (Negative, listOfDigits number) |> MyBigInt
    let positive digits = (Positive, digits) |> MyBigInt
    let negative digits = (Negative, digits) |> MyBigInt

    let bigIntOf n =
        if n < 0
        then createNegative n
        else createPositive n 
    
    let zero () = createPositive 0
    let one () = createPositive 1  
    let negOne () = createNegative 1 
    
    let (|Zero|One|NegativeOne|Number|) (n: MyBigInt) =
        match n.Sign, n.Digits with
        | Positive, Single 0 -> Zero
        | Positive, Single 1 -> One
        | Negative, Single 1 -> NegativeOne
        | _, _ -> Number
    
    let tryParseBigInt (input: string) =
        try
            let digits =
                input
                |> List.ofSeq
                |> List.map tryConvertToInt
                |> MyList.fromList
            digits
            |> positive
            |> Ok 
        with
        | BigIntParseError msg -> Error msg
    
    let parseBigInt (input: string) =
        let digits =
            input
            |> List.ofSeq
            |> List.map charToInt
            |> MyList.fromList

        positive digits

    let getSign (x: MyBigInt) =
        if x.Sign = Positive
        then 1
        else -1

    let setSign x =
        match x with
        | 0 | 1 -> Positive
        | -1    -> Negative
        | unexpected ->
            $"Expected: 1, 0 or -1; got: {unexpected} "
            |> BigIntOperationError
            |> raise

    let negate (x: MyBigInt) =
        match x with
        | Zero -> x
        | One -> negOne ()
        | NegativeOne -> one ()
        | Number ->
            let sign =
                match x.Sign with
                | Negative -> Positive
                | Positive -> Negative
            MyBigInt(sign, x.Digits)

    /// Adds zeros at the beginning
    /// of specified digit list until list's lengths differ
    let private equalize (x, y) = 
        let rec go x y lengthDiff =
            if lengthDiff = 0 then (x, y)
            elif lengthDiff < 0 then go (Nodes(0, x)) y (lengthDiff + 1)
            else go x (Nodes(0, y)) (lengthDiff - 1)

        let diff = MyList.length x - MyList.length y
        go x y diff

    /// Removes all zeros from the prefix
    /// of `BigInt` (repr. as list of digits)
    let rec private deleteZeroPrefix digits =
        match digits with
        | Single _ -> digits
        | Nodes (head, tail) ->
            if head = 0 then deleteZeroPrefix tail else digits

    /// Adds zeros at the beginning
    /// of specified BigInt (repr. as list of digits) 
    let rec private addZeros count myList =
        if count < 0 then invalidArg "count" "addZeroes counter could not be < 0"
        if count = 0 then myList
        else addZeros (count - 1) (Nodes(0, myList))

    /// Returns true if x >= y
    /// (x lexicographically greater than or equal to y)
    let private notLess x y =
        let lenX = MyList.length x
        let lenY = MyList.length y

        if lenX <> lenY then lenX > lenY
        else
            let rec go x y =
                match x, y with
                | Single _, Nodes _
                | Nodes _, Single _ ->
                    "Expected operands to be equal by length;"
                        + $"instead got: %A{x}; %A{y}"
                        |> BigIntOperationError
                        |> raise
                | Single headX, Single headY -> headX >= headY
                | Nodes (headX, tailX), Nodes (headY, tailY) ->
                    if headX = headY
                    then go tailX tailY
                    else headX >= headY
            go x y

    /// Goes through the list, pushing the excess to the next digit
    let rec private manageRemainders =
        function 
        | Single x ->
            if x < 10 then Single x
            else manageRemainders (Nodes(x % 10, Single(x / 10)))
        | Nodes (head, tail) ->
            let manageRem (rem, next) x =
                let y = x + rem
                if y >= 0 then (y / 10, Nodes(y % 10, next))
                else (-1, Nodes(10 + y, next))
            
            let acc =
                if head >= 0 then (head / 10, Single(head % 10))
                else (-1, Single(10 + head))
            
            let remainder, folded = MyList.fold manageRem acc tail
            deleteZeroPrefix (Nodes(remainder, folded))

    let private sumOrSub op (x: MyBigInt) (y: MyBigInt)  =
        // Уравняли списки по длине
        let xEq, yEq = equalize (x.Digits, y.Digits)

        let mapped =
            MyList.map2 (fun x1 y1 -> op (getSign x * x1) (getSign y * y1)) xEq yEq
            |> deleteZeroPrefix
            // Сложили/вычли поразрядно и развернули список
            |> MyList.reverse

        let result = manageRemainders mapped
        MyBigInt(setSign (sign (MyList.head result)), result)

    // Если вычитаемое больше уменьшаемого по модулю, разность "в столбик" не работает
    let sum (x: MyBigInt) (y: MyBigInt) =
        match x.Sign, y.Sign with
        // Поэтому приходится делать проверку на модуль и знак
        | Positive, Positive -> sumOrSub (+) x y 
        | Negative, Negative -> sumOrSub (+) (negate x) (negate y) |> negate
        | Positive, Negative when notLess x.Digits y.Digits -> sumOrSub (+) x y 
        | Positive, Negative -> sumOrSub (-) (negate y) x |> negate
        | Negative, Positive when notLess x.Digits y.Digits -> sumOrSub (-) (negate x) y |> negate
        | Negative, Positive -> sumOrSub (+) y x

    let sub (x: MyBigInt) (y: MyBigInt) = sum x (negate y)

    let mul (x: MyBigInt) (y: MyBigInt) =
        let acc = zero (), 0
        let result, _ =
            y.Digits
            |> MyList.reverse
            |> MyList.fold
                (fun (r, rank) y ->
                    // Умножаем поразрядно, добавляя нули в конец числа
                    // в соответствии с разрядом множителя
                    let mapped =
                        MyList.map (fun x1 -> x1 * y) x.Digits
                        |> deleteZeroPrefix
                        |> MyList.reverse
                        |> addZeros rank
                    let newRem = manageRemainders mapped
                    (sum r (positive newRem), rank + 1)
                )
                acc

        MyBigInt(x.Sign * y.Sign, result.Digits)

    let private divOrRem (x: MyBigInt) (y: MyBigInt) =
        // Находит частное(от 0 до 9) и остаток от деления.
        let divide x y =
            // Применяется только если длина
            // делимого равна или больше на 1, чем у делителя
            let mutable down = 1
            let mutable up = 10

            while up - down > 1 do
                let r = (up + down) / 2 |> createPositive
                let f = (mul (positive y) r)
                if notLess x f.Digits
                then down <- ((up + down) / 2)
                else up <- ((up + down) / 2)

            // quotient of division (quotient is integer)
            let quotient = (up + down) / 2
            let quotXRes =
                quotient
                |> Single
                |> positive
                |> mul (positive y) 

            let remainder = sub (positive x) quotXRes
            (quotient, remainder.Digits)

        if y.Digits = Single 0 then raise (BigIntOperationError "Division by zero")
        let divisorLen = MyList.length y.Digits
        let acc = (Single 0, Single 0, divisorLen, 0)
        let rem, res, _, c =
            x.Digits
            |> MyList.fold
                // Отрезаем от делимого числа до тех пор, пока не получится...
                (fun (dividend, result, divisorLen, c) x1 ->
                    // ...использовать divide и добавляем нули,
                    // если было занято более 1 разряда за раз
                    let newC = c + 1
                    let newRem =
                        if newC >= 2
                        then Nodes(0, result)
                        else result

                    let newDividend =
                        Single x1
                        |> MyList.concat dividend
                        |> deleteZeroPrefix
                    
                    let newDividendLength = MyList.length newDividend
                    
                    if  newDividendLength > divisorLen
                        || (newDividendLength = divisorLen
                        && notLess newDividend y.Digits) then
                            let m, rem = divide newDividend y.Digits
                            rem, Nodes(m, newRem), divisorLen, 0
                    else newDividend, newRem, divisorLen, c + 1
                ) acc

        let newRem =
            // Если после последнего divide были заняты ещё разряды,
            // необходимо добавить 0 в результат
            addZeros (if c > 0 then 1 else 0) res
            |> MyList.reverse
            |> deleteZeroPrefix
        (rem, newRem)

    let div (x: MyBigInt) (y: MyBigInt) =
        let result = snd (divOrRem x y)
        MyBigInt(x.Sign * y.Sign, result)

    let getMod (x: MyBigInt) (y: MyBigInt) =
        MyBigInt(x.Sign, fst (divOrRem x y))

    let power (number: MyBigInt) (exp: MyBigInt) =
        let rec go bigN (exp: MyBigInt) =
            match exp.Digits with
            | Single 0 -> one ()
            | Single 1 -> bigN
            | _else ->
                let remainder, dv =
                    Single 2
                    |> positive
                    |> divOrRem exp 
                
                let div = positive dv
                let newRem = go bigN div

                if remainder = Single 0
                then mul newRem newRem
                else mul bigN (mul newRem newRem)

        if exp.Sign = Positive
        then go number exp
        else
            "Negative exp. is not supported"
            |> BigIntOperationError
            |> raise 

    let toBinary (x: MyBigInt) =
        let rec go l r =
            match l with
            | Single 0 -> r
            | _else ->
                let rem, div =
                    Single 2
                    |> positive
                    |> divOrRem (positive l)
                go div (Nodes (MyList.head rem, r))

        let rem, div =
            Single 2
            |> positive
            |> divOrRem (positive x.Digits)

        let binaryN =
            rem
            |> MyList.head
            |> Single
            |> go div 
        
        MyBigInt(x.Sign, binaryN)

    let abs (x: MyBigInt) = positive x.Digits

    let equal (x: MyBigInt) (y: MyBigInt) =
        let signDiffers = x.Sign <> y.Sign
        let lengthDiffers =  MyList.length x.Digits <> MyList.length y.Digits
        match signDiffers || lengthDiffers with
        | true -> false
        | false ->
            y.Digits
            |> MyList.map2 (=) x.Digits
            |> MyList.fold (&&) true
            
    let notEqual (x: MyBigInt) (y: MyBigInt) = not (equal x y)
    
    /// Returns true if first greater than second
    let greater (a: MyBigInt) (b: MyBigInt) =
        let rec loop x y =
            match x, y with
            | Single a, Single b -> a > b
            | Single _, Nodes _ -> false         
            | Nodes _, Single _ -> true
            | Nodes (hd1, tail1), Nodes (hd2, tail2) ->
                let lengthX, lengthY = MyList.length x, MyList.length y
                match lengthX = lengthY with
                | false -> lengthX > lengthY
                | true -> if hd1 <> hd2 then hd1 > hd2 else (loop tail1 tail2)

        match a.Sign, b.Sign with
        | Negative, Positive -> false
        | Positive, Negative -> true
        | Positive, Positive -> loop a.Digits b.Digits
        | Negative, Negative -> not (loop a.Digits b.Digits)

    let less (a: MyBigInt) (b: MyBigInt) =
        not (greater a b)
        
    let greaterOrEqual (a: MyBigInt) (b: MyBigInt) =
        match equal a b with
        | true -> true
        | false -> greater a b
        
    let lessOrEqual (a: MyBigInt) (b: MyBigInt) =
        match equal a b with
        | true -> true
        | false -> less a b

// Extending BigInt...
open BigInt
// ...with operators >, <, =, !=, +, -, etc.
type MyBigInt with
    static member (+) (a, b) = sum a b
    static member (-) (a, b) = sub a b
    static member (%) (a, b) = getMod a b
    static member (*) (a, b) = mul a b
    static member (/) (a, b) = div a b
    static member op_Equality (a, b) = equal a b
    static member op_Inequality (a, b) = notEqual a b
    static member op_GreaterThan (a, b) = greater a b
    static member op_LessThan (a, b) = less a b
    static member op_GreaterThanOrEqual (a, b) = greaterOrEqual a b
    static member op_LessThanOrEqual (a, b) = lessOrEqual a b