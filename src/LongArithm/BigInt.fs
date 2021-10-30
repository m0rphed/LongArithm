module LongArithm.BigInt

open System
open MyList
open LongArithm

type Sign =
    | Positive
    | Negative

let intToMyList number =
    let rec go n remainder =
        if n = 0 then remainder
        else go (n / 10) (Nodes(n % 10, remainder))
    go (number / 10) (Single(number % 10))

[<Struct>]
type MyBigInt =
    val Sign: Sign
    val Digits: MyList<int>

    new(s, b) =
        { Sign = if b = Single 0 then Positive else s
          Digits =
              if (MyList.fold (fun _ i -> i >= 0 && i <= 9) true b) then
                  b
              else
                  failwith "Digits should be in range 0..9" }
        
    override this.ToString() =
        let r =
            this.Digits |> MyList.fold (fun acc x -> acc + string x) ""

        if this.Sign = Negative then "-" + r else r

let big0 = MyBigInt (Positive, 0 |> intToMyList)
let big1 = MyBigInt (Positive, 1 |> intToMyList)
let negBig1 = MyBigInt (Negative, 1 |> intToMyList )

let equal (x: MyBigInt) (y: MyBigInt) =
    if x.Sign <> y.Sign
       || MyList.length x.Digits <> MyList.length y.Digits then
        false
    else
        MyList.map2 (=) x.Digits y.Digits
        |> MyList.fold (&&) true
        
let notEqual (x: MyBigInt) (y: MyBigInt) = not (equal x y)

let getSign (x: MyBigInt) = if x.Sign = Positive then 1 else -1

let setSign x =
    if x = 1 || x = 0 then Positive
    elif x = -1 then Negative
    else failwith "1, 0 or -1 expected"

let reverseSign (x: MyBigInt) =
    let sign =
        match x.Sign with
        | Negative -> Positive
        | Positive -> Negative
    
    MyBigInt(sign, x.Digits)

let equalize (x, y) = // добавляет нули в начало одного из списков, пока их длина разная
    let rec go x y dif =
        if dif = 0 then
            (x, y)
        elif dif < 0 then
            go (Nodes(0, x)) y (dif + 1)
        else
            go x (Nodes(0, y)) (dif - 1)

    let dif = MyList.length x - MyList.length y
    go x y dif

let rec delZeroHead l = // удаляет все нули из префикса списка
    match l with
    | Single _ -> l
    | Nodes (h, tail) -> if h = 0 then delZeroHead tail else l

let rec addZeroes c l =
    if c <= 0 then
        l
    else
        addZeroes (c - 1) (Nodes(0, l))

let notLesser x y = // возвращает true, если x >= y в лексикографическом порядке
    let lx = MyList.length x
    let ly = MyList.length y

    if lx <> ly then
        lx > ly
    else
        let rec go x y =
            match x with
            | Single x1 ->
                match y with
                | Single y1 -> x1 >= y1
                | Nodes _ -> failwith "Impossible case"
            | Nodes (x1, tailx) ->
                match y with
                | Single _ -> failwith "Impossible case"
                | Nodes (y1, taily) ->
                    if x1 = y1 then
                        go tailx taily
                    else
                        x1 >= y1

        go x y

let rec private manageRemainders =
    function // Проходит по списку, перекидывая лишнее на следующий разряд
    | Single x ->
        if x < 10 then
            Single x
        else
            manageRemainders (Nodes(x % 10, Single(x / 10)))
    | Nodes (head, tail) ->
        let remainder, folded =
            MyList.fold
                (fun (r, res) x ->
                    let y = x + r

                    if y >= 0 then
                        (y / 10, Nodes(y % 10, res))
                    else
                        (-1, Nodes(10 + y, res)))
                (if head >= 0 then
                     (head / 10, Single(head % 10))
                 else
                     (-1, Single(10 + head)))
                tail

        delZeroHead (Nodes(remainder, folded))

let sumOrSub (x: MyBigInt) (y: MyBigInt) operator =
    let xEq, yEq = equalize (x.Digits, y.Digits) // Уровняли списки по длине

    let mapped =
        MyList.map2 (fun x1 y1 -> operator (getSign x * x1) (getSign y * y1)) xEq yEq
        |> delZeroHead
        |> MyList.reverse // Сложили/вычли поразрядно и развернули список

    let result = manageRemainders mapped
    MyBigInt(setSign (sign (MyList.head result)), result)

let sum (x: MyBigInt) (y: MyBigInt) = // Если вычитаемое больше уменьшаемого по модулю, разность "в столбик" не работает
    match x.Sign, y.Sign with // Поэтому приходится делать проверку на модуль и знак
    | Positive, Positive -> sumOrSub x y (+)
    | Negative, Negative -> reverseSign (sumOrSub (reverseSign x) (reverseSign y) (+))
    | Positive, Negative when notLesser x.Digits y.Digits -> sumOrSub x y (+)
    | Positive, Negative -> reverseSign (sumOrSub (reverseSign y) x (-))
    | Negative, Positive when notLesser x.Digits y.Digits -> reverseSign (sumOrSub (reverseSign x) y (-))
    | Negative, Positive -> sumOrSub y x (+)

let sub (x: MyBigInt) (y: MyBigInt) = sum x (reverseSign y)


// <Karatsuba>
let zeroPad (number: MyBigInt) zeros left =
    let mutable digits = number.Digits
    for i = 1 to zeros do
        if left
        then digits <- MyList.concat (Single 0) digits
        else digits <- MyList.concat digits (Single 0)
    MyBigInt (number.Sign, digits)

let rec karatsuba (x: MyBigInt) (y: MyBigInt) =    
    let helper (x: MyBigInt) (y: MyBigInt) =
        let n = MyList.length x.Digits
        let mutable j = n / 2
        if n % 2 <> 0
        then j <- j + 1
        let BZeroPadding = n - j
        let AZeroPadding = BZeroPadding * 2
        let a = MyBigInt (Positive, (x.Digits |> MyList.toList).[..j - 1] |> MyList.fromList)
        let b = MyBigInt (Positive, (x.Digits |> MyList.toList).[j..] |> MyList.fromList)
        let c = MyBigInt (Positive, (y.Digits |> MyList.toList).[..j - 1] |> MyList.fromList)
        let d = MyBigInt (Positive, (y.Digits |> MyList.toList).[j..] |> MyList.fromList)
        let ac = karatsuba a c
        let bd = karatsuba b d
        let k = karatsuba (sum a b) (sum c d)
        let A = zeroPad ac AZeroPadding false
        let subtraction = sub (sub k ac) bd
        let B = zeroPad subtraction BZeroPadding false
        sum (sum A B) bd
   
    match MyList.length(x.Digits), MyList.length(y.Digits) with
    | 1, 1 -> MyBigInt(setSign (getSign x * getSign y),
                       (MyList.head x.Digits * MyList.head y.Digits) |> intToMyList)
    | _, _ when MyList.length x.Digits < MyList.length y.Digits ->
        let x = zeroPad x (MyList.length y.Digits - MyList.length x.Digits) true
        let resWOSign = helper x y
        MyBigInt(setSign (getSign x * getSign y), resWOSign.Digits)
    | _, _ when MyList.length x.Digits > MyList.length y.Digits ->
        let y = zeroPad y (MyList.length x.Digits - MyList.length y.Digits) true
        let resWOSign = helper x y
        MyBigInt(setSign (getSign x * getSign y), resWOSign.Digits)
    | _, _ ->
        let resWOSign = helper x y
        MyBigInt(setSign (getSign x * getSign y), resWOSign.Digits)    
// </Karatsuba>


let mul (x: MyBigInt) (y: MyBigInt) =
    let result, _ =
        MyList.fold
            (fun (r, rank) y ->
                let mapped =
                    MyList.map (fun x1 -> x1 * y) x.Digits
                    |> delZeroHead
                    |> MyList.reverse
                    |> addZeroes rank // Умножаем поразрядно, добавляя нули в конец числа в соответствии с разрядом множителя

                let newR = manageRemainders mapped
                (sum r (MyBigInt(Positive, newR)), rank + 1))
            (MyBigInt(Positive, Single 0), 0)
            (MyList.reverse y.Digits)

    MyBigInt(setSign (getSign x * getSign y), result.Digits)

let divOrRem (x: MyBigInt) (y: MyBigInt) =
    let divide x y = // Находит частное(от 0 до 9) и остаток от деления.
        let mutable down = 1 // Применяется только если длина делимого равна или больше на 1, чем у делителя
        let mutable up = 10

        while up - down > 1 do
            let r =
                MyBigInt(Positive, intToMyList ((up + down) / 2))

            let f = (mul (MyBigInt(Positive, y)) r)

            if notLesser x f.Digits
            then down <- ((up + down) / 2)
            else up <- ((up + down) / 2)

        let quot = (up + down) / 2

        let quotXRes =
            mul (MyBigInt(Positive, y))
                (MyBigInt(Positive, Single quot))

        let remainder = sub (MyBigInt(Positive, x)) quotXRes
        (quot, remainder.Digits)

    let rSign = setSign (getSign x * getSign y)

    if y.Digits = Single 0 then
        failwith "Division by zero"
    else
        let divisorLen = MyList.length y.Digits

        let rem, res, _, c =
            MyList.fold
                (fun (dividend, result, divisorLen, c) x1 -> // Отрезаем от делимого числа до тех пор, пока не получится...
                    let newC = c + 1 // ...использовать divide и добавляем нули, если было занято более 1 разряда за раз

                    let newRes =
                        if newC >= 2
                        then Nodes(0, result)
                        else result

                    let newDividend =
                        MyList.concat dividend (Single x1) |> delZeroHead

                    if MyList.length newDividend > divisorLen
                       || (MyList.length newDividend = divisorLen
                           && notLesser newDividend y.Digits) then
                        let m, rem = divide newDividend y.Digits
                        (rem, Nodes(m, newRes), divisorLen, 0)
                    else (newDividend, newRes, divisorLen, c + 1))
                (Single 0, Single 0, divisorLen, 0)
                x.Digits

        let newRes =
            addZeroes (if c > 0 then 1 else 0) res
            |> MyList.reverse
            |> delZeroHead // Если после последнего divide были заняты ещё разряды, необходимо добавить 0 в результат

        (rem, newRes)

let div (x: MyBigInt) (y: MyBigInt) =
    let rSign = setSign (getSign x * getSign y)
    let res = snd (divOrRem x y)
    MyBigInt(rSign, res)

let rem (x: MyBigInt) (y: MyBigInt) =
    let res = fst (divOrRem x y)
    MyBigInt(x.Sign, res)

let power (n: MyBigInt) (pow: MyBigInt) =
    let rec go r (p: MyBigInt) =
        match p.Digits with
        | Single 0 -> MyBigInt(Positive, Single 1)
        | Single 1 -> r
        | _ ->
            let rm, dv = divOrRem p (MyBigInt(Positive, Single 2))
            let div = MyBigInt(Positive, dv)
            let nr = go r div

            if rm = Single 0 then
                mul nr nr
            else
                mul n (mul nr nr)

    if pow.Sign = Negative then
        failwith "Positive power expected"
    else
        go n pow

let toBinary (x: MyBigInt) =
    let rec go l r =
        match l with
        | Single 0 -> r
        | _ ->
            let rem, divd =
                divOrRem (MyBigInt(Positive, l)) (MyBigInt(Positive, Single 2))

            go divd (Nodes(MyList.head rem, r))

    let rem, divd =
        divOrRem (MyBigInt(Positive, x.Digits)) (MyBigInt(Positive, Single 2))

    MyBigInt(x.Sign, go divd (Single(MyList.head rem)))

let parseBigInt (input: string) =
    let convert (ch: char) =
        Int32.Parse (string ch)
    
    let digits =
        input
        |> List.ofSeq
        |> List.map convert
        |> MyList.fromList

    MyBigInt(Positive, digits)

exception BigIntParseError of string

let tryParseBigInt (input: string) =
    let tryConvert ch =
        match Int32.TryParse(string ch) with
        | true, res -> res
        | false, _ -> BigIntParseError($"Expected digit, got: %c{ch}") |> raise
    try
        let digits =
            input
            |> List.ofSeq
            |> List.map tryConvert
            |> MyList.fromList
        true, MyBigInt(Positive, digits)
    with
    | BigIntParseError _ ->
        false, MyBigInt(Positive, Single 0)

let abs (x: MyBigInt) = MyBigInt(Positive, x.Digits)

let negate (x: MyBigInt) =
    match x.Sign with
    | Negative -> MyBigInt(Positive, x.Digits)
    | Positive when x.Digits = Single 0 -> MyBigInt(Positive, x.Digits)
    | Positive ->  MyBigInt(Negative, x.Digits)

/// выводит true если первое число больше второго
let rec private greater' (x: MyList<int>) (y: MyList<int>) =
    match (x, y) with
    | Single a, Single b -> a > b
    | Single _, Nodes _ -> false         
    | Nodes _, Single _ -> true
    | Nodes (hd1, tail1), Nodes (hd2, tail2) ->
        let lengthX, lengthY = MyList.length x, MyList.length y
        match lengthX = lengthY with
        | false -> lengthX > lengthY
        | true -> if hd1 <> hd2 then hd1 > hd2 else (greater' tail1 tail2)

let greater (a: MyBigInt) (b: MyBigInt) =
    match a.Sign, b.Sign with
    | Negative, Positive -> false
    | Positive, Negative -> true
    | Positive, Positive -> greater' a.Digits b.Digits
    | Negative, Negative ->
        not (greater' a.Digits b.Digits)

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

type MyBigInt with
    static member (+) (a, b: MyBigInt) = sum a b
    static member (-) (a, b: MyBigInt) = sub a b
    static member (%) (a, b: MyBigInt) = rem a b
    static member (*) (a, b: MyBigInt) = mul a b
    static member (/) (a, b: MyBigInt) = div a b
    static member op_Equality (a, b: MyBigInt) = equal a b
    static member op_Inequality (a, b: MyBigInt) = notEqual a b
    static member op_GreaterThan (a, b: MyBigInt) = greater a b
    static member op_LessThan (a, b: MyBigInt) = less a b
    static member op_GreaterThanOrEqual (a, b: MyBigInt) = greaterOrEqual a b
    static member op_LessThanOrEqual (a, b: MyBigInt) = lessOrEqual a b
