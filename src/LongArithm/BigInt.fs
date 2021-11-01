namespace LongArithm.BigInt

open System
open LongArithm
open MyList

[<AutoOpen>]
module BigIntUtils =
    exception BigIntError of string
    exception BigIntParseError of string
    
    let checkDigits myList =
        let mutable digitBuffer = ""
        let _msg = "is out of range (0 <= digits <= 9) in number"
        myList
        |> MyList.iter
            (fun digit ->
                if digit >= 0 && digit <= 9
                then digitBuffer <- digitBuffer + (string digit)
                else BigIntError($"Digit: %i{digit} {_msg}: %s{digitBuffer} <- %i{digit}")
                    |> raise)

type Sign =
    | Positive
    | Negative

[<Struct>]
type MyBigInt =
    val Sign: Sign
    val Digits: MyList<int>

    new(sign, digitsList) =
        let sign =
            if digitsList = Single 0
            then Positive else sign
        
        checkDigits digitsList
        { Sign = sign
          Digits = digitsList}
        
    override this.ToString() =
        let result =
            this.Digits
            |> MyList.fold (fun acc x -> acc + string x) ""

        if this.Sign = Negative
        then "-" + result
        else result

module BigInt =
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
    
    let parseBigInt (input: string) =
        let convert (ch: char) =
            Int32.Parse (string ch)
        
        let digits =
            input
            |> List.ofSeq
            |> List.map convert
            |> MyList.fromList

        MyBigInt(Positive, digits)
    
    let toMyList number =
        let rec go n remainder =
            if n = 0 then remainder
            else go (n / 10) (Nodes(n % 10, remainder))
        go (number / 10) (Single(number % 10))
        
    let systemBigIntToMyList (number: System.Numerics.BigInteger) =
        let rec go n remainder =
            if n = 0I then remainder
            else go (n / 10I) (Nodes(n % 10I, remainder))
        go (number / 10I) (Single(number % 10I))
        
    let big0 = (Positive, 0 |> toMyList) |> MyBigInt 
    let big1 = (Positive, 1 |> toMyList) |> MyBigInt
    let negBig1 = (Negative, 1 |> toMyList ) |> MyBigInt 

    let getSign (x: MyBigInt) = if x.Sign = Positive then 1 else -1

    let setSign x =
        if x = 1 || x = 0 then Positive
        elif x = -1 then Negative
        else failwith "1, 0 or -1 expected"

    let private negateSign (x: MyBigInt) =
        let sign =
            match x.Sign with
            | Negative -> Positive
            | Positive -> Negative
        
        MyBigInt(sign, x.Digits)

    /// Добавляет нули в начало одного из списков, пока их длина разная
    let equalize (x, y) = 
        let rec go x y lengthDiff =
            if lengthDiff = 0 then (x, y)
            elif lengthDiff < 0 then go (Nodes(0, x)) y (lengthDiff + 1)
            else go x (Nodes(0, y)) (lengthDiff - 1)

        let diff = MyList.length x - MyList.length y
        go x y diff

    /// Удаляет все нули из префикса списка
    let rec delZeroPrefix myList =
        match myList with
        | Single _ -> myList
        | Nodes (head, tail) ->
            if head = 0 then delZeroPrefix tail else myList

    let rec addZeroes count myList =
        if count < 0 then failwith "addZeroes counter could not be < 0"
        if count = 0 then myList
        else addZeroes (count - 1) (Nodes(0, myList))

    /// Возвращает true, если x >= y в лексикографическом порядке
    let notLesser x y =
        let lenX = MyList.length x
        let lenY = MyList.length y

        if lenX <> lenY then lenX > lenY
        else
            let rec go x y =
                match x, y with
                | Single _, Nodes _
                | Nodes _, Single _ -> failwith "Impossible case"
                | Single headX, Single headY -> headX >= headY
                | Nodes (headX, tailX), Nodes (headY, tailY) ->
                    if headX = headY then go tailX tailY else headX >= headY

            go x y

    /// Проходит по списку, перекидывая лишнее на следующий разряд
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
            delZeroPrefix (Nodes(remainder, folded))

    let sumOrSub (x: MyBigInt) (y: MyBigInt) operator =
        // Уравняли списки по длине
        let xEq, yEq = equalize (x.Digits, y.Digits)

        let mapped =
            MyList.map2 (fun x1 y1 -> operator (getSign x * x1) (getSign y * y1)) xEq yEq
            |> delZeroPrefix
            // Сложили/вычли поразрядно и развернули список
            |> MyList.reverse

        let result = manageRemainders mapped
        MyBigInt(setSign (sign (MyList.head result)), result)

    // Если вычитаемое больше уменьшаемого по модулю, разность "в столбик" не работает
    let sum (x: MyBigInt) (y: MyBigInt) =
        match x.Sign, y.Sign with
        // Поэтому приходится делать проверку на модуль и знак
        | Positive, Positive -> sumOrSub x y (+)
        | Negative, Negative -> sumOrSub (negateSign x) (negateSign y) (+) |> negateSign
        | Positive, Negative when notLesser x.Digits y.Digits -> sumOrSub x y (+)
        | Positive, Negative -> sumOrSub (negateSign y) x (-) |> negateSign
        | Negative, Positive when notLesser x.Digits y.Digits -> sumOrSub (negateSign x) y (-) |> negateSign
        | Negative, Positive -> sumOrSub y x (+)

    let sub (x: MyBigInt) (y: MyBigInt) = sum x (negateSign y)

    let mul (x: MyBigInt) (y: MyBigInt) =
        let acc = MyBigInt(Positive, Single 0), 0
        let result, _ =
            y.Digits
            |> MyList.reverse
            |> MyList.fold
                (fun (r, rank) y ->
                    // Умножаем поразрядно, добавляя нули в конец числа
                    // в соответствии с разрядом множителя
                    let mapped =
                        MyList.map (fun x1 -> x1 * y) x.Digits
                        |> delZeroPrefix
                        |> MyList.reverse
                        |> addZeroes rank

                    let newRes = manageRemainders mapped
                    (sum r (MyBigInt(Positive, newRes)), rank + 1)
                )
                acc

        MyBigInt(setSign (getSign x * getSign y), result.Digits)

    let divOrRem (x: MyBigInt) (y: MyBigInt) =
        let divide x y = // Находит частное(от 0 до 9) и остаток от деления.
            let mutable down = 1 // Применяется только если длина делимого равна или больше на 1, чем у делителя
            let mutable up = 10

            while up - down > 1 do
                let r = MyBigInt(Positive, toMyList ((up + down) / 2))
                let f = (mul (MyBigInt(Positive, y)) r)

                if notLesser x f.Digits
                then down <- ((up + down) / 2)
                else up <- ((up + down) / 2)

            // частное от деления (целая часть)
            let quotient = (up + down) / 2
            let quotXRes = mul (MyBigInt(Positive, y)) (MyBigInt(Positive, Single quotient))

            let remainder = sub (MyBigInt(Positive, x)) quotXRes
            (quotient, remainder.Digits)

        // let rSign = setSign (getSign x * getSign y) // todo: what is this?
        if y.Digits = Single 0 then raise (BigIntError "Division by zero")
        let divisorLen = MyList.length y.Digits
        let acc = (Single 0, Single 0, divisorLen, 0)
        let rem, res, _, c =
            x.Digits
            |> MyList.fold
                // Отрезаем от делимого числа до тех пор, пока не получится...
                (fun (dividend, result, divisorLen, c) x1 ->
                    // ...использовать divide и добавляем нули, если было занято более 1 разряда за раз
                    let newC = c + 1
                    let newRes =
                        if newC >= 2
                        then Nodes(0, result)
                        else result

                    let newDividend =
                        Single x1
                        |> MyList.concat dividend
                        |> delZeroPrefix
                    
                    let newDividendLength = MyList.length newDividend
                    if newDividendLength > divisorLen || (newDividendLength = divisorLen && notLesser newDividend y.Digits) then
                        let m, rem = divide newDividend y.Digits
                        rem, Nodes(m, newRes), divisorLen, 0
                    else newDividend, newRes, divisorLen, c + 1
                ) acc

        let newRes =
            // Если после последнего divide были заняты ещё разряды, необходимо добавить 0 в результат
            addZeroes (if c > 0 then 1 else 0) res
            |> MyList.reverse
            |> delZeroPrefix

        (rem, newRes)

    let div (x: MyBigInt) (y: MyBigInt) =
        let rSign = setSign (getSign x * getSign y)
        let res = snd (divOrRem x y)
        MyBigInt(rSign, res)

    let getMod (x: MyBigInt) (y: MyBigInt) =
        let res = fst (divOrRem x y)
        MyBigInt(x.Sign, res)

    let power (n: MyBigInt) (pow: MyBigInt) =
        let rec go r (p: MyBigInt) =
            match p.Digits with
            | Single 0 -> MyBigInt(Positive, Single 1)
            | Single 1 -> r
            | _ ->
                let reminder, dv = divOrRem p (MyBigInt(Positive, Single 2))
                let div = MyBigInt(Positive, dv)
                let nr = go r div

                if reminder = Single 0
                then mul nr nr
                else mul n (mul nr nr)

        if pow.Sign = Negative
        then failwith "Positive power expected"
        else go n pow

    let toBinary (x: MyBigInt) =
        let rec go l r =
            match l with
            | Single 0 -> r
            | _ ->
                let rem, div =
                    divOrRem (MyBigInt(Positive, l)) (MyBigInt(Positive, Single 2))
                go div (Nodes(MyList.head rem, r))

        let rem, div =
            divOrRem (MyBigInt(Positive, x.Digits)) (MyBigInt(Positive, Single 2))

        MyBigInt(x.Sign, go div (Single(MyList.head rem)))

    let abs (x: MyBigInt) = MyBigInt(Positive, x.Digits)

    let negate (x: MyBigInt) =
        match x.Sign with
        | Negative -> MyBigInt(Positive, x.Digits)
        | Positive when x.Digits = Single 0 -> MyBigInt(Positive, x.Digits)
        | Positive ->  MyBigInt(Negative, x.Digits)

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
    
    /// Выводит true если первое число больше второго
    let greater (a: MyBigInt) (b: MyBigInt) =
        let rec greater' (x: MyList<int>) (y: MyList<int>) =
            match (x, y) with
            | Single a, Single b -> a > b
            | Single _, Nodes _ -> false         
            | Nodes _, Single _ -> true
            | Nodes (hd1, tail1), Nodes (hd2, tail2) ->
                let lengthX, lengthY = MyList.length x, MyList.length y
                match lengthX = lengthY with
                | false -> lengthX > lengthY
                | true -> if hd1 <> hd2 then hd1 > hd2 else (greater' tail1 tail2)

        match a.Sign, b.Sign with
        | Negative, Positive -> false
        | Positive, Negative -> true
        | Positive, Positive -> greater' a.Digits b.Digits
        | Negative, Negative -> not (greater' a.Digits b.Digits)

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

open BigInt
type MyBigInt with
    static member (+) (a, b: MyBigInt) = sum a b
    static member (-) (a, b: MyBigInt) = sub a b
    static member (%) (a, b: MyBigInt) = getMod a b
    static member (*) (a, b: MyBigInt) = mul a b
    static member (/) (a, b: MyBigInt) = div a b
    static member op_Equality (a, b: MyBigInt) = equal a b
    static member op_Inequality (a, b: MyBigInt) = notEqual a b
    static member op_GreaterThan (a, b: MyBigInt) = greater a b
    static member op_LessThan (a, b: MyBigInt) = less a b
    static member op_GreaterThanOrEqual (a, b: MyBigInt) = greaterOrEqual a b
    static member op_LessThanOrEqual (a, b: MyBigInt) = lessOrEqual a b