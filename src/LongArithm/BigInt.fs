module LongArithm.BigInt

open MyList2
type Sign =
    | Positive
    | Negative

[<Struct>]
type BigInt =
    val Sign: Sign
    val Digits: MyList<int>
    new (s, b) = {
        Sign = if b = Single 0 then Positive else s
        Digits =
            if (MyList.fold (fun _ i -> i >= 0 && i <= 9) true b)
            then b
            else failwith "Digits should be in range 0..9"
        }

let equal (x: BigInt) (y: BigInt) =
    if x.Sign <> y.Sign || MyList.length x.Digits <> MyList.length y.Digits
    then false
    else MyList.map2 (=) x.Digits y.Digits |> MyList.fold (&&) true

let getSign (x: BigInt) =
    if x.Sign = Positive then 1 else -1

let setSign x =
    if x = 1 || x = 0 then Positive elif x = -1 then Negative else failwith "1, 0 or -1 expected"

let reverseSign (x: BigInt) =
    BigInt((if x.Sign = Positive then Negative else Positive), x.Digits)

let equalize (x, y) = // добавляет нули в начало одного из списков, пока их длина разная
    let rec go x y dif =
        if dif = 0 then (x, y) elif dif < 0 then go (Nodes(0, x)) y (dif + 1) else go x (Nodes(0, y)) (dif - 1)

    let dif = MyList.length x - MyList.length y
    go x y dif

let rec delZeroHead l = // удаляет все нули из префикса списка
    match l with
    | Single _ -> l
    | Nodes(h, tail) -> if h = 0 then delZeroHead tail else l

let rec addZeroes c l =
    if c <= 0 then l else addZeroes (c - 1) (Nodes(0, l))

let notLesser x y = // возвращает true, если x >= y в лексикографическом порядке
    let lx = MyList.length x
    let ly = MyList.length y
    if lx <> ly
    then lx > ly
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
                | Nodes (y1, taily) -> if x1 = y1 then go tailx taily else x1 >= y1

        go x y

let rec private manageRemainders = function  // Проходит по списку, перекидывая лишнее на следующий разряд
    | Single x ->
        if x < 10 then Single x else manageRemainders (Nodes (x % 10, Single (x / 10)))
    | Nodes (head, tail) ->
        let remainder, folded = MyList.fold (fun (r, res) x ->  
            let y = x + r
            if y >= 0
            then (y / 10, Nodes (y % 10, res))
            else (-1, Nodes (10 + y, res))) (if head >= 0 then (head / 10, Single (head % 10)) else (-1, Single (10 + head))) tail
        delZeroHead (Nodes(remainder, folded))

let sumOrSub (x:BigInt) (y:BigInt) operator =
    let xEq, yEq = equalize (x.Digits, y.Digits)        // Уровняли списки по длине
    let mapped = MyList.map2 (fun x1 y1 -> operator (getSign x * x1) (getSign y * y1)) xEq yEq |> delZeroHead |> MyList.reverse     // Сложили/вычли поразрядно и развернули список
    let result = manageRemainders mapped
    BigInt (setSign (sign (MyList.head result)), result)

let sum (x: BigInt) (y: BigInt) =         // Если вычитаемое больше уменьшаемого по модулю, разность "в столбик" не работает
    match x.Sign, y.Sign with           // Поэтому приходится делать проверку на модуль и знак
    | Positive, Positive -> sumOrSub x y (+)
    | Negative, Negative ->  reverseSign (sumOrSub (reverseSign x) (reverseSign y) (+))
    | Positive, Negative when notLesser x.Digits y.Digits -> sumOrSub x y (+)
    | Positive, Negative -> reverseSign (sumOrSub (reverseSign y) x (-))
    | Negative, Positive when notLesser x.Digits y.Digits -> reverseSign (sumOrSub (reverseSign x) y (-))
    | Negative, Positive -> sumOrSub y x (+)

let sub (x: BigInt) (y: BigInt) = sum x (reverseSign y)

let mul (x: BigInt) (y: BigInt) =
    let result, _ = MyList.fold (fun (r, rank) y ->
        let mapped = MyList.map (fun x1 -> x1 * y) x.Digits |> delZeroHead |> MyList.reverse |> addZeroes rank  // Умножаем поразрядно, добавляя нули в конец числа в соответствии с разрядом множителя
        let newR = manageRemainders mapped
        (sum r (BigInt(Positive, newR)), rank + 1)) (BigInt(Positive, Single 0), 0) (MyList.reverse y.Digits)

    BigInt(setSign(getSign x * getSign y), result.Digits)

let divOrRem (x:BigInt) (y:BigInt) =
    let divide x y =            // Находит частное(от 0 до 9) и остаток от деления.
        let mutable down = 1    // Применяется только если длина делимого равна или больше на 1, чем у делителя
        let mutable up = 10
        while up - down > 1 do
            let r = BigInt(Positive, intToMyList ((up + down) / 2))
            let f = (mul (BigInt(Positive, y)) r)
            if notLesser x f.Digits
            then down <- ((up + down) / 2)
            else up <- ((up + down) / 2)
        let quot = (up + down) / 2
        let quotXres = mul (BigInt(Positive, y)) (BigInt(Positive, Single quot))
        let remainder = sub (BigInt(Positive, x)) quotXres
        (quot, remainder.Digits)

    let rSign = setSign (getSign x * getSign y)
    if y.Digits = Single 0 then failwith "Division by zero"
    else 
        let divisorLen = MyList.length y.Digits
        let rem, res, _, c = MyList.fold (fun (dividend, result, divisorLen, c) x1 ->    // Отрезаем от делимого числа до тех пор, пока не получится...
            let newC = c + 1                                                    // ...использовать divide и добавляем нули, если было занято более 1 разряда за раз
            let newRes = if newC >= 2 then Nodes (0, result) else result
            let newDividend = MyList.concat dividend (Single x1) |> delZeroHead
            if MyList.length newDividend > divisorLen || (MyList.length newDividend = divisorLen && notLesser newDividend y.Digits)
            then
                let m, rem = divide newDividend y.Digits
                (rem, Nodes (m, newRes), divisorLen, 0)
            else (newDividend, newRes, divisorLen, c + 1)) (Single 0, Single 0, divisorLen, 0) x.Digits
        let newRes = addZeroes (if c > 0 then 1 else 0) res |> MyList.reverse |> delZeroHead    // Если после последнего divide были заняты ещё разряды, необходимо добавить 0 в результат
        (rem, newRes)

let div (x: BigInt) (y: BigInt) =
    let rSign = setSign(getSign x * getSign y)
    let res = snd (divOrRem x y)
    BigInt (rSign, res)

let rem (x: BigInt) (y: BigInt) =
    let res = fst (divOrRem x y)
    BigInt (x.Sign, res)

let power (n: BigInt) (pow: BigInt) =
    let rec go r (p:BigInt) =
        match p.Digits with
        | Single 0 -> BigInt(Positive, Single 1)
        | Single 1 -> r
        | _ ->
            let rm, dv = divOrRem p (BigInt(Positive, Single 2))
            let div = BigInt(Positive, dv)
            let nr = go r div
            if rm = Single 0 then mul nr nr else mul n (mul nr nr) 

    if pow.Sign = Negative then failwith "Positive power expected"
    else go n pow

let toBinary (x:BigInt) =
    let rec go l r =
        match l with
        | Single 0 -> r
        | _ ->
            let rem, divd = divOrRem (BigInt(Positive, l)) (BigInt(Positive, Single 2))
            go divd (Nodes (MyList.head rem, r))

    let rem, divd = divOrRem (BigInt(Positive, x.Digits)) (BigInt(Positive, Single 2))
    BigInt(x.Sign, go divd (Single(MyList.head rem)))

let stringToBigInt (n:string) =
    let s = if n.[0] = '-' then Negative else Positive
    let l = n |> List.ofSeq |> List.map string
    let ml = (if l.[0] = "+" || l.[0] = "-" then l.[1..] else l) |> List.map int |> MyList.fromList
    BigInt (s, ml)

let bigIntToString (n:BigInt) =
    let r = n.Digits |> MyList.fold (fun acc x -> acc + string x) ""
    if n.Sign = Negative then "-" + r else r

let abs (x:BigInt) = BigInt(Positive, x.Digits)