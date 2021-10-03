module LongArithm.MyList

type MyList<'t> =
    | One of 't
    | Cons of 't * MyList<'t>

type MyString = MyList<char>

let head = function
    | One x -> x
    | Cons(x, _) -> x

let tail = function
    | One _ -> failwith "There's no tail"
    | Cons(_, tail) -> tail

let rec fold folder acc l  =
    match l with
    | One x -> folder acc x
    | Cons(x, tail) -> fold folder (folder acc x) tail

let len l =
    fold (fun i _ -> i + 1 ) 0 l

let rec concat l1 l2 =
    match l1 with
        | One last -> Cons(last, l2)
        | Cons(head, tail) -> Cons(head, concat tail l2)

let rec concatMyString (s1:MyString) (s2:MyString):MyString =
    concat s1 s2

let rec map mapping l =
    match l with
    | One el -> One (mapping el)
    | Cons(head, tail) -> Cons(mapping head, map mapping tail)

let rec iter action l =
    match l with
    | One el -> action el
    | Cons(head, tail) ->
        action head
        iter action tail    

let bubbleSort l =
    let rec go l =
        match l with
        | One el -> One el
        | Cons(head, One el) -> if head <= el then l else Cons(el, One head)
        | Cons(a, Cons(b, tail)) -> if a <= b then Cons(a, (go (Cons(b, tail)))) else Cons(b, (go (Cons(a, tail))))

    let rec go2 l c lenL =
        if c = lenL then l
        else go2 (go l) (c + 1) lenL

    go2 l 0 (len l)

let listToMyList l =
    let rec go l ml =
        match l with
        | [] -> ml
        | head :: tail -> go tail (Cons(head, ml))

    if List.isEmpty l
    then failwith "List should not be empty"
    else go (List.rev l).Tail (One l.[l.Length - 1])

let myListToList l =
    (fold (fun list x -> x :: list) [] l) |> List.rev

let stringToMyString s:MyString =
    listToMyList (Seq.toList s)

let myStringToString (s:MyString) =
    myListToList s |> List.toArray |> System.String |> string

let reverse l =
    match l with
    | One x -> l
    | Cons(h, tail) -> fold (fun rl x -> Cons(x, rl)) (One h) tail

let map2 mapping (x:MyList<'t>) (y:MyList<'t>) =
    let rec go mapping (x:MyList<'t>) (y:MyList<'t>) =
        match x with
        | One x1 ->
            match y with
            | One y1 -> One (mapping x1 y1)
            | Cons _ -> failwith "Impossible case"
        | Cons(x1, tailx) ->
            match y with
            | One _ -> failwith "Impossible case"
            | Cons(y1, taily) -> Cons(mapping x1 y1, go mapping tailx taily)

    if len x = len y
    then
        match x with
        | One x1 ->
            match y with
            | One y1 -> One(mapping x1 y1)
            | Cons _ -> failwith "Impossible case"
        | Cons(x1, tailx) ->
            match y with
            | One _ -> failwith "Impossible case"
            | Cons(y1, taily) -> go mapping x y
    else failwith "Length of lists should be equal"

let intToMyList i =
    let rec go i r =
        if i = 0 then r else go (i/10) (Cons(i%10, r))

    go (i/10) (One(i%10))
