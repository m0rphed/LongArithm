module LongArithm.MyList2

/// Generic List implementation "from scratch";
/// `MyList` must be nonempty (always contains a value)
type MyList<'T> =
    | Single of 'T
    | Nodes of value: 'T * next: MyList<'T>

/// Implementation of std. list function for MyList
module MyList =
    let head = function
        | Single x -> x
        | Nodes (x, _) -> x

    let tail = function
        | Single _ -> failwith "There's no tail"
        | Nodes (_, tail) -> tail

    let rec iter func list =
        match list with
        | Single v -> func v
        | Nodes (v, nextNode) ->
            func v
            nextNode |> iter func

    /// Map implemented for `MyList`
    let rec map func list =
        match list with
        | Single v -> Single(func v)
        | Nodes (v, nextNode) -> Nodes(func v, map func nextNode)

    /// Fold implemented for MyList
    let rec fold folder acc list =
        match list with
        | Single v -> folder acc v
        | Nodes (v, nextNode) ->
            nextNode
            |> fold folder (folder acc v)

    /// Returns length of the list
    let length list = fold (fun acc _elem -> acc + 1) 0 list

    /// Combines two lists of the same type into one
    let rec concat listA listB =
        match listA with
        | Single v -> Nodes(v, listB)
        | Nodes (v, nextNode) -> Nodes(v, concat nextNode listB)

    /// Converts from list to nonempty MyList
    let rec fromList list =
        match list with
        | [ v ] -> Single v
        | head :: tail -> Nodes(head, fromList tail)
        | [] -> failwith "Could not construct nonempty MyList from an empty std. list"

    /// Converts MyList to nonempty list
    let rec toList myList =
        match myList with
        | Single v -> [ v ]
        | Nodes (v, nextNode) ->
            nextNode
            |> fold (fun listAcc currentElement -> listAcc @ [ currentElement ]) [ v ]


    /// Quicksort implemented for MyList
    let qsort comparer myList =
        let rec sort list =
            match list with
            | Single theTail -> Single theTail
            | Nodes (v, nextNode) ->
                let previous = v

                match nextNode with
                | Nodes (nodeValue, nodeAfterThat) ->
                    let current = nodeValue
                    let tail = nodeAfterThat

                    if comparer previous current
                    then Nodes(current, sort (Nodes(previous, tail)))
                    else Nodes(previous, sort (Nodes(current, tail)))

                | Single current ->
                    if comparer previous current then
                        Nodes(current, Single previous)
                    else
                        Nodes(previous, Single current)

        let mutable res = myList
        for i = 0 to (length myList) - 1 do
            res <- sort res

        res
        
    let reverse myList =
        match myList with
        | Single _ -> myList
        | Nodes (head, tail) ->
            fold (fun rest v -> Nodes(v, rest)) (Single head) tail
            
    let map2 mapping (list1:MyList<'t>) (list2:MyList<'t>) =
        let rec go mapping (x:MyList<'t>) (y:MyList<'t>) =
            match x with
            | Single x1 ->
                match y with
                | Single y1 -> Single (mapping x1 y1)
                | Nodes _ -> failwith "Impossible case"
            | Nodes (x1, tailx) ->
                match y with
                | Single _ -> failwith "Impossible case"
                | Nodes (y1, taily) -> Nodes (mapping x1 y1, go mapping tailx taily)

        
        if length list1 = length list2
        then
            match list1 with
            | Single x1 ->
                match list2 with
                | Single y1 -> Nodes (mapping x1 y1)
                | Nodes _ -> failwith "Impossible case"
            | Cons(x1, tailx) ->
                match list2 with
                | One _ -> failwith "Impossible case"
                | Cons(y1, taily) -> go mapping list1 list2
        else failwith "Length of lists should be equal"