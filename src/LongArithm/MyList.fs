module LongArithm.MyList

/// Generic List implementation "from scratch";
/// `MyList` must be nonempty (always contains a value)
type MyList<'T> =
    | Single of 'T
    | Nodes of value: 'T * next: MyList<'T>

/// Implementation of std. list function for MyList
module MyList =
    let head = function
        | Single v -> v
        | Nodes (v, _) -> v

    let tail = function
        | Single _ -> failwith "There's no tail"
        | Nodes (_, tail) -> tail

    let rec iter action list =
        match list with
        | Single v -> action v
        | Nodes (v, nextNode) ->
            action v
            nextNode |> iter action

    /// Map implemented for `MyList`
    let rec map mapping list =
        match list with
        | Single v -> Single(mapping v)
        | Nodes (v, nextNode) -> Nodes(mapping v, map mapping nextNode)

    /// Fold implemented for MyList
    let rec fold folder acc list =
        match list with
        | Single v -> folder acc v
        | Nodes (v, nextNode) ->
            nextNode
            |> fold folder (folder acc v)

    /// Returns length of the list
    let length list = fold (fun acc _elem -> acc + 1) 0 list

    let reverse myList =
        match myList with
        | Single _ -> myList
        | Nodes (head, tail) ->
            fold (fun rest v -> Nodes(v, rest)) (Single head) tail

    let rec map2 mapping myListA myListB =
        match myListA, myListB with
        | Single a, Single b -> Single (mapping a b)
        | Nodes (a, tailA), Nodes (b, tailB) -> Nodes (mapping a b, map2 mapping tailA tailB)
        | _ -> invalidArg "myListB" "Error [map2]: input lists differ in length."
    
    /// Combines two lists of the same type into one
    let rec concat myListA myListB =
        match myListA with
        | Single v -> Nodes(v, myListB)
        | Nodes (v, nextNode) -> Nodes(v, concat nextNode myListB)

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
                    if comparer previous current
                    then Nodes(current, Single previous)
                    else Nodes(previous, Single current)

        let mutable res = myList
        for i = 0 to (length myList) - 1 do
            res <- sort res
        res
