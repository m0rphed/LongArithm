module LongArithm.MyList
(*  This is naive "home-grown" implementation of functional list data structure,
    so the name is also "naive" and kinda dumb but it's a part of the task anyway :)    
*)


/// Generic List implementation "from scratch";
/// `MyList` must be nonempty (always contains a value)
type MyList<'T> =
    | Single of 'T
    | Nodes of value: 'T * next: MyList<'T>

/// Implementation of std. list function for MyList
module MyList =
    /// Returns list head
    let head = function
        | Single v -> v
        | Nodes (v, _) -> v

    /// Returns list tail
    let tail = function
        | Single _ -> failwith "There's no tail"
        | Nodes (_, tail) -> tail

    /// Applies the given function to each element of the list
    let rec iter action list =
        match list with
        | Single v -> action v
        | Nodes (v, nextNode) ->
            action v
            nextNode |> iter action

    /// Returns a new list containing
    /// only the elements of the collection for which the given predicate returns "true";
    /// fails if no elements satisfying predicate were found
    let rec filter condition list =
        match list with
        | Single v as myList ->
            if not (condition v)
            then failwith "List can not be empty"
            else myList 
        | Nodes (v, nextNode) ->
            if condition v
            then Nodes (v, (filter condition nextNode))
            else filter condition nextNode

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

    /// Returns reversed MyList
    let reverse list =
        match list with
        | Single _ -> list
        | Nodes (head, tail) ->
            fold (fun rest v -> Nodes(v, rest)) (Single head) tail

    /// Builds a new MyList whose elements are the results of applying
    /// the given function to the corresponding elements of the two MyList-s pairwise.
    let rec map2 mapping myListA myListB =
        match myListA, myListB with
        | Single a, Single b -> Single (mapping a b)
        | Nodes (a, tailA), Nodes (b, tailB) -> Nodes (mapping a b, map2 mapping tailA tailB)
        | _ -> invalidArg "myListB" "Error [map2]: input lists differ in length."
    
    /// Returns a new MyList that contains the elements
    /// of the first MyList followed
    /// by elements of second MyList.
    let rec concat myListA myListB =
        match myListA with
        | Single v -> Nodes(v, myListB)
        | Nodes (v, nextNode) -> Nodes(v, concat nextNode myListB)

    /// Converts list to nonempty MyList
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
