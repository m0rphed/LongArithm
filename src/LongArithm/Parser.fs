module LongArithm.Parser

open FParsec

type OperatorTypes =
    | Add
    | Subtract
    | Multiply
    | Divide

type ParenthesisTypes =
    | Left
    | Right

type ParserTokens =
    | RPNNumber of float
    | RPNOperator of int * OperatorTypes
    | RPNParenthesis of ParenthesisTypes

let operatorsMapping =
    [ ("ADD", RPNOperator(1, OperatorTypes.Add))
      ("SUBTRACT", RPNOperator(1, OperatorTypes.Subtract))
      ("MULTIPLY", RPNOperator(2, OperatorTypes.Multiply))
      ("DIVIDE", RPNOperator(2, OperatorTypes.Divide)) ]
    |> Map.ofList

let parenthesisMapping =
    [ ("LEFT", RPNParenthesis ParenthesisTypes.Left)
      ("RIGHT", RPNParenthesis ParenthesisTypes.Right) ]
    |> Map.ofList

let parseInfixNotation (input: string) =
    let parseOperator =
        choice [ stringReturn "+" operatorsMapping.["ADD"]
                 stringReturn "-" operatorsMapping.["SUBTRACT"]
                 stringReturn "*" operatorsMapping.["MULTIPLY"]
                 stringReturn "/" operatorsMapping.["DIVIDE"] ]

    let parseNumber = pfloat |>> RPNNumber

    let parseParenthesis =
        (stringReturn "(" parenthesisMapping.["LEFT"])
        <|> (stringReturn ")" parenthesisMapping.["RIGHT"])

    let emptyStack (output, stack) =
        let rec popStack output stack =
            match stack with
            | head :: tail -> popStack (head :: output) tail
            | [] -> (output, stack)

        match (popStack output stack) with
        | output, _stack -> output

    let rec handleOperator (operator: ParserTokens) (output: ParserTokens list) (stack: ParserTokens list) =
        match stack with
        | head :: tail ->
            match head with
            | RPNOperator (op2Priority, _op2Type) ->
                match operator with
                | RPNOperator (opPriority, _opType) ->
                    if op2Priority >= opPriority then
                        handleOperator operator (head :: output) tail
                    else
                        (output, operator :: stack)
                | _ -> failwith "Wrong operator type"
            | _ -> (output, operator :: stack)
        | [] -> (output, operator :: stack)

    let handleParenthesis (parenthesis: ParserTokens) (output: ParserTokens list) (stack: ParserTokens list) =
        let rec popOperator (output: ParserTokens list) (stack: ParserTokens list) =
            match stack with
            | head :: tail ->
                match head with
                | RPNParenthesis _ ->
                    if head = parenthesisMapping.["LEFT"] then
                        (output, tail)
                    else
                        failwith "Invalid operator in the queue"
                | _ -> popOperator (head :: output) tail
            | [] -> (output, stack)

        match parenthesis with
        | RPNParenthesis _ ->
            if (parenthesis = parenthesisMapping.["LEFT"]) then
                (output, parenthesis :: stack)
            else
                popOperator output stack
        | _ -> failwith "Wrong parenthesis type"

    let handleNumber number output stack = ((RPNNumber number) :: output, stack)

    let handleNextToken token (output, stack) =
        match token with
        | RPNNumber num -> handleNumber num output stack
        | RPNOperator _ -> handleOperator token output stack
        | RPNParenthesis _ -> handleParenthesis token output stack

    let rec nextToken input (output, stack) =
        match String.length input with
        | 0 -> (output, stack)
        | _ ->
            match (run
                       (spaces
                        >>. (parseNumber <|> parseOperator <|> parseParenthesis))
                       input) with
            | Failure (str, _err, _) -> failwith $"PARSER ERROR: %s{str}"
            | Success (res, _, position) ->
                (output, stack)
                |> handleNextToken res
                |> nextToken input.[int (position.Index)..]

    let output: ParserTokens list = []
    let stack: ParserTokens list = []

    nextToken input (output, stack)
    |> emptyStack
    |> List.rev