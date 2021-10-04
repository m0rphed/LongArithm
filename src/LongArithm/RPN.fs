module LongArithm.RPN

open MathParser

let private operatorsMapping =
    [ (OperatorTypes.Add, (+))
      (OperatorTypes.Divide, (/))
      (OperatorTypes.Multiply, (*))
      (OperatorTypes.Subtract, (-)) ]
    |> Map.ofList


let calculateRPN rpnInput =
    let resStack: float list = []

    let handleOperator stack opType =
        match stack with
        | num1 :: num2 :: tail -> (operatorsMapping.[opType] num2 num1) :: tail
        | [ _ ]
        | [] -> failwith "Not enough operator arguments!"

    let rec nextToken rpnInput resStack =
        match rpnInput with
        | head :: tail ->
            match head with
            | RPNNumber num -> nextToken tail (num :: resStack)
            | RPNOperator (_opPriority, opType) -> nextToken tail (handleOperator resStack opType)
            | _ -> failwith "Invalid input token"

        | [] -> (rpnInput, resStack)

    match (nextToken rpnInput resStack) with
    | _input, output -> output.[0]