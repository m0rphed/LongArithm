namespace LongArithm.Parser

[<AutoOpen>]
module Parsing =
    open FParsec
    open AST
    
    let pWord str = pstring str .>> spaces

    // A combinator that transforms a parser by requiring that it
    // be wrapped in parentheses
    let parens parser = between (pWord "(") (pWord ")") parser
    
    let pBool: Parser<Value, Unit> =
        pWord "true" <|> pWord "false"
        |>> function
            | "true" -> Bool true
            | "false" -> Bool false
            | _ -> failwith "Expected 'true' or 'false' boolean literal"

    // FParsec defines the pint32 parser.
    // We simply cast its result to an int
    // then construct an Integer Value from it
    let pBigInt: Parser<Value, Unit> =
        many1Satisfy isDigit
            |>> System.Numerics.BigInteger.Parse
            .>> spaces
            |>> Int

    let pStringLiteral: Parser<Value, Unit> =
        // This line returns a list of chars, which we have to
        // turn into a string before turning into a Str Value
        pchar '\"' >>. manyCharsTill anyChar (pchar '\"')
        |>> string |>> Str
        // Discard the spaces at the end
        .>> spaces
        
    let pValue: Parser<Value, Unit> =
        choice [
            pBigInt
            pStringLiteral
            pBool
        ]
        
    let test parser strInput =
        match run parser strInput with
        // Assuming your parser returns something
        // that can be printed. For our purposes,
        // %O is usually enough.
        | Success (result, _, _) -> printfn $"{result}"
        | Failure (error, _, _) -> printfn $"%s{error}"

    let pLiteral: Parser<Expr, Unit> = pValue |>> Literal

    let pIdentifier: Parser<string, Unit> =
        many1Satisfy2 System.Char.IsLetter System.Char.IsLetterOrDigit
        .>> spaces
    
    let pVariable = pIdentifier |>> Variable
    
    let intOperatorParser =
        OperatorPrecedenceParser<Expr, Unit, Unit>()

    let intExpr = intOperatorParser.ExpressionParser
    
    let intTerm = choice [
        pBigInt .>> spaces |>> Literal <|> pVariable
        parens intExpr
    ]
    
    // Assign the term parser we designed to the
    // OperatorPrecedenceParser instance
    do intOperatorParser.TermParser <- intTerm
    
    let createOperation op x y = BinaryOp (x, op, y)

    type OperatorDetails = {  Symbol: string;
                              Precedence: int;
                              Operator: BinOperator }

    let intOperators = [
        {Symbol = ">"; Precedence = 1; Operator = Gt}
        {Symbol = "<"; Precedence = 1; Operator = Lt}
        {Symbol = ">="; Precedence = 1; Operator = Gte}
        {Symbol = "<="; Precedence = 1; Operator = Lte}
        {Symbol = "=="; Precedence = 1; Operator = Eq}
        {Symbol = "!="; Precedence = 1; Operator = Neq}
        {Symbol = "+"; Precedence = 2; Operator = Add}
        {Symbol = "-"; Precedence = 2; Operator = Sub}
        {Symbol = "*"; Precedence = 3; Operator = Mult}
        {Symbol = "/"; Precedence = 3; Operator = Div}
        {Symbol = "%"; Precedence = 3; Operator = Mod}
    ]

    let addOperators (precedenceParser : OperatorPrecedenceParser<_,_,_>) operatorTable =
        operatorTable
        |> List.iter (fun details ->
            let operator =
              InfixOperator(
                  details.Symbol,
                  spaces,
                  details.Precedence,
                  Associativity.Left,
                  createOperation details.Operator
              )
            precedenceParser.AddOperator(operator))


    do addOperators intOperatorParser intOperators
    // Define similar structures for booleans and strings
    let boolOperatorParser = OperatorPrecedenceParser<Expr, Unit, Unit>()
    let boolExpr = boolOperatorParser.ExpressionParser
    
    let pBool': Parser<bool, Unit> =
        pstring "true" <|> pstring "false"
        |>> System.Boolean.Parse
    
    let pBoolValue = pBool' |>> Bool

    let boolTerm = choice [
        pBoolValue .>> spaces |>> Literal
        pVariable
        parens boolExpr
    ]
    
    boolOperatorParser.TermParser <- boolTerm

    let strOperatorParser = OperatorPrecedenceParser<Expr, Unit, Unit>()
    let strExpr = strOperatorParser.ExpressionParser
    // We want to make sure we can concatenate
    // non-string values with strings, so we
    // accept any literal or variable
    let strTerm = choice [
        pLiteral
        pVariable
        intExpr
        boolExpr
        parens strExpr 
    ]
    
    do strOperatorParser.TermParser <- strTerm
    
    let boolOperators = [
        {Symbol = "and"; Precedence = 2; Operator =  And}
        {Symbol = "or"; Precedence = 1; Operator = Or}
    ]

    let stringOperators = [
        {Symbol = "++"; Precedence = 1; Operator = StrConcat}
    ]

    do addOperators boolOperatorParser boolOperators
    do addOperators strOperatorParser stringOperators
    
    let pOperation = choice [
        intExpr
        boolExpr
        strExpr
    ]
    
    let pExpression = choice [
        pOperation
        pLiteral
        pVariable
    ]
    
    let pStatement, pStatementRef =
        createParserForwardedToRef<Statement, Unit>()
    
    let pPrint: Parser<Statement, Unit> =
        pWord "print"
        >>. parens pExpression
        |>> Print
        
    let pSetValue: Parser<Statement, Unit> =
        let identifier =
            many1Satisfy2 System.Char.IsLetter System.Char.IsLetterOrDigit
             .>> manyChars (anyOf " \t") .>> pWord "="

        identifier .>>. pExpression
        |>> Set

    let pBlock: Parser<Statement list, Unit> =
        between (pWord "{") (pWord "}") (many pStatement)
    let pIf: Parser<Statement, Unit> =
        let condition = pWord "if" >>. pExpression
        let inner = pBlock
        let elseBlock = pWord "else" >>. pBlock |> opt
        
        pipe3 condition inner elseBlock (fun condition inner elseBlock ->
            If (condition, inner, elseBlock))

    let pWhile: Parser<Statement, Unit> =
        let condition = pWord "while" >>. pExpression
        
        condition .>>. pBlock
        |>> While
        
    do pStatementRef := choice [
        pPrint
        pIf
        pWhile
        pSetValue
    ]
    
    let parseSourceFile filePath =
        match runParserOnFile (many pStatement) () filePath System.Text.Encoding.UTF8 with
        | Success (result, _, _) -> printfn $"%A{result}"
        | Failure (error, _, _) -> printfn $"%s{error}"
        
    let parseString str =
        match runParserOnString (many pStatement) () "run parser on string" str with
        | Success (result, _, _) -> result
        | Failure (error, _, _) -> failwith $"Error: %s{error}"