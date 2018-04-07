namespace FirstOrderLogic

module Parser =
    open System
    open FirstOrderLogic.Model
    open ParserCombinators
    open ParserCombinators.Char

    let existentialQuantifier =
        parseChar '∃'
        |>> fun _ -> Quantifier.Existential
        <?> "existential quantifier"

    let universalQuantifier =
        parseChar '∀'
        |>> fun _ -> Quantifier.Universal
        <?> "universal quantifier"

    let quantifier =
        existentialQuantifier <|> universalQuantifier    

    let disjunctionOperator =
        parseChar '∨'
        |>> fun _ -> BinaryOperator.Disjunction
        <?> "disjunction operator"

    let conjunctionOperator =
        parseChar '∧'
        |>> fun _ -> BinaryOperator.Conjunction
        <?> "conjunction operator"

    let implicationOperator =
        parseChar '→'
        |>> fun _ -> BinaryOperator.Implication
        <?> "implication operator"

    let binaryOperator =
        choice [disjunctionOperator; conjunctionOperator; implicationOperator]    

    let negationOperator =
        Char.parseChar '¬'
        |>> fun _ -> UnaryOperator.Negation
        <?> "negation operator"

    let unaryOperator = negationOperator    

    let (term, termRef) = createForwardedToRefParser<Term>    

    let parseVariable =
        Char.anyOf ['x'; 'y'; 'z'; 'v'; 'w']
        .>>. many Char.digit
        |>> fun (ch, digits) -> String(List.toArray (ch::digits))

    let variable =
        parseVariable
        |>> Variable
        <?> "variable"

    let constant =
        Char.anyOf ['a'; 'b'; 'c'; 'd']
        .>>. many Char.digit
        |>> fun (ch, digits) -> String(List.toArray (ch::digits))
        |>> Constant
        <?> "constant"

    let functionName =
        Char.anyOf ['f'; 'g'; 'h'] 
        .>>. many Char.digit
        |>> fun (ch, digits) -> String(List.toArray (ch::digits))

    let betweenParens parser =
        between (parseChar '(') parser (parseChar ')')

    let func =
        let arguments = 
            sepBy term (parseChar ',')

        functionName
        .>>. betweenParens arguments
        |>> Function
        <?> "function"

    termRef := choice [
            variable;
            constant;
            func
        ]

    let (formula, formulaRef) = createForwardedToRefParser<Formula>

    let quantifiedFormula =
        quantifier
        .>>. parseVariable
        .>>. formula
        |>> fun ((q, v), f) -> QuantifiedFormula (q, v, f)
        <?> "quantifier formula"

    let binaryFormula =
        betweenParens (formula .>>. binaryOperator .>>. formula)
        |>> fun ((lhs, op), rhs) -> BinaryFormula (lhs, op, rhs)
        <?> "binary formula"

    let unaryFormula =
        unaryOperator .>>. formula
        |>> UnaryFormula
        <?> "unary formula"    

    let predicateName =
        anyOf ['P'; 'Q'; 'R'; 'S']
        .>>. many digit
        |>> fun (ch, digits) -> String(List.toArray (ch::digits))

    let predicate =
        let arguments = 
            sepBy term (parseChar ',')

        predicateName
        .>>. betweenParens arguments
        |>> Predicate
        <?> "predicate"   

    formulaRef := choice [
            quantifiedFormula;
            unaryFormula;
            binaryFormula;
            predicate
        ]

    let parseFormula input =
        match (applyParser formula input) with
        | Failure err -> Failure err
        | Success (formula, _) -> Success formula

    let parseTerm input =
        match (applyParser term input) with
        | Failure err -> Failure err
        | Success (term, _) -> Success term
    