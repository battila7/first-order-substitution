namespace FirstOrderLogic

module Parser =
    open System

    open FirstOrderLogic.Model
    open ParserCombinators
    open ParserCombinators.Char
    open ParserCombinators.Operators


    module private Elements =
        // Megpróbál parse-olni egy ∃ karaktert, s amennyiben ez sikeres, létrehoz egy Quantifier.Existential objektumot.
        let existentialQuantifier =
            parseChar '∃'
            |>> fun _ -> Quantifier.Existential
            <?> "existential quantifier"

        let universalQuantifier =
            parseChar '∀'
            |>> fun _ -> Quantifier.Universal
            <?> "universal quantifier"

        // Kvantor parse-olását végzi. Egy kvantor vagy egzisztenciális vagy univerzális lehet. Először az előbbit próbáljuk meg
        // illeszteni, amennyiben ez sikertelen, akkor az utóbbit.
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

        // Egy bináris operátort parse-ol. A fenti három parser valamelyikével próbálja meg feldolgozni a bemenet rákövetkező karakterét.
        let binaryOperator =
            choice [disjunctionOperator; conjunctionOperator; implicationOperator]    

        let negationOperator =
            Char.parseChar '¬'
            |>> fun _ -> UnaryOperator.Negation
            <?> "negation operator"

        let unaryOperator = negationOperator    

        // Hosszú és bonyolult...
        // Röviden arról van szó, hogy a következő parserek rekurzívak. Mivel egy term tartalmazhat más termet, ezért erre
        // a parsing során is tekintettel kell lennünk. F#-ban létre lehet hozni kölcsönösen rekurzív függvényeket, azonban
        // az nem mindig elegáns megoldás, különösen, ha kettőnél több ilyen függvényünk van.
        // Ennek következtében létrehozunk egy olyan parsert, aminek a mélyén egy referencia van. Ez a referencia kezdetben egy
        // teljesen haszontalan parserre mutat, azonban amikor elkészülünk a különbözőfajta termeket feldolgozó összes parserrel,
        // akkor beállítjuk egy valós értékre.
        // term itt a parser, termRef pedig az ennek alján megbúvó parserre mutató reference cell.
        let (term, termRef) = createForwardedToRefParser<Term>    

        // Egy változónév az xyzvw karakterek valamelyikével kezdődthet, majd tetszőleges számú számjeggyel folytatódhat.
        let variableName =
            Char.anyOf ['x'; 'y'; 'z'; 'v'; 'w']
            .>>. many Char.digit
            |>> fun (ch, digits) -> String(List.toArray (ch::digits))

        let variable =
            variableName
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
            // A függvény argumentumait ',' karakter választja el.
            let arguments = 
                sepBy term (parseChar ',')

            // Egy függvény egy névből, valamint zárójelek között elhyelezett argumentumokból áll.
            functionName
            .>>. betweenParens arguments
            |>> Function
            <?> "function"

        // Itt állítjuk be a termRef-et egy valós, termeket parse-olni képes parserre.
        termRef := choice [
                variable;
                constant;
                func
            ]

        let (formula, formulaRef) = createForwardedToRefParser<Formula>

        // QuantifiedFormula típust egy kvantor, egy változónév, valamint egy formula parse-olásával tudunk előállítani.
        let quantifiedFormula =
            quantifier
            .>>. variableName
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

    // Előállít egy elsőrendű formulát a bemenetből.
    let parseFormula input =
        applyParser Elements.formula input
        |> dropRemaining
        
    // A bemenetet feldolgozva, létrehoz egy termet.
    let parseTerm input =
        applyParser Elements.term input
        |> dropRemaining

    /// Amennyiben az input egy szabályos változónevet tartalmaz, visszaadja ezt a nevet, egyébként hibát ad.
    let parseVariable input =
        applyParser Elements.variableName input
        |> dropRemaining
    