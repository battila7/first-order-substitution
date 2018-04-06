module Parser
    open System

    type Result<'a> =
        | Success of 'a
        | Failure of string

    type Parser<'T> = 
        Parser of (string -> Result<'T * string>)    

    let parseChar charToMatch =
        let parseFn input =
            if String.IsNullOrEmpty input then
                Failure "No more input"
            else
                let currentChar = input.[0]
                if currentChar = charToMatch then
                    Success (currentChar, input.[1..])
                else
                    Failure <| sprintf "Expected %c, got %c" charToMatch currentChar

        Parser parseFn

    let applyParser parser input =
        match parser with
        | Parser fn -> fn input

    let andThen first second =
        let parseFn input =
            let firstResult = applyParser first input

            match firstResult with
            | Failure err -> Failure err
            | Success (firstValue, firstRemaining) ->
                let secondResult = applyParser second firstRemaining

                match secondResult with
                | Failure err -> Failure err
                | Success (secondValue, secondRemaining) ->
                    Success((firstValue, secondValue), secondRemaining)

        Parser parseFn        

    let (.>>.) = andThen

    let orElse first second =
        let parseFn input =
            match applyParser first input with
            | Success result -> Success result
            | Failure _      -> applyParser second input

        Parser parseFn

    let (<|>) = orElse

    let choice parsers =
        List.reduce (<|>) parsers
      
    let anyOf chars =
        chars
        |> List.map parseChar
        |> choice    


    let mapP f parser =
        let parseFn input =
            match applyParser parser input with
            | Failure err                 -> Failure err
            | Success (result, remaining) -> Success ((f result), remaining)

        Parser parseFn

    let (<!>) = mapP
    let (|>>) parser f = mapP f parser

    let returnP value =
        let parseFn input = Success (value, input)

        Parser parseFn

    let applyP fP xP =
        fP .>>. xP    
        |> mapP (fun (f, x) -> f x)

    let (<*>) = applyP

    let rec sequence parsers =
        let cons head tail = head::tail

        let consP xP yP = returnP cons <*> xP <*> yP

        match parsers with
        | [] -> returnP []
        | head::tail -> consP head (sequence tail)

    let parseString str =
        let toString charList = String(List.toArray charList)

        str
        |> List.ofSeq
        |> List.map parseChar
        |> sequence
        |> mapP toString

    let parseZeroOrMore parser input =
        let rec inner (x, str) =
            match (applyParser parser str) with
            | Failure _                          -> (x, str)
            | Success (result, strAfterResult)   -> inner (result::x, strAfterResult)

        inner ([], input)

    let many parser =
        Parser (fun input -> Success <| parseZeroOrMore parser input)        

    let many1 parser =
        let parseFn input =
            match (applyParser parser input) with
            | Failure err -> Failure err
            | Success (firstResult, remainingAfterFirst) ->
                let (zeroOrMoreResult, remaining) = parseZeroOrMore parser remainingAfterFirst

                Success (firstResult::zeroOrMoreResult, remaining)

        Parser parseFn

    let opt parser =
        let couldMatch = mapP Some parser
        let noMatch = returnP None

        couldMatch <|> noMatch

    let (.>>) p1 p2 =
        p1 .>>. p2
        |> mapP fst

    let (>>.) p1 p2 =
        p1 .>>. p2
        |> mapP snd

    let between p1 p2 p3 =
        p1 >>. p2 .>> p3    

    let sepBy1 parser separator =
        parser .>>. many (separator >>. parser)
        |>> fun (p, pList) -> p::pList

    let sepBy parser separator =
        sepBy1 parser separator <|> returnP []
