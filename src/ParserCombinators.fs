module ParserCombinators
    open System

    type ParserLabel = string
    type ParserError = string

    type ParserResult<'a> = Result<'a, (ParserLabel * ParserError)>

    type Parser<'T> = {
        parseFn: (string -> ParserResult<'T * string>)
        label: ParserLabel
    }

    let labelOf parser =
        parser.label

    let applyParser parser input =
        parser.parseFn input

    let resultAsString = function
        | Ok (value, _) -> sprintf "%A" value
        | Error (label, err) -> sprintf "Error parsing %s\n%s" label err

    let dropRemaining = function
        | Ok (value, _) -> Ok value
        | Error err -> Error err    

    let createForwardedToRefParser<'a> =
        let label = "unknown"
        let unfixedParser =
            let parseFn _ : ParserResult<'a * string>  = failwith "unfixed parser"
            { parseFn = parseFn; label = label }

        let parserRef = ref unfixedParser

        let parseFn input =
            applyParser !parserRef input

        let wrapperParser = { parseFn = parseFn; label = label }

        (wrapperParser, parserRef)

    let satisfy predicate =
        let label = "unknown"
        let parseFn input =
            if String.IsNullOrEmpty input then
                Error (label, "No more input")
            else
                let currentChar = input.[0]
                if predicate currentChar then
                    Ok (currentChar, input.[1..])
                else
                    Error (label, sprintf "Unexpected %c" currentChar)

        { parseFn = parseFn; label = label }

    let setLabel parser newLabel =
        let parseFn input =
            match (applyParser parser input) with
            | Ok result      -> Ok result
            | Error (_, err) -> Error (newLabel, err)

        { parseFn = parseFn; label = newLabel }

    let private (<?>) = setLabel    

    let andThen first second =
        let label = sprintf "%s and then %s" (labelOf first) (labelOf second)
        let parseFn input =
            let firstResult = applyParser first input

            match firstResult with
            | Error err -> Error err
            | Ok (firstValue, firstRemaining) ->
                let secondResult = applyParser second firstRemaining

                match secondResult with
                | Error err -> Error err
                | Ok (secondValue, secondRemaining) ->
                    Ok ((firstValue, secondValue), secondRemaining)

        { parseFn = parseFn; label = label }

    let private (.>>.) = andThen

    let orElse first second =
        let label = sprintf "%s or else %s" (labelOf first) (labelOf second)
        let parseFn input =
            match applyParser first input with
            | Ok result -> Ok result
            | Error _   -> applyParser second input

        { parseFn = parseFn; label = label }

    let private (<|>) = orElse

    let choice parsers =
        List.reduce (<|>) parsers
      
    let mapP f parser =
        let label = "unknown"
        let parseFn input =
            match applyParser parser input with
            | Error (oldLabel, err)     -> Error (oldLabel, err)
            | Ok (result, remaining)    -> Ok ((f result), remaining)

        { parseFn = parseFn; label = label }        

    let private (<!>) = mapP
    let private (|>>) parser f = mapP f parser

    let returnP value =
        let parseFn input = Ok (value, input)

        { parseFn = parseFn; label = sprintf "%A" value }

    let applyP fP xP =
        fP .>>. xP    
        |> mapP (fun (f, x) -> f x)

    let private (<*>) = applyP

    let rec sequence parsers =
        let cons head tail = head::tail

        let consP xP yP = returnP cons <*> xP <*> yP

        match parsers with
        | [] -> returnP []
        | head::tail -> consP head (sequence tail)

    let private parseZeroOrMore parser input =
        let rec inner (x, str) =
            match (applyParser parser str) with
            | Error _                       -> (List.rev x, str)
            | Ok (result, strAfterResult)   -> inner (result::x, strAfterResult)

        inner ([], input)

    let many parser =
        let label = sprintf "any of %s" (labelOf parser)
        let parseFn input = Ok <| parseZeroOrMore parser input

        { parseFn = parseFn; label = label }

    let many1 parser =
        let label = sprintf "at least one of %s" (labelOf parser)
        let parseFn input =
            match (applyParser parser input) with
            | Error (_, err) -> Error (label, err)
            | Ok (firstResult, remainingAfterFirst) ->
                let (zeroOrMoreResult, remaining) = parseZeroOrMore parser remainingAfterFirst

                Ok (firstResult::zeroOrMoreResult, remaining)

        { parseFn = parseFn; label = label }        

    let opt parser =
        let couldMatch = mapP Some parser
        let noMatch = returnP None

        couldMatch <|> noMatch


    let dropFst p1 p2 =
        p1 .>>. p2
        |> mapP fst

    let dropSnd p1 p2 =
        p1 .>>. p2
        |> mapP snd    

    let private (.>>) p1 p2 =
        p1 .>>. p2
        |> mapP fst

    let private (>>.) p1 p2 =
        p1 .>>. p2
        |> mapP snd

    let between p1 p2 p3 =
        p1 >>. p2 .>> p3    

    let sepBy1 parser separator =
        parser .>>. many (separator >>. parser)
        |>> fun (p, pList) -> p::pList

    let sepBy parser separator =
        sepBy1 parser separator <|> returnP []


    module Operators =
        let (<?>) = (<?>)

        let (.>>.) = (.>>.)

        let (<|>) = (<|>)

        let (<!>) = (<!>)

        let (|>>) = (|>>)

        let (<*>) = (<*>)

        let (.>>) = (.>>)

        let (>>.) = (>>.)


    module Char =
        let parseChar (charToMatch: char) =
            satisfy (fun ch -> ch = charToMatch)
            <?> (sprintf "char %c" charToMatch)

        let anyOf chars =
            let label = sprintf "any of %A" chars

            chars
            |> List.map parseChar
            |> choice    
            <?> label

        let parseString str =
            let toString charList = String(List.toArray charList)

            str
            |> List.ofSeq
            |> List.map parseChar
            |> sequence
            |> mapP toString

        let digit =
            satisfy Char.IsDigit
            <?> "digit"