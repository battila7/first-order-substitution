module ParserCombinators
    open System

    type ParserLabel = string
    type ParserError = string

    type Result<'a> =
        | Success of 'a
        | Failure of (ParserLabel * ParserError)

    type Parser<'T> = {
        parseFn: (string -> Result<'T * string>)
        label: ParserLabel
    }

    let labelOf parser =
        parser.label

    let resultAsString = function
        | Success (value, _) -> sprintf "%A" value
        | Failure (label, err) -> sprintf "Error parsing %s\n%s" label err

    let applyParser parser input =
        parser.parseFn input

    let satisfy predicate =
        let label = "unknown"
        let parseFn input =
            if String.IsNullOrEmpty input then
                Failure (label, "No more input")
            else
                let currentChar = input.[0]
                if predicate currentChar then
                    Success (currentChar, input.[1..])
                else
                    Failure (label, sprintf "Unexpected %c" currentChar)

        { parseFn = parseFn; label = label }                        

    let setLabel parser newLabel =
        let parseFn input =
            match (applyParser parser input) with
            | Success result -> Success result
            | Failure (_, err) -> Failure (newLabel, err)

        { parseFn = parseFn; label = newLabel }

    let (<?>) = setLabel    

    let andThen first second =
        let label = sprintf "%s and then %s" (labelOf first) (labelOf second)
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

        { parseFn = parseFn; label = label }

    let (.>>.) = andThen

    let orElse first second =
        let label = sprintf "%s or else %s" (labelOf first) (labelOf second)
        let parseFn input =
            match applyParser first input with
            | Success result -> Success result
            | Failure _      -> applyParser second input

        { parseFn = parseFn; label = label }

    let (<|>) = orElse

    let choice parsers =
        List.reduce (<|>) parsers
      
    let mapP f parser =
        let label = "unknown"
        let parseFn input =
            match applyParser parser input with
            | Failure (oldLabel, err)     -> Failure (oldLabel, err)
            | Success (result, remaining) -> Success ((f result), remaining)

        { parseFn = parseFn; label = label }        

    let (<!>) = mapP
    let (|>>) parser f = mapP f parser

    let returnP value =
        let parseFn input = Success (value, input)

        { parseFn = parseFn; label = sprintf "%A" value }

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

    let parseZeroOrMore parser input =
        let rec inner (x, str) =
            match (applyParser parser str) with
            | Failure _                          -> (x, str)
            | Success (result, strAfterResult)   -> inner (result::x, strAfterResult)

        inner ([], input)

    let many parser =
        let label = sprintf "any of %s" (labelOf parser)
        let parseFn input = Success <| parseZeroOrMore parser input

        { parseFn = parseFn; label = label }

    let many1 parser =
        let label = sprintf "at least one of %s" (labelOf parser)
        let parseFn input =
            match (applyParser parser input) with
            | Failure (_, err) -> Failure (label, err)
            | Success (firstResult, remainingAfterFirst) ->
                let (zeroOrMoreResult, remaining) = parseZeroOrMore parser remainingAfterFirst

                Success (firstResult::zeroOrMoreResult, remaining)

        { parseFn = parseFn; label = label }        

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