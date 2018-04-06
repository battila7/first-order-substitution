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
