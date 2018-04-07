open Microsoft.FSharp.Collections
open System

open FirstOrderLogic.Substitution
open FirstOrderLogic.Parser

let exhaustInput() =
    Seq.initInfinite (fun _ -> Console.ReadLine())
    |> Seq.takeWhile ((<>) null)

let flattenResults (lst: List<Result<'a, 'b>>) =
        let foldr acc elem =
            elem |> Result.bind (fun inner -> acc |> Result.map (fun lst -> inner::lst))

        lst |> List.fold foldr (Ok [])

let processInput() =
    let toSubPair (str: String) =
        let [|v; t; |] = str.Split [| ' ' |]

        parseVariable v
        |> Result.bind (fun v -> (parseTerm t) |> Result.map (fun t -> (v, t)))

    let extractPairs() =
        exhaustInput()
            |> Seq.map toSubPair
            |> Seq.toList
            |> flattenResults

    let formula = parseFormula <| Console.ReadLine()

    formula
        |> Result.bind 
            (fun f -> extractPairs() |> Result.map (fun pairs -> (f, pairs))) 
        |> Result.map (fun (f, pairs) -> (f, Map.ofList(pairs)))

[<EntryPoint>]
let main argv =
    Console.InputEncoding <- System.Text.Encoding.UTF8
    Console.OutputEncoding <- System.Text.Encoding.UTF8

    processInput()
    |> Result.mapError (fun (label, err) -> sprintf "%s\n%s" label err)
    |> Result.bind (fun (f, pairs) -> performSubstitution f pairs)
    |> Result.map (fun f -> printfn "%s" (f.ToString()))
    |> Result.mapError (fun err -> printfn "%A" err)
    |> ignore

    0
