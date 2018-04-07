open Microsoft.FSharp.Collections
open System

open FirstOrderLogic.Substitution
open FirstOrderLogic.Parser
open Utility

let exhaustInput() =
    Seq.initInfinite (fun _ -> Console.ReadLine())
    |> Seq.takeWhile ((<>) null)

let processInput() =
    let toSubPair (str: String) =
        let [|v; t |] = str.Split [| ' ' |]

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
let main _ =
    Console.InputEncoding <- System.Text.Encoding.UTF8
    Console.OutputEncoding <- System.Text.Encoding.UTF8

    processInput()
    |> Result.mapError (fun (label, err) -> sprintf "%s\n%s" label err)
    |> Result.bind (fun (formula, pairs) -> performSubstitution formula pairs)
    |> Result.map (fun formula -> printfn "%s" (formula.ToString()))
    |> Result.mapError (fun err -> printfn "%A" err)
    |> ignore

    0
