open Microsoft.FSharp.Collections
open System

open FirstOrderLogic.Substitution
open FirstOrderLogic.Parser

let exhaustInput() =
    Seq.initInfinite (fun _ -> Console.ReadLine())
    |> Seq.takeWhile ((<>) null)

let processInput() =
    let k = Console.ReadLine()
    printfn "%s" k
    let formula = parseFormula <| k

    let toSubPair (str: String) =
        let [|v; t; |] = str.Split [| ' ' |]

        match (parseVariable v) with
        | Error err -> Error err
        | Ok v ->
            match (parseTerm t) with
            | Error err -> Error err
            | Ok term -> Ok (v, term)

    let rec hp = function
        | [] -> Ok []
        | (Ok res)::xs ->
            match (hp xs) with
            | Error err -> Error err
            | Ok lst -> Ok (res::lst)
        | (Error err)::_ -> Error err            

    match formula with
    | Error err -> Error err
    | Ok f ->
        let pairs =
            exhaustInput()
            |> Seq.map toSubPair
            |> Seq.toList
            |> hp

        match pairs with
        | Error err -> Error err
        | Ok p -> Ok (f, Map.ofList(p))        

[<EntryPoint>]
let main argv =
    Console.InputEncoding <- System.Text.Encoding.UTF8
    Console.OutputEncoding <- System.Text.Encoding.UTF8

    match processInput() with
    | Ok (f, pairs) -> 
        match (performSubstitution f pairs) with
        | Ok f ->  printfn "%s" (f.ToString())
        | Error err -> printfn "%A" err
    | Error err -> printfn "%A" err

    0
