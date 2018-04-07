open Microsoft.FSharp.Collections
open System

open FirstOrderLogic.Substitution
open FirstOrderLogic.Parser
open ParserCombinators

let exhaustInput() =
    Seq.initInfinite (fun _ -> Console.ReadLine())
    |> Seq.takeWhile ((<>) null)

let processInput() =
    let k = Console.ReadLine()
    printfn "%s" k
    let formula = parseFormula <| k

    let toSubPair (str: String) =
        let [|v; t; |] = str.Split [| ' ' |]

        match (pVariable v) with
        | Failure err -> Failure err
        | Success v ->
            match (parseTerm t) with
            | Failure err -> Failure err
            | Success term -> Success (v, term)

    let rec hp = function
        | [] -> Success []
        | (Success res)::xs ->
            match (hp xs) with
            | Failure err -> Failure err
            | Success lst -> Success (res::lst)
        | (Failure err)::_ -> Failure err            

    match formula with
    | Failure err -> Failure err
    | Success f ->
        let pairs =
            exhaustInput()
            |> Seq.map toSubPair
            |> Seq.toList
            |> hp

        match pairs with
        | Failure err -> Failure err
        | Success p -> Success (f, Map.ofList(p))        

[<EntryPoint>]
let main argv =
    Console.InputEncoding <- System.Text.Encoding.UTF8
    Console.OutputEncoding <- System.Text.Encoding.UTF8

    match processInput() with
    | Success (f, pairs) -> 
        match (performSubstitution f pairs) with
        | Ok f ->  printfn "%s" (f.ToString())
        | Error err -> printfn "%A" err
    | Failure err -> printfn "%A" err

    0
