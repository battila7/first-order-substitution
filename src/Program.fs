open Microsoft.FSharp.Collections
open System

open FirstOrderLogic.Substitution
open FirstOrderLogic.Parser
open Utility

/// Beolvassa az összes hátralevő sort a standard bemenetről.
let exhaustInput() =
    Seq.initInfinite (fun _ -> Console.ReadLine())
    |> Seq.takeWhile ((<>) null)

/// Felbontja a bemenetet egy elsőrendű formulára és változó-term párokra (egy Mapben).
let processInput() =
    // Egy stringet egy változóra és egy termre bont.
    let toSubPair (str: String) =
        match str.Split [| ' ' |] with
        | [| v; t |] -> parseVariable v
                        |> Result.mapError(fun (label, _) -> (label, "Expected a variable, but received something else (see above)."))
                        |> Result.bind (fun v -> (parseTerm t) |> Result.map (fun t -> (v, t)))
        | _          -> Error (str, "^ Unexpected input: expected a variable and a term separated by a single space.")

    // Létrehozza a változó-term párokat a bemenet hátralevő soraiból.
    let extractPairs() =
        exhaustInput()
        |> Seq.map toSubPair
        |> Seq.toList
        |> flattenResults

    // Először beolvassuk, majd parse-oljuk a formulát.
    let formula = parseFormula <| Console.ReadLine()

    // Ha megvan a formula, akkor beolvassuk a változó-term párokat is,
    // majd visszaadjuk a kapottakat.
    formula
    |> Result.bind 
        (fun f -> extractPairs() |> Result.map (fun pairs -> (f, pairs))) 
    |> Result.map (fun (f, pairs) -> (f, Map.ofList(pairs)))

/// A program belépési pontja. 
/// Elsőként feldolgozzuk az inputot, azaz elsőrendű formulát, illetve termeket gyártunk belőle,
/// majd végrehajtjuk a helyettesítést.
[<EntryPoint>]
let main _ =
    // A logikai szimbólumok (például: ∀, ∃) megfelelő beolvasásához és kiírásához
    // szükséges az UTF-8 bemenet/kimenet.
    Console.InputEncoding <- System.Text.Encoding.UTF8
    Console.OutputEncoding <- System.Text.Encoding.UTF8

    // Az egész számítás a Result monádon (https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/results)
    // belül zajlik. Ha valami nem sikerül (például érvénytelen a bejövő formula),
    // akkor Error értékünk lesz, s minden további számítást "átugrunk".
    processInput()
    |> Result.mapError (fun (label, err) -> sprintf "%s\n%s" label err)
    |> Result.bind (fun (formula, pairs) -> performSubstitution formula pairs)
    |> Result.map (fun formula -> printfn "%s" (formula.ToString()))
    |> Result.mapError Console.WriteLine
    |> ignore

    0
