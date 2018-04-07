namespace FirstOrderLogic

module Substitution =
    open Model
    open Microsoft.FSharp.Collections

    type SubstitutionPair = string * Term

    let substituteOf (subMap: Map<string, Term>) x =
        if subMap.ContainsKey x then
            Some subMap.[x]
        else
            None

    let rec isForbidden (forbiddenSet: Set<string>) term =
        match term with
        | Variable v -> forbiddenSet.Contains v
        | Constant c -> forbiddenSet.Contains c
        | Function (_, args) ->
            args |> List.exists (fun x -> isForbidden forbiddenSet x)

    let rec termSub term subMap forbidden =
        let rec hp = function
        | [] -> Ok []
        | (Ok res)::xs ->
            match (hp xs) with
            | Error err -> Error err
            | Ok lst -> Ok (res::lst)
        | (Error err)::_ -> Error err

        match term with
        | Variable v -> 
            match (substituteOf subMap v) with
            | None -> 
                Ok <| Variable v
            | Some nv when isForbidden forbidden nv ->
                Error <| sprintf "%s cannot be substituted by term \"%s\" because it contains bound variable(s)." v (nv.ToString())
            | Some nv when isForbidden forbidden (Variable v) ->
                Error <| sprintf "%s is a bound variable that cannot be substituted by %s" v (nv.ToString())
            | Some nv ->
                Ok nv 
        | Constant c -> Ok <| Constant c
        | Function (name, args) ->
            let argResult =
                args 
                |> List.map (fun arg -> termSub arg subMap forbidden)
                |> hp

            match argResult with
            | Error err -> Error err
            | Ok res -> Ok <| Function (name, res)
    
    let bind switch twoTrack =
        match twoTrack with
        | Ok res -> switch res
        | Error err -> Error err

    let (>>=) twoTrack switch =
        bind switch twoTrack

    let rec subHelper formula subMap forbidden =
        let rec hp = function
        | [] -> Ok []
        | (Ok res)::xs ->
            match (hp xs) with
            | Error err -> Error err
            | Ok lst -> Ok (res::lst)
        | (Error err)::_ -> Error err

        match formula with
        | UnaryFormula (_, formula) -> subHelper formula subMap forbidden
        | Predicate (name, args) ->
            let argResult =
                args 
                |> List.map (fun arg -> termSub arg subMap forbidden)
                |> hp

            match argResult with
            | Error err -> Error err
            | Ok res -> Ok <| Predicate (name, res)
        | BinaryFormula (lhs, op, rhs) ->
            match (subHelper lhs subMap forbidden) with
            | Error err -> Error err
            | Ok reslhs ->
                match (subHelper rhs subMap forbidden) with
                | Error err -> Error err
                | Ok resrhs -> Ok <| BinaryFormula (reslhs, op, resrhs)
        | QuantifiedFormula (q, v, f) ->
            match (subHelper f subMap (forbidden.Add v)) with
            | Error err -> Error err
            | Ok res -> Ok <| QuantifiedFormula (q, v, res)

    let performSubstitution formula subMap =
        subHelper formula subMap (new Set<string>([]))
