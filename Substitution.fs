namespace FirstOrderLogic

module Substitution =
    open Model
    open Microsoft.FSharp.Collections

    type SubstitutionPair = string * Term

    let flattenResults (lst: List<Result<'a, 'b>>) =
        let foldr acc elem =
            elem |> Result.bind (fun inner -> acc |> Result.map (fun lst -> inner::lst))

        lst |> List.fold foldr (Ok [])

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
            args 
            |> List.map (fun arg -> termSub arg subMap forbidden)
            |> flattenResults
            |> Result.map (fun args -> Function (name, args))

    let rec subHelper formula subMap forbidden =
        match formula with
        | UnaryFormula (_, formula) -> subHelper formula subMap forbidden
        | Predicate (name, args) ->
            args 
            |> List.map (fun arg -> termSub arg subMap forbidden)
            |> flattenResults
            |> Result.map (fun args -> Predicate(name, args))
        | BinaryFormula (lhs, op, rhs) ->
            subHelper lhs subMap forbidden
            |> Result.bind 
                (fun k -> (subHelper rhs subMap forbidden) 
                          |> Result.bind (fun f -> Ok (k, f)))
            |> Result.bind (fun (k, f) -> Ok <| BinaryFormula (k, op, f))
        | QuantifiedFormula (q, v, f) ->
            subHelper f subMap (forbidden.Add v)
            |> Result.map (fun res -> QuantifiedFormula (q, v, res))

    let performSubstitution formula subMap =
        subHelper formula subMap (new Set<string>([]))
