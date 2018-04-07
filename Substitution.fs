namespace FirstOrderLogic

module Substitution =
    open Microsoft.FSharp.Collections

    open Model
    open Utility

    let private substituteOf (subMap: Map<string, Term>) x =
        if subMap.ContainsKey x then
            Some subMap.[x]
        else
            None

    let rec private isBound (bounds: Set<string>) term =
        match term with
        | Variable v -> bounds.Contains v
        | Constant _ -> false
        | Function (_, args) ->
            args |> List.exists (fun t -> isBound bounds t)

    let rec private substituteTerm term substitutions boundVars =
        match term with
        | Variable v -> 
            match (substituteOf substitutions v) with
            | None -> 
                Ok <| Variable v
            | Some subTerm when isBound boundVars subTerm ->
                Error <| sprintf "%s cannot be substituted by term \"%s\" because it contains bound variable(s)." v (subTerm.ToString())
            | Some subTerm when isBound boundVars (Variable v) ->
                Error <| sprintf "%s is a bound variable that cannot be substituted by %s" v (subTerm.ToString())
            | Some subTerm ->
                Ok subTerm
        | Constant c -> Ok <| Constant c
        | Function (name, args) ->
            args 
            |> List.map (fun arg -> substituteTerm arg substitutions boundVars)
            |> flattenResults
            |> Result.map (fun args -> Function (name, args))

    let rec private substituteFormula formula substitutions boundVars =
        match formula with
        | UnaryFormula (op, formula) -> 
            substituteFormula formula substitutions boundVars
            |> Result.map (fun res -> UnaryFormula (op, res))

        | Predicate (name, args) ->
            args 
            |> List.map (fun arg -> substituteTerm arg substitutions boundVars)
            |> flattenResults
            |> Result.map (fun args -> Predicate(name, args))

        | BinaryFormula (lhs, op, rhs) ->
            substituteFormula lhs substitutions boundVars
            |> Result.bind 
                (fun left -> (substituteFormula rhs substitutions boundVars) 
                             |> Result.bind (fun right -> Ok (left, right)))
            |> Result.bind (fun (left, right) -> Ok <| BinaryFormula (left, op, right))

        | QuantifiedFormula (quantifier, variable, formula) ->
            substituteFormula formula substitutions (boundVars.Add variable)
            |> Result.map (fun res -> QuantifiedFormula (quantifier, variable, res))

    let performSubstitution formula substitutions =
        substituteFormula formula substitutions (new Set<string>([]))
