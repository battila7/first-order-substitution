namespace FirstOrderLogic

module Model =
    type Quantifier =
        | Existential
        | Universal
        override q.ToString() = 
            match q with
            | Existential -> "∃"
            | Universal -> "∀"

    type BinaryOperator =
        | Disjunction
        | Conjunction
        | Implication
        override op.ToString() =
            match op with
            | Disjunction -> "∨"
            | Conjunction -> "∧"
            | Implication -> "→"

    type UnaryOperator =
        | Negation
        override op.ToString() =
            match op with
            | Negation -> "¬"

    type Term =
        | Function of (string * Term list)
        | Constant of string
        | Variable of string
        override t.ToString() =
            match t with
            | Constant c -> c
            | Variable v -> v
            | Function (name, args) ->
                let argStr = 
                    args 
                    |> List.map (fun arg -> arg.ToString())
                    |> String.concat " "
                
                sprintf "%s(%s)" name argStr            
                

    type Formula =
        | QuantifiedFormula of (Quantifier * string * Formula)
        | BinaryFormula of (Formula * BinaryOperator * Formula)
        | UnaryFormula of (UnaryOperator * Formula)
        | Predicate of (string * Term list)
        override f.ToString() =
            match f with
            | QuantifiedFormula (quantifier, variable, formula) -> 
                sprintf "%s%s%s" (quantifier.ToString()) variable (formula.ToString())
            | BinaryFormula (lhs, op, rhs) ->
                sprintf "(%s%s%s)" (lhs.ToString()) (op.ToString()) (rhs.ToString())
            | UnaryFormula (op, formula) ->
                sprintf "%s%s" (op.ToString()) (formula.ToString())
            | Predicate (name, args) ->
                let argStr = 
                    args 
                    |> List.map (fun arg -> arg.ToString())
                    |> String.concat " "
                
                sprintf "%s(%s)" name argStr                                 
    
