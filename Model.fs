namespace FirstOrderLogic

module Model =
    type Quantifier =
        | Existential
        | Universal

    type BinaryOperator =
        | Disjunction
        | Conjunction
        | Implication

    type UnaryOperator =
        | Negation        

    type Term =
        | Function of (string * Term list)
        | Constant of string
        | Variable of string

    type Formula =
        | QuantifiedFormula of (Quantifier * string * Formula)
        | BinaryFormula of (Formula * BinaryOperator * Formula)
        | UnaryFormula of (UnaryOperator * Formula)
        | Predicate of (string * Term list)
