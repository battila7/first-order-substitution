namespace FirstOrderLogic

module Model =
    type Quantor =
        | Existential
        | Universal

    type BinaryOperator =
        | Disjunction
        | Conjunction
        | Implication

    type UnaryOperator =
        | Negation        

    type Term =
        | Function of (string * List<Term>)
        | Constant of string
        | Variable of string

    type Formula =
        | QuantifiedFormula of (Quantor * string * Formula)
        | BinaryFormula of (Formula * BinaryOperator * Formula)
        | UnaryFormula of (UnaryOperator * Formula)
        | Predicate of (string * List<Term>)
