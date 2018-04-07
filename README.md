# First-Order Substitution

Simple program performing variable to term substitution in [first-order logic](https://en.wikipedia.org/wiki/First-order_logic) formulae.

## Build & Run

Assuming you have the [.NET CLI](https://docs.microsoft.com/en-us/dotnet/core/tools/?tabs=netcore2x) installed, the program can be started by the following command:

~~~~
$ dotnet run
~~~~

## Using First-Order Substitution

The program reads the formula and the variable and term set from the standard input. First it expects a formula and then the variable and terms pairs separated by a space, each pair in its own line. Example:

~~~~
(∀yP(x)∨¬Q(z))
x f(x)
z w
~~~~

### Rules of Substitution

For more information on the rules of substitution, please read [Substitution in first-order languages](http://web.mat.bham.ac.uk/R.W.Kaye/logic/subsyn.html).

### Accepted Language

First-Order Substitution accepts a language generated by a fairly restricted grammar.

  * Variables - Must start with any of `xyzvw` that can be followed by zero or more digits.
  * Constants - Must start with any of `abcd` that can be followed by zero or more digits.
  * Functions  - Must start with any of `fgh` that can be followed by zero or more digits.
  * Predicates - Must start with any of `PQRS` that can be followed by zero or more digits.
  * Quantifiers - Each quantified variable must have its own quantifier. Therefore `∀x∀yP(x,y)` is valid, while `∀xyP(x, y)` is **not**.
  * Binary Formula - The formula must be enclosed with parentheses. Therefore `(P(x)∨Q(x))` is valid, while `P(x)∨Q(x)` is **not**.

Additional whitespace and parentheses are not accepted.
