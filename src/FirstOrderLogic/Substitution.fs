namespace FirstOrderLogic

/// Elsőrendű logikai formulák helyettesítési algoritmusát tartalmazó modul.
module Substitution =
    open Microsoft.FSharp.Collections

    open Model
    open Utility

    /// Meghatározza, hogy egy adott változó helyére mely termet kell helyettesíteni (ha van ilyen).
    let private substituteOf (subMap: Map<string, Term>) x =
        if subMap.ContainsKey x then
            Some subMap.[x]
        else
            None

    /// Meghatározza, hogy egy adott term tartalmaz-e kötött változót.
    let rec private isBound (bounds: Set<string>) term =
        match term with
        // Ha a term egy v változó, akkor megnézzük, hogy benne van-e a kötött változók halmazában.
        | Variable v -> bounds.Contains v
        // Ha a term egy konstans, akkor nincs dolgunk, konstanst nem lehet kötni.
        | Constant _ -> false
        // Ha a term egy függvény, akkor annak argumentumaira rekurzívan meghívjuk ezt a függvényt.
        | Function (_, args) ->
            args |> List.exists (fun t -> isBound bounds t)

    /// Elvégzi a behelyettesítést egy termen belül. Visszaad egy új, már a helyettesítésekkel frissített termet.
    /// Az eredeti termet nem változtatja meg.
    /// Ez a függvény rekurzív. Azaz a függvényargumentumokra önmagát hívja meg, mindaddig, míg el nem jut egy
    /// változóig (melyet helyettesít vagy sem), vagy egy konstansig (melyet nem változtat meg).
    let rec private substituteTerm term substitutions boundVars =
        match term with
        // Ha a term egy változó, akkor előfordulhat, hogy helyettesítést kell végrehajtaunk.
        // Ehhez először a substituteOf-fal megnézzük, hogy van-e term, melyet v helyére kell beírnunk.
        | Variable v -> 
            match (substituteOf substitutions v) with
            // Ha nincs, akkor visszaadjuk a változót változatlanul (haha).
            | None -> 
                Ok <| Variable v
            // Ha subTerm kerülne v helyére, azonban subTerm tartalmaz kötött változókat, akkor hibát adunk vissza,
            // nem lehet helyettesíteni.            
            | Some subTerm when isBound boundVars subTerm ->
                Error <| sprintf "%s cannot be substituted by term \"%s\" because it contains bound variable(s)." v (subTerm.ToString())
            // Ha v kötött, akkor szintén nem lehet a helyettesítést elvégezni.            
            | Some subTerm when isBound boundVars (Variable v) ->
                Error <| sprintf "%s is a bound variable that cannot be substituted by %s" v (subTerm.ToString())
            // Ha minden rendben, akkor v helyett visszaadjuk a helyére beírandó termet.            
            | Some subTerm ->
                Ok subTerm
        // Ha a term egy konstans, akkor ezúttal sincs dolgunk, hiszen konstans helyére nem lehet beírni.            
        | Constant c -> Ok <| Constant c
        // Ha a term egy függvény, akkor rekurzívan, annak minden argumentumára (melyek szintén termek) meghívjuk e függvényt.
        | Function (name, args) ->
            args 
            |> List.map (fun arg -> substituteTerm arg substitutions boundVars)
            |> flattenResults
            |> Result.map (fun args -> Function (name, args))

    /// Elvégzi a helyettesítést egy formulán belül, és visszaadja a frissített formulát.
    /// Ahogy a Model.fs Formula típusa, úgy ez is rekurzív lesz, lépésről lépésre bontogatja ki a formulákat, mindaddig,
    /// amíg el nem jut egy predikátumig. Ha megvan egy predikátum, akkor annak argumentumaira meghívja a substituteTerm függvényt.
    let rec private substituteFormula formula substitutions boundVars =
        match formula with
        // Ha unáris operátort tartalmazó formulánk van, akkor végezzük el a helyettesítést az operátor (op)
        // operandusaként (formula) megjelenő formulában. Végül ezt adjuk vissza.
        | UnaryFormula (op, formula) -> 
            substituteFormula formula substitutions boundVars
            |> Result.map (fun res -> UnaryFormula (op, res))

        // Amennyiben a formula egy predikátum, akkor vegyük annak az argumentumait, és végezzük el rájuk a helyettesítést.
        // Utána adjuk vissza a frissített formulát.
        | Predicate (name, args) ->
            args 
            |> List.map (fun arg -> substituteTerm arg substitutions boundVars)
            |> flattenResults
            |> Result.map (fun args -> Predicate(name, args))

        // Ha a formula egy bináris operátort (op) tartalmaz, akkor először dolgozzuk fel annak bal operandusát (lhs),
        // majd, amennyiben ez sikeres volt, a jobb operandusát (rhs), és adjuk vissza a frissített formulát.
        | BinaryFormula (lhs, op, rhs) ->
            substituteFormula lhs substitutions boundVars
            |> Result.bind 
                (fun left -> (substituteFormula rhs substitutions boundVars) 
                             |> Result.bind (fun right -> Ok (left, right)))
            |> Result.bind (fun (left, right) -> Ok <| BinaryFormula (left, op, right))

        // Ha a formula egy kvantort tartalmaz, akkor nem csak helyettesítést kell végrehajtanuk, hanem hozzá is kell
        // adnunk a kvantor által kötött változót a kötött változók halmazához.
        | QuantifiedFormula (quantifier, variable, formula) ->
            substituteFormula formula substitutions (boundVars.Add variable)
            |> Result.map (fun res -> QuantifiedFormula (quantifier, variable, res))

    /// Végrehajtja a helyettesítést a formula formulában, a substitutions Map-nek megfelelő
    /// változó-term párokkal. 
    let performSubstitution formula substitutions =
        // A harmadik paraméter egy üres halmaz, hiszen kezdetben nincsenek kötött változók.
        substituteFormula formula substitutions (new Set<string>([]))
