# Elsőrendű helyettesítés

Helyettesítést végrehajtó program [elsőrendű logikai formulákban](https://en.wikipedia.org/wiki/First-order_logic).

## Fordítás és futtatás

A fordítás és futtatás legegyszerűbb módja (mind Windowson, mind Linuxon), ha telepítjük a .NET CLI-t, majd kiadjuk a következő parancsot a repository gyökérkönyvtárában:

~~~~
$ dotnet run
~~~~

Ezzel a program fordításra és futtatásra kerül, majd rögtön várja az inputot. Érdemes az inputot valamilyen text fájlban elhelyezni, ekkor használható a következő forma is:

~~~~
$ dotnet run < input.txt
~~~~

## A program használata

A program a formulát, és a behelyettesítendő termeket a standard inputról olvassa be. A bemenet első sorának egy formulát, a rákövetkező sorainak pedig soronként egy-egy változó-term párt kell tartalmazniuk, pontosan egy szóközzel elválasztva. Például:

~~~~
(∀yP(x)∨¬Q(z))
x f(x)
z w
~~~~

### Helyettesítési szabályok

A helyettesítés részletes szabályai megtekinthetők [itt](http://web.mat.bham.ac.uk/R.W.Kaye/logic/subsyn.html).

A legfontosabb szabályok a következők:

  * csak változót lehet helyettesíteni,
  * kötött változó nem helyettesíthető,
  * kötött változót tartalmazó term nem illeszthető be.

### Az elfogadott nyelv

A program a következő nyelvtan által generált nyelvet fogadja el:

  * Változók - A változónevek az `xyzw` karakterek valamelyikével kezdődhetnek, melyet 0 vagy több számjegy követhet.
  * Konstansok - A konstansszimbólumok nevei az `abcd` karakterek valamelyikével kezdődhetnek, melyet 0 vagy több számjegy követhet.
  * Függvények  - A függvényszimbólumok az `xyzw` karakterek valamelyikével kezdődhetnek, melyet 0 vagy több számjegy követhet.
  * Predikátumok - A predikátumszimbólumok az `PQRS` karakterek valamelyikével kezdődhetnek, melyet 0 vagy több számjegy követhet.
  * Kvantorok - Egy kvantor csak egy változót köthet, azaz a `∀x∀yP(x,y)` formula helyes, míg a `∀xyP(x, y)` **nem**.
  * Bináris operátort taralmazó formula - A formulát kötelező zárójelek közé helyezni, tehát `(P(x)∨Q(x))` helyes, `P(x)∨Q(x)` azonban **nem**.

Fölösleges whitespace karakterek vagy fölösleges zárójelek nem elfogadottak.

## A kód szerkezete

A program forráskódja az [src](src) mappában található:

  * `FirstOrderLogic`
    * `Model.fs` - A formulákat és termeket leíró adatszerkezetek.
    * `Parser.fs` - A formula-parser. Karakterláncokból készít a `Model.fs` fájlban leírtaknak megfelelő típusú objektumokat.
    * `Substitution.fs` - A tényleges helyettesítési algoritmust tartalmazó fájl.
  * `ParserCombinators.fs` - Az elsőrendű logikától független, parser-íráshoz használható építőelemeket tartalmaz. (nincs kommentelve)
  * `Program.fs` - Tartalmazza a belépési pontot és az input beolvasását.
  * `Utility.fs` - Kisebb segédfüggvényeket tartalmaz. (nincs kommentelve)

<div align="center">
    <a href="http://fsharp.org">
        <img src="https://raw.github.com/battila7/first-order-substitution/master/img/fsharp.png">
    </a>
</div>