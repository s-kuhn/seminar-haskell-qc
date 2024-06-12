# Seminararbeit-Quickcheck

## Inhalt
- [Seminararbeit-Quickcheck](#seminararbeit-quickcheck)
  - [Inhalt](#inhalt)
  - [Quickcheck](#quickcheck)
    - [Einführung](#einführung)
    - [Ausführen:](#ausführen)
    - [Generators](#generators)
    - [Arbitrary](#arbitrary)
    - [Beispiel: String](#beispiel-string)
    - [Beispiel: MyList](#beispiel-mylist)
  - [Tests](#tests)
  - [Funktionale Programmierung in Haskell](#funktionale-programmierung-in-haskell)

## Quickcheck
### Einführung
- Bibliothek für zufälliges Testen der Programm**eigenschaften**.
- automatische Testdatengenerierung
- Man definiert Eigenschaften, welche die Funktionen erfüllen sollen (Sortierfunktion soll sortierte Liste zurückgeben).
- Unter einer Eigenschaft versteht man eine Beobachtung, von der erwartet wird, dass sie unabhängig von den Eingabewerten immer zutrifft, wie zum Beispiel, dass die Länge eines Strings nach Anwendung der Funktion `reverse` unverändert bleibt.
- Bei einem Fehler wird versucht, das Problem einzugrenzen (shrinking).
- Erste Implementierung von QuickCheck in Haskell, inzwischen in über 30 Sprachen übernommen.

### Ausführen:
Projekt bauen:
```bash
stack build
```

Programm ausführen:
```bash
stack exec <path to file or exe> # stack exec .\.stack-work\dist\eebe39f7\build\seminar-haskell-qc-
```

Test ausführen (Code Coverage optional):
```bash
stack test --coverage
```

### Generators
- Werden verwendet, um Werte zu erzeugen
- Bietet große Anzahl an Methoden zu spezifizieren (https://hackage.haskell.org/package/QuickCheck-2.15/docs/Test-QuickCheck.html#g:8)

Beispiele für GHCI:
```haskell
-- generate :: Gen a -> IO a

-- produce 1, 2, or 3
generate $ elements [1,2,3]

-- produce a lowercase letter
generate $ choose ('a', 'z')

-- produce a constant value (since Gen has a Monad instance)
generate $ return 1
```
Diese Funktion führt den Generator aus und gibt das Ergebnis zurück. Da die Generierung von Zufallswerten Nebenwirkungen hat (weil sie auf einen Zufallszahlengenerator zugreift), ist das Ergebnis in der IO-Monade verpackt, wodurch Haskell eine strikte Trennung zwischen reinem und nicht-reinem Code erzwingt.

### Arbitrary
- Produziert Generatoren
- Stellt Standard-Generatoren für klassische Typen (https://hackage.haskell.org/package/QuickCheck-2.15/docs/Test-QuickCheck.html#g:5).

```haskell
generate (arbitrary :: Gen Bool)

generate (arbitrary :: Gen [(Int, Bool)])


data MyType = MyType {
    foo :: Int,
    bar :: Bool,
    baz :: Float
    } deriving (Show)

generate $ MyType <$> arbitrary <*> arbitrary <*> arbitrary
```

`arbitrary` ist eine Methode der Typklasse `Arbitrary`, die in QuickCheck definiert ist. Sie wird verwendet, um Standard-Generatoren für verschiedene Typen zu definieren. Wenn ein Typ eine Instanz von `Arbitrary` ist, bedeutet dies, dass es eine Definition für `arbitrary` gibt, die einen Generator für diesen Typ zurückgibt.

Hier wird eine Typannotation verwendet, um anzugeben, dass `arbitrary` ein Generator für den Typ `Bool` oder für `[(Int, Bool)]` ist. Die Typannotation `:: Gen Bool` spezifiziert, dass `arbitrary` als ein `Gen Bool` interpretiert werden soll. Dies ist notwendig, wenn der Compiler den Typ nicht automatisch ableiten kann oder wenn eine explizite Typangabe erforderlich ist.


### Beispiel: [String](./src/StringArbitrary.hs)
```haskell
module StringArbitrary
    ( stringGen
    ) where

import Test.QuickCheck
import Control.Monad (replicateM)

stringGen :: Gen [Char]
stringGen = do
  numSpaces <- chooseInt (0, 10)
  numLetters <- chooseInt (0, 40 - numSpaces)

  -- replicateM :: Applicative m => Int -> m a -> m [a]
  letters <- replicateM numLetters (elements ['a'..'z'])
  spaces <- replicateM numSpaces (return ' ')

  -- Kombiniere und mische Buchstaben und Leerzeichen
  shuffled <- shuffle (letters ++ spaces)

  return $ limitConsecutiveSpaces 5 shuffled

-- Begrenzt aufeinanderfolgende Leerzeichen auf die angegebene Maximalanzahl
limitConsecutiveSpaces :: Int -> String -> String
limitConsecutiveSpaces maxSpaces = go 0
  where
    go _ [] = []
    go n (x:xs)
      | x == ' ' =
          if n < maxSpaces
          then x : go (n+1) xs  -- Leerzeichen bleibt bestehen
          else go n xs          -- Leerzeichen wird entfernt
      | otherwise = x : go 0 xs -- Reset des Zählers, wenn kein Leerzeichen

```

```haskell
generate (stringGen :: Gen String)
```

### Beispiel: [MyList](./src/MyListArbitrary.hs)
```haskell
data List a = Nil | Cons a (List a) deriving (Eq, Show)

-- Die Arbitrary Instanz für List a
instance Arbitrary a => Arbitrary (List a) where
  arbitrary = oneof [return Nil, Cons <$> arbitrary <*> arbitrary]
```

## Tests
In der `main` der [Testdatei](./test/Spec.hs) wird mit `verboseCheck` oder `quickCheck` die Testproperty mit einem Generator aufgerufen. 

In diesem Beispiel wird die Eigenschaft überprüft, dass der String nicht länger als 40 Zeichen ist, wobei die Überprüfung mit generierten Strings erfolgt.
```haskell
main :: IO ()
main = do
  verboseCheck prop_lengthCheck

prop_lengthCheck :: SpecialString -> Bool
prop_lengthCheck (SpecialString str) = length str < 40
```

## Funktionale Programmierung in Haskell
- Rekursion: die Hilfsfunktion wird rekursiv aufgerufen.
```haskell
limitConsecutiveSpaces :: Int -> String -> String
limitConsecutiveSpaces maxSpaces = go 0
  where
    go _ [] = []
    go n (x:xs)
      | x == ' ' =
          if n < maxSpaces
          then x : go (n+1) xs  -- Leerzeichen bleibt bestehen
          else go n xs          -- Leerzeichen wird entfernt
      | otherwise = x : go 0 xs -- Reset des Zählers, wenn kein Leerzeichen
```
- Pattern Matching: wird go mit einem beliebigen n und der leeren Liste aufgerufen, greift dieser fall. Wird es mit einer nicht leeren Liste aufgerufen, greift der andere Fall.
- Guards: Nach der `|` wird eine boolsche Bedingung definiert und für den Fall `true` nach dem `=` definiert was geschehen soll. Alle anderen Fälle werden in `otherwise` gefangen.
