# Seminararbeit-Quickcheck

## Quickcheck
### Einführung
- Bibliothek für zufälliges Testen der Programm**eigenschaften**.
- automatische Testgenerierung
- Man definiert Eigenschaften, welche die Funktionen erfüllen sollen (Sortierfunktion soll sortierte Liste zurückgeben).
- Eine Eigenschaft eines ist eine Beobachtung, von der wir erwarten, dass sie egal der Eingabe wahr ist (Die Länge eines Strings bleibt nach Anwendung von `reverse` gleich).
- Bei einem Fehler wird versucht, das Problem einzugrenzen (shrinking).
- Erste Implementierung von QuickCheck in Haskell und inzwischen in über 30 Sprachen übernommen.

### Ausführen:
Projekt bauen:
```bash
stack build
```

Programm ausführen:
```bash
stack exec <path to file or exe> # stack exec .\.stack-work\dist\eebe39f7\build\seminar-haskell-qc-
```

Code Coverage (optional):
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
Diese Funktion führt den Generator aus und gibt das Ergebnis zurück. Da die Generierung von Zufallswerten Nebenwirkungen hat (weil sie auf einen Zufallszahlengenerator zugreift), ist das Ergebnis in der `IO`-Monade verpackt.

### Arbitrary
- Produziert Generators
- Stellt Standard-Generatoren für klassischeTypen (https://hackage.haskell.org/package/QuickCheck-2.15/docs/Test-QuickCheck.html#g:5).

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
Hier wird eine Typannotation verwendet, um anzugeben, dass `arbitrary` ein Generator für den Typ `Bool` oder `[(Int, Bool)]` sein soll. Die Typannotation `:: Gen Bool` spezifiziert, dass `arbitrary` als ein `Gen Bool` interpretiert werden soll. Dies ist notwendig, wenn der Compiler den Typ nicht automatisch ableiten kann oder wir den Typ explizit klarstellen wollen.


### Beispiel: String
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

### Beispiel: MyList
```haskell
data List a = Nil | Cons a (List a) deriving (Eq, Show)

-- Die Arbitrary Instanz für List a
instance Arbitrary a => Arbitrary (List a) where
  arbitrary = oneof [return Nil, Cons <$> arbitrary <*> arbitrary]
```


## FP in Haskell
- Rekursion
- Pattern Matching
- 

## Typeclasses
- ähnlich wie Interfaces

konkretes Beispiel
