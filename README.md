# Seminararbeit-Quickcheck

## Quickcheck
### Einführung
- Bibliothek für zufälliges Testen der Programm**eigenschaften**.
- automatische Testgenerierung
- Man definiert Eigenschaften, welche die Funktionen erfüllen sollen (Sortierfunktion soll sortierte Liste zurückgeben).
- Eine Eigenschaft eines ist eine Beobachtung von der wir erwarten, dass sie egal der Eingabe wahr ist (Die Länge eines Strings bleibt nach Anwendung von `reverse` gleich).
- Bei einem Fehler wird versucht das Problem einzugrenzen (shrinking).
- Erste Implementierung von QuickCheck in Haskell und inzwischen in über 30 Sprachen übernommen.

### Ausführen:
Project bauen:
```bash
stack build
```

Programm ausführen:
```bash
stack exec <path to file or exe>
```

Code Coverage (optional):
```
stack test --coverage
```

### Generators
- Werden verwendet um Werte zu erzeugen

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

### Arbitrary
- Produziert Generators
- Stellt Standardgeneratoren für basic Typen.

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


### Beispiel: String
```haskell
stringGen :: Gen [Char]
stringGen = do
  -- Anzahl an Leerzeichen max 10
  numSpaces <- chooseInt (0, 10)

  -- Gesamtlänge max 40 abüglich Leerzeichen
  numLetters <- chooseInt (0, 40 - numSpaces)

  -- Zufällige Kleinbuchstaben anhand von der Anzahl ziehen
  -- replicateM :: Applicative m => Int -> m a -> m [a]
  -- 
  letters <- replicateM numLetters (elements ['a'..'z'])

  -- Zufällige Kleinbuchstaben anhand von der Anzahl ziehen
  spaces <- replicateM numSpaces (return ' ')

  -- Kombiniere und mische Buchstaben und Leerzeichen
  let combined = letters ++ spaces
  shuffled <- shuffle combined

  return $ fixSpaces shuffled

-- Wendet limitSpaces auf jede Gruppe von Zeichen an
fixSpaces :: String -> String
fixSpaces = concatMap limitSpaces . groupBySpaces

-- Gruppiere die Zeichen in Listen von aufeinanderfolgenden Leerzeichen oder Buchstaben
groupBySpaces :: String -> [String]
groupBySpaces [] = []
groupBySpaces (x:xs) = (x : takeWhile (== x) xs) : groupBySpaces (dropWhile (== x) xs)

-- Begrenze die Anzahl aufeinanderfolgender Leerzeichen auf 5
-- `s@` ist ein Alias, der es ermöglicht, den gesamten String weiterhin als `s` zu verwenden.
-- Wird nur mit String mit gleichen Zeichen aufgerufen. Bsp: "aaa", "      ", usw.
limitSpaces :: String -> String
limitSpaces s@(x:_) 
  | x == ' '  = take 5 s  -- Maximal 5 Leerzeichen
  | otherwise = s
```

```haskell
generate (stringGen :: Gen String)
```

### Beispiel: MyList
```haskell

```


## FP in Haskell
- Rekursion
- Pattern Matching
- 

## Typeclasses
- 

konkretes beispiel
