module StringArbitrary
    ( stringGen
    ) where

import Test.QuickCheck
import Control.Monad (replicateM)

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

-- Funktion, um sicherzustellen, dass nicht mehr als 5 aufeinanderfolgende Leerzeichen vorkommen
fixSpaces :: String -> String
fixSpaces = concatMap limitSpaces . groupBySpaces

-- Gruppiere die Zeichen in Listen von aufeinanderfolgenden Leerzeichen oder Buchstaben
groupBySpaces :: String -> [String]
groupBySpaces [] = []
groupBySpaces (x:xs) = (x : takeWhile (== x) xs) : groupBySpaces (dropWhile (== x) xs)

-- Begrenze die Anzahl aufeinanderfolgender Leerzeichen auf 5
limitSpaces :: String -> String
limitSpaces s@(x:_) 
  | x == ' '  = take 5 s  -- Maximal 5 Leerzeichen
  | otherwise = s
