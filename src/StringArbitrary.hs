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

  letters <- replicateM numLetters (elements ['a'..'z'])
  spaces <- replicateM numSpaces (return ' ')
  shuffle (letters ++ spaces)