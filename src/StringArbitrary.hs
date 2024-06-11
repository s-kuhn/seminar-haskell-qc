module StringArbitrary
    ( SpecialString(..)
    , stringGen
    ) where

import Test.QuickCheck
import Control.Monad (replicateM)

-- Neuer Typ für spezielle Strings
newtype SpecialString = SpecialString { getSpecialString :: String }
    deriving (Eq, Show)

instance Arbitrary SpecialString where
    arbitrary = SpecialString <$> stringGen

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
