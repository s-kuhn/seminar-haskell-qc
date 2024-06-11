import Test.QuickCheck
import MyListArbitrary
import StringArbitrary

main :: IO ()
main = do
  verboseCheck (prop_toList :: List Int -> Bool)

  verboseCheck prop_lengthCheck
  verboseCheck prop_maxTenSpaces
  verboseCheck prop_consecutiveSpaces
  verboseCheck prop_validCharacters


prop_toList :: Eq a => List a -> Bool
prop_toList lst = lst == toMyList (toList lst)

prop_lengthCheck :: SpecialString -> Bool
prop_lengthCheck (SpecialString str) = length str < 40

-- Testen, ob die Anzahl der Leerzeichen in der Zeichenkette maximal 10 ist
prop_maxTenSpaces :: SpecialString -> Bool
prop_maxTenSpaces (SpecialString str) = countSpaces str <= 10
  where
    countSpaces = length . filter (== ' ')

-- Testen, ob die Zeichenkette nie mehr als 5 aufeinanderfolgende Leerzeichen hat
prop_consecutiveSpaces :: SpecialString -> Bool
prop_consecutiveSpaces (SpecialString str) = go 0 str
  where
    go :: Int -> String -> Bool
    go _ [] = True
    go count (x:xs)
      | x == ' ' = (count < 5) && go (count + 1) xs
      | otherwise = go 0 xs

-- Testen, ob die Zeichenkette nur aus Kleinbuchstaben und Leerzeichen besteht
prop_validCharacters :: SpecialString -> Bool
prop_validCharacters (SpecialString str) = all (`elem` (['a'..'z'] ++ " ")) str
