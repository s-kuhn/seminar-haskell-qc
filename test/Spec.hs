import Test.QuickCheck
import MyListArbitrary

main :: IO ()
main = verboseCheck (prop_toList :: List Int -> Bool)

prop_toList :: Eq a => List a -> Bool
prop_toList lst = lst == toMyList (toList lst)