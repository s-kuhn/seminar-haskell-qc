module MyListArbitrary
    ( List(..)
    , toList
    , toMyList
    ) where

import Test.QuickCheck

data List a = Nil | Cons a (List a) deriving (Eq, Show)

-- Die Arbitrary Instanz für List a
instance Arbitrary a => Arbitrary (List a) where
  arbitrary = oneof [return Nil, Cons <$> arbitrary <*> arbitrary]

toList :: List a -> [a]
toList Nil = []
toList (Cons x xs) = x : toList xs

toMyList :: [a] -> List a
toMyList [] = Nil
toMyList (x:xs) = Cons x (toMyList xs)