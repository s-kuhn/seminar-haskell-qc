module Main (main) where

import StringArbitrary
import Test.QuickCheck

main :: IO ()
main = sample stringGen
