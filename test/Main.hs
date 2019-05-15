module Main where

import BiProductionsLib()
import Test.QuickCheck
import ClassyPrelude

prop_revapp :: [Int] -> [Int] -> Bool
prop_revapp xs ys = reverse (xs++ys) == reverse xs ++ reverse ys

main :: IO ()
main = do
    quickCheck prop_revapp