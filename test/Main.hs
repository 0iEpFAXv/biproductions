module Main where

import ClassyPreludeTemplateLib()
import Test.QuickCheck
import ClassyPrelude

prop_revapp :: [Int] -> [Int] -> Bool
prop_revapp xs ys = reverse (xs++ys) == reverse xs ++ reverse ys

main :: IO ()
main = quickCheck prop_revapp