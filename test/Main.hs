module Main where

import BiProductionsLib
import Test.QuickCheck
import ClassyPrelude

prop_decodedEncodedInt :: Int -> Bool
prop_decodedEncodedInt x = x == decodeBitList (encodeBitList x)

genSmallInt :: Gen Int
genSmallInt = abs `fmap` (arbitrary :: Gen Int) `suchThat` (\x -> x > 0 && x < 256)

main :: IO ()
main = do
    quickCheck $ forAll genSmallInt $ prop_decodedEncodedInt
    