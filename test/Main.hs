module Main where

import BiProductionsLib
import Test.QuickCheck
import ClassyPrelude

prop_decodedEncodedInt :: Int -> Bool
prop_decodedEncodedInt x = x == decodeBitList (encodeBitList x)

prop_padOrdinalLength :: Int -> [Int] -> Bool
prop_padOrdinalLength size l = length (padOrdinal size l) == size 

genSmallInt :: Gen Int
genSmallInt = abs `fmap` (arbitrary :: Gen Int) `suchThat` (\x -> x > 0 && x < 256)

main :: IO ()
main = do
    quickCheck $ forAll genSmallInt prop_decodedEncodedInt
    quickCheck $ forAll genSmallInt prop_padOrdinalLength
    