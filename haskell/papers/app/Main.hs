module Main where

import Lib
import Test.QuickCheck
import Test.QuickCheck.Function


main :: IO ()
main = do
    quickCheck prop_Rev
    quickCheck prop_RevRev
    quickCheck prop_MaybeAssoc



prop_LeftUnit x (Fun _ f) = (return x >>= f) == f x
prop_RightUnit m = (m >>= return) == m
prop_Assoc m (Fun _ f) (Fun _ g) =  ((m >>= f) >>= g) == (m >>= \x -> f x >>= g)

prop_MaybeAssoc :: Maybe Integer ->
                   Fun Integer (Maybe Integer) ->
                   Fun Integer (Maybe Integer) ->
                   Bool
prop_MaybeAssoc = prop_Assoc


prop_Rev, prop_RevRev :: [Integer] -> Bool
prop_Rev xs = reverse xs == xs
prop_RevRev xs = reverse (reverse xs) == xs
