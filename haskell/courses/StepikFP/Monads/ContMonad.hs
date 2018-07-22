{-# LANGUAGE FlexibleContexts #-}
module ContMonad where

-- >>> decode one hundred twenty three as a number
-- >>> decode one hundred twenty three as a number
-- 123
-- >>> decode one hundred twenty one as a number
-- 121
-- >>> decode one hundred twenty as a number
-- 120
-- >>> decode one hundred as a number
-- 100
-- >>> decode three hundred as a number
-- 300
-- >>> decode two thousand seventeen as a number
-- 2017
-- decode :: Int -> (Int -> Int) -> Int
decode x c = c x
as x c = x


a x c = x

number = id

one :: Int -> (Int -> Int) -> Int
one x c = c $ 1 + x
two x c = c $ x + 2
three x c = c $ x +  3
seventeen x c = c $ x +  17
twenty x c = c $ x + 20
hundred x c = c $ x * 100
thousand x c = c $ x * 1000
