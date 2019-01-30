module Stepik.Unit5.Exercises.Tasks5_4 where

-------------------------------------------------------------------------------
-- Exercise 5.4.2

-- |
-- Пусть имеется тип данных, который описывает конфигурацию шахматной доски:
--
-- data Board = ...
-- Кроме того, пусть задана функция
-- nextPositions :: Board -> [Board]
-- которая получает на вход некоторую конфигурацию доски и возвращает все возможные конфигурации,
-- которые могут получиться, если какая-либо фигура сделает один ход. Напишите функцию:
-- nextPositionsN :: Board -> Int -> (Board -> Bool) -> [Board]
-- которая принимает конфигурацию доски, число ходов n, предикат и возвращает все возможные
-- конфигурации досок, которые могут получиться, если фигуры сделают n ходов и которые
-- удовлетворяют заданному предикату. При n < 0 функция возвращает пустой список.

-- dummy definitions
data Board = Board Int deriving Show

nextPositions :: Board -> [Board]
nextPositions (Board i) = [Board (i + 1), Board (i + 2)]

-- solution A

nextPositionsN :: Board -> Int -> (Board -> Bool) -> [Board]
nextPositionsN b n pred =  nextPositionsNKleisli n pred b

-- | filp args to make Kleisli
nextPositionsNKleisli :: Int -> (Board -> Bool) ->  Board -> [Board]
nextPositionsNKleisli n pred b
          | n < 0 = []
          | n == 0 = filter pred [b]
          | otherwise = nextPositions b >>= nextPositionsNKleisli (n - 1) pred


nextPositionsN' :: Board -> Int -> (Board -> Bool) -> [Board]
nextPositionsN' b n pred
  | n < 0 = []
  | n == 0 = filter pred [b]
  | otherwise = do
    xs <- nextPositions b
    ys <- nextPositionsN' xs (n - 1) pred
    return ys

{-
-- | try convert recursive function on list tp list comprehension, not sure it is possible
nextPositionsN'' b n pred = [ z | xs <- nextPositions b,
                                   y <- xs,
                                   z <- nextPositionsN'' y (n-1) pred, pred z]

-}






-------------------------------------------------------------------------------
-- Exercise 5.4.3

-- |
-- Используя монаду списка и do-нотацию, реализуйте функцию
-- pythagoreanTriple :: Int -> [(Int, Int, Int)]
-- которая принимает на вход некоторое число x и возвращает список троек (a,b,c), таких что
-- a2+b2=c2,a>0,b>0,c>0,c≤x,a<b
-- Число x может быть ≤0 , на таком входе должен возвращаться пустой список.
--
-- GHCi> pythagoreanTriple 5
-- [(3,4,5)]
--
-- GHCi> pythagoreanTriple 0
-- []
--
-- GHCi> pythagoreanTriple 10
-- [(3,4,5),(6,8,10)]
