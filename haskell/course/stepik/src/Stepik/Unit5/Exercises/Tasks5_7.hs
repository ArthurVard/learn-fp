{-# LANGUAGE OverloadedStrings #-}
module Stepik.Unit5.Exercises.Tasks5_7 where

import Data.Monoid
import Stepik.Unit5.Monads.MonadWriter

-------------------------------------------------------------------------------
-- Exercise 5.7.2

type Shopping = Writer (Sum Integer) ()


main = total shopping

shopping :: Shopping
shopping = do
  purchase "Jeans"   19200
  purchase "Water"     180
  purchase "Lettuce"   328



purchase :: String -> Integer -> Shopping
purchase name cost = Writer ((), Sum cost)


total :: Shopping -> Integer
total  = getSum . snd . runWriter


-- | Измените определение типа Shopping и доработайте функцию purchase из предыдущего
-- задания таким образом, чтобы можно было реализовать функцию items, возвращающую список
-- купленных товаров (в том же порядке, в котором они были перечислены при покупке):


-------------------------------------------------------------------------------
-- Exercise 5.7.3

type ShoppingE = Writer ((Sum Integer), [String]) ()


mainE = do
  print $ totalE shoppingE
  print $ items shoppingE

shoppingE :: ShoppingE
shoppingE = do
  purchaseE "Jeans"   19200
  purchaseE "Water"     180
  purchaseE "Lettuce"   328



purchaseE :: String -> Integer -> ShoppingE
purchaseE name cost = Writer ((), (Sum cost,  [name]))


totalE :: ShoppingE -> Integer
totalE  = getSum . fst . snd . runWriter


items :: ShoppingE -> [String]
items  = snd . snd . runWriter
