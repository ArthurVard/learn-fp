{-# LANGUAGE OverloadedStrings #-}
module Lib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"


data List a = Nil | Cons a (List a) deriving (Show)

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving (Show)


xs = foldr (\x acc -> "(" ++ show x ++ "+" ++ acc ++ ")") "0" [1..10]
ys = foldl (\acc x -> "(" ++ show x ++ "+" ++ acc ++ ")" ) "0" [1..10]



-- Strats iterview by https://pneron.github.io/

-- Included libs:
-- bytestring hashtables parsec split text unordered-containers vector vector-algorithms hunit hspec
--
-- Code is run with runghc



data JVal = JNull
     | JObject [(String, JVal)]
     | JArr [JVal]
     | JInt Int
     | JStr String
 deriving (Show)

type A a = ((Either String Int) -> a -> a)
type B a = (Int -> a -> a,String -> a -> a)
type C a = (Either (String -> a -> a) (Int  -> a -> a))

-- A ~ B
-- B ~ C ?

{-
g :: (Int -> a -> a,String -> a -> a) -> ((Either String Int) -> a -> a)
g (fi, fs) e = case e of
                Left s -> fs s
                Right i -> fi i

h :: ((Either String Int) -> a -> a) -> ( Int -> a -> a ,String -> a -> a)
h f = (fi,fs)
   where fi :: Int -> a -> a
         fi i = f (Right i)
         fs s = f (Left s)
-}

-- (Int -> a -> a,String -> a -> a)
foldJson :: ((Either String Int) -> a -> a) -> JVal -> a -> a

foldJson f (JStr s)  z = f (Left s) z
foldJson f (JInt i)  z = f (Right i) z
foldJson f (JArr js) z = foldl (\acc x -> foldJson f x acc) z js

f :: String -> Int
f  = length

ex =  (either ((+) . f)  (+))
test = foldJson ex (JArr [JInt 1, JInt 3, JStr "asd"]) 0
{- Object or array of json or a number or a string or null
-}

main1nn = putStrLn "Hello, Arthur!"

