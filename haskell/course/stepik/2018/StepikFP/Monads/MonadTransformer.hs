{-# LANGUAGE OverloadedStrings #-}
module MonadTransformer where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class (lift)
import Data.Char(toUpper)

-- >>> runReader secondElem ["a", "b"]
secondElem :: Reader [String] String
secondElem = do
  el2 <- asks(map toUpper . head . tail)
  return el2

-- >> runWriter logFirst ["asd", ""adssa1212"]
-- ("ADSSA1212","asd")
logFirst :: [String] -> Writer String String
logFirst xs = do
  let el1 = head xs
      el2 = (map toUpper . head . tail) xs
  tell el1
  return el2


-- միավորենք երկու էֆֆեկտները(ազդեցությունները)
-- >>> runWriter $  runReaderT logFirstAndRetSecond ["asdas", "xcvxc"]
-- runReaderT :: ReaderT r m a -> r -> m a
logFirstAndRetSecond :: ReaderT [String] -- տրանսֆորմեր
                        (Writer String)  -- ներքին մոնադը
                        String           -- մոնադների կոմպոզիցիայի վերադարձվող տիպը
logFirstAndRetSecond = do
  el1 <- asks head
  el2 <- asks (map toUpper . head . tail)
  lift $ tell el1 -- բարձրացնում ենք հաշվարկը ներքին մոնադից
  return el2


-------------------------------------------------------------------------------
-- ex 3.3.1

-- | Перепишите функцию logFirstAndRetSecond из предыдущего видео,
-- используя трансформер WriterT из модуля Control.Monad.Trans.Writer
-- библиотеки transformers, и монаду Reader в качестве базовой

-- >>> runReader (runWriterT logFirstAndRetSecond') strings

logFirstAndRetSecond' :: WriterT String -- տրանսֆորմեր
                        (Reader [String])  -- ներքին մոնադը
                        String           -- մոնադների կոմպոզիցիայի վերադարձվող տիպը
logFirstAndRetSecond' = do
  el1 <- lift $ asks head
  el2 <- lift $ asks (map toUpper . head . tail)
  tell el1 -- բարձրացնում ենք հաշվարկը ներքին մոնադից
  return el2


-- ex 3.3.2
-- >>> (runWriter . runWriterT) $ separate (<3) (>7) [0..10]
-- (([3,4,5,6,7],[0,1,2]),[8,9,10])
-- | Эта функция принимает два предиката и список и записывает в один лог элементы списка,
-- удовлетворяющие первому предикату, в другой лог — второму предикату, а возвращающает
-- список элементов, ни одному из них не удовлетворяющих.
separate :: (a -> Bool) -> (a -> Bool) -> [a] -> WriterT [a] (Writer [a]) [a]
separate p1 p2 xs = do
  tell $ filter p1 xs
  lift . tell $ filter p2 xs
  return $ filter (\x -> (not $ p1 x) && (not  $ p2 x)) xs


-------------------------------------------------------------------------------
-- ex. 3.3.3

type MyRW = ReaderT [String] (Writer String)

logFirstAndRetSecond'' :: MyRW String
logFirstAndRetSecond'' = do
  el1 <- myAsks head
  el2 <- myAsks (map toUpper . head . tail)
  myTell el1
  return el2


myAsks :: ([String] -> a) -> MyRW a
myAsks f = asks f

myTell :: String -> MyRW ()
myTell = lift . tell

runMyRW :: MyRW a -> [String] -> (a, String)
runMyRW rw e = runWriter (runReaderT rw e)


-------------------------------------------------------------------------------
-- ex 3.3.4
-- Превратите монаду MyRW в трансформер монад MyRWT:
-- >>> runMyRWT logFirstAndRetSecond''' ["abc","defg","hij"]
-- First is "abc"
-- Second is "DEFG"
-- ("DEFG","abc")
logFirstAndRetSecond''' :: MyRWT IO String
logFirstAndRetSecond''' = do
  el1 <- myAsks' head
  myLift' $ putStrLn $ "First is " ++ show el1
  el2 <- myAsks' (map toUpper . head . tail)
  myLift' $ putStrLn $ "Second is " ++ show el2
  myTell' el1
  return el2


type MyRWT m = ReaderT [String] (WriterT String m)

runMyRWT :: ReaderT r (WriterT w m) a -> r -> m (a, w)
runMyRWT rwt e = runWriterT (runReaderT rwt e)

myAsks' :: Monad m => ([String] -> a) -> MyRWT m a
myAsks' = asks

myTell' :: Monad m => String -> MyRWT m ()
myTell' = lift . tell

myLift' :: Monad m => m a -> ReaderT [String] (WriterT String m) a
myLift' = lift . lift



-------------------------------------------------------------------------------
-- ex 3.3.5

-- | С помощью трансформера монад MyRWT мы можем написать безопасную версию logFirstAndRetSecond:
-- >>> runMyRWT logFirstAndRetSecondSafe ["abc","defg","hij"]
-- >> Just ("DEFG","abc")
-- >>> runMyRWT logFirstAndRetSecondSafe ["abc"]
-- >> Nothing
logFirstAndRetSecondSafe :: MyRWT Maybe String
logFirstAndRetSecondSafe = do
  xs <- asks id
  case xs of
    (el1 : el2 : _) -> myTell' el1 >> return (map toUpper el2)
    _ -> myLift' Nothing


-- | Реализуйте безопасную функцию veryComplexComputation, записывающую в лог через запятую
-- первую строку четной длины и первую строку нечетной длины, а возвращающую пару из второй
-- строки четной и второй строки нечетной длины, приведенных к верхнему регистру:
-- Подсказка: возможно, полезно будет реализовать функцию myWithReader.
-- >>> runMyRWT veryComplexComputation ["abc","defg","hij"]
-- Nothing
-- >>> runMyRWT veryComplexComputation ["abc","defg","hij","kl"]
-- Just (("KL","HIJ"),"defg,abc")
-- veryComplexComputation :: MyRWT Maybe (String, String)
veryComplexComputation = do
  xs <- ask -- asks id
  case xs of
    (el1 : el2 : _) -> myTell' el1 >> return (map toUpper el2)
    _ -> myLift' Nothing
-- where
 --  go (x:xs) = length x

withMyRWT :: (a1 -> r) -> ReaderT r (WriterT w m) a2 -> a1 -> m (a2, w)
withMyRWT f m = runMyRWT m . f




-------------------------------------------------------------------------------
-- Exercise


-------------------------------------------------------------------------------
-- Exercise


tickCollatz :: State Integer Integer
tickCollatz = do
  n <- get
  let res = if odd n then 3 * n + 1 else n `div` 2
  put res
  return n


tickCollatz' :: StateT (Int, Integer) (Writer [Integer]) Integer
tickCollatz' = do
  (k, n) <- get
  let res = if odd n then 3 * n + 1 else n `div` 2
  lift $ tell [res]
  put (k+1, res)
  if res == 1
  then return res
  else  tickCollatz'
