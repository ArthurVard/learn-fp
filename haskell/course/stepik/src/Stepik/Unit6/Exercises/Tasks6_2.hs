{-# LANGUAGE RankNTypes #-}
module Stepik.Unit6.Exercises.Tasks6_2 where

import Control.Applicative                           ((<**>))
import Stepik.Unit6.Applicatives.ApplicativeExamples
-------------------------------------------------------------------------------
-- Exercise 6.2.1



x1s = [1,2,3]
x2s = [4,5,6]
x3s = [7,8,9]
x4s = [10,11,12]
{-
В модуле Data.List имеется семейство функций zipWith, zipWith3, zipWith4,..:
GHCi> zipWith (\a b -> 2*a+3*b) x1s x2s
[14,19,24]
GHCi> zipWith3 (\a b c -> 2*a+3*b+5*c) x1s x2s x3s
[49,59,69]
GHCi> zipWith4 (\a b c d -> 2*a+3*b+5*c-4*d) x1s x2s x3s x4s
[9,15,21]
-}

-- >>> getZipList $ (\a b -> 2*a+3*b) <$> ZipList x1s <*> ZipList x2s

-- >>> getZipList $ (\a b c -> 2*a+3*b+5*c) <$> ZipList x1s <*> ZipList x2s <*> ZipList x3s

-- >>> getZipList $ (\a b c d -> 2*a+3*b+5*c-4*d) <$> ZipList x1s <*> ZipList x2s <*>ZipList x3s <*> ZipList x4s


-- | Реализуйте операторы (>*<) и (>$<),
-- позволяющие спрятать упаковку ZipList и распаковку getZipList:

-- >>> (\a b -> 2*a+3*b) >$< x1s >*< x2s

-- >>> (\a b c -> 2*a+3*b+5*c) >$< x1s >*< x2s >*< x3s

-- >>> (\a b c d -> 2*a+3*b+5*c-4*d) >$< x1s >*< x2s >*< x3s >*< x4s

(>$<) :: (a -> b) -> [a] -> [b]
(>$<) f xs = getZipList $  f <$> ZipList xs

(>*<) :: [a -> b] -> [a] -> [b]
(>*<) f xs = getZipList $ ZipList f <*> ZipList xs


-------------------------------------------------------------------------------
-- Exercise 6.2.2

divideList :: Fractional a => [a] -> a
divideList []     = 1
divideList (x:xs) = (/) x (divideList xs)


-- | такую что последовательность вычислений отражается в логе:

-- >>> divideList' [3,4,5]
-- ("<-3.0/<-4.0/<-5.0/1.0",3.75)
divideList' :: (Show a, Fractional a) => [a] -> (String,a)
divideList' []     = ("1.0",1.0)
divideList' (x:xs) = (/) <$> ("<-" ++ show x ++ "/",x) <*>  (divideList' xs)



-------------------------------------------------------------------------------
-- Exercise 6.2.3

newtype Arr2 e1 e2 a = Arr2 { getArr2 :: e1 -> e2 -> a }

newtype Arr3 e1 e2 e3 a = Arr3 { getArr3 :: e1 -> e2 -> e3 -> a }


-- >>> getArr2 (Arr2 (\x y z -> x+y-z) <*> Arr2 (*)) 2 3
-- -1

-- >>> getArr3 (Arr3 (\x y z w -> x+y+z-w) <*> Arr3 (\x y z -> x*y*z)) 2 3 4
-- -15

instance Functor (Arr2 e1 e2) where
    fmap f  (Arr2 h) = Arr2 $ \e1 e2 -> f ( h e1 e2)

instance Applicative (Arr2 e1 e2) where
    pure a = Arr2 $ \_ _ -> a
    (Arr2 f) <*> (Arr2 h) = Arr2 $ \e1 e2 -> f e1 e2 (h e1 e2)


instance Functor (Arr3 e1 e2 e3) where
    fmap f  (Arr3 h) = Arr3 $ \e1 e2 e3 -> f ( h e1 e2 e3)

instance Applicative (Arr3 e1 e2 e3) where
    pure a = Arr3 $ \_ _ _ -> a
    (Arr3 f) <*> (Arr3 h) = Arr3 $ \e1 e2 e3 -> f e1 e2 e3 (h e1 e2 e3)




-------------------------------------------------------------------------------
-- Exercise 6.2.4

{-
В следующих шести примерах вашей задачей будет привести такие контрпримеры
для стандартных типов данных, для которых они существуют.
Следует заменить аппликативное выражение в предложении in на выражение
того же типа, однако дающее разные результаты при вызовах с
(<??>) = (<**>) и (<??>) = (<*?>). Проверки имеют вид exprXXX (<**>) == exprXXX (<*?>)
для различных имеющихся XXX. Если вы считаете, что контрпримера
не существует, то менять ничего не надо.

infixl 4 <**>
(<**>) :: Applicative f => f a -> f (a -> b) -> f b
(<**>) = liftA2 (flip ($))
-}


infixl 4 <*?>
(<*?>) :: Applicative f => f a -> f (a -> b) -> f b
(<*?>) = flip (<*>)

exprMaybe :: (forall a b . Maybe a -> Maybe (a -> b) -> Maybe b) -> Maybe Int
exprMaybe op =
  let (<??>) = op
      infixl 4 <??>
  in Just 5 <??> Just (+2) -- place for counterexample

exprList :: (forall a b . [a] -> [a -> b] -> [b]) -> [Int]
exprList op =
  let (<??>) = op
      infixl 4 <??>
  in [1,2] <??> [(+3),(+6)] -- place for counterexample

exprZipList :: (forall a b . ZipList a -> ZipList (a -> b) -> ZipList b) -> ZipList Int
exprZipList op =
  let (<??>) = op
      infixl 4 <??>
  in ZipList [1,3, 5] <??> ZipList [(\a  -> a - 3), (+ 4), id]  -- place for counterexample

exprEither :: (forall a b . Either String a -> Either String (a -> b) -> Either String b) -> Either String Int
exprEither op =
  let (<??>) = op
      infixl 4 <??>
  in Left "AA" <??> Right (+1)  -- place for counterexample

exprPair :: (forall a b . (String,a) -> (String,a -> b) -> (String,b)) -> (String,Int)
exprPair op =
  let (<??>) = op
      infixl 4 <??>
  in ("AA", 3) <??> ("BB",(+1))  -- place for counterexample

exprEnv :: (forall a b . (String -> a) -> (String -> (a -> b)) -> (String -> b)) -> (String -> Int)
exprEnv op =
  let (<??>) = op
      infixl 4 <??>
  in length <??> (\_ -> (+5))  -- place for counterexample


test_all = [ exprMaybe (<**>) == exprMaybe (<*?>)
           , exprList (<**>) == exprList (<*?>)
           , exprZipList(<**>) == exprZipList (<*?>)
           , exprEither (<**>) == exprEither (<*?>)
           , exprPair (<**>) == exprPair (<*?>)
     --      , exprEnv (<**>) == exprEnv (<*?>)
           ]

-- run_all = sequence test_all
