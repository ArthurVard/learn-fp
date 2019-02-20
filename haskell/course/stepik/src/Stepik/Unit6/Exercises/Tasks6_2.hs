module Stepik.Unit5.Exercises.Tasks6_2 where

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
