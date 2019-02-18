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
