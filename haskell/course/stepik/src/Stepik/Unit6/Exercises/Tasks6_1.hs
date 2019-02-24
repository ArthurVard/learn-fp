module Stepik.Unit6.Exercises.Tasks6_1 where

-------------------------------------------------------------------------------
-- Exercise 6.1.1

-- | Сделайте типы данных Arr2 e1 e2 и Arr3 e1 e2 e3 представителями класса типов Functor:


newtype Arr2 e1 e2 a = Arr2 {getArr2 :: e1 -> e2 -> a}
newtype Arr3 e1 e2 e3 a = Arr3 {getArr3 :: e1 -> e2 -> e3 -> a}

instance Functor (Arr2 e1 e2) where
  fmap f (Arr2 g) = Arr2 $ \a b -> f (g a b)

instance Functor (Arr3 e1 e2 e3) where
  fmap f (Arr3 g) = Arr3 $ \a b c -> f (g a b c)

-- >>> getArr2 (fmap length (Arr2 take)) 10 "abc"
-- 3
-- >>> getArr3 (tail <$> tail <$> Arr3 zipWith) (+) [1,2,3,4] [10,20,30,40,50]
-- [33,44]



-------------------------------------------------------------------------------
-- Exercise 6.1.2

-- | Сделайте этот тип функтором и аппликативным функтором с естественной
--   для векторов семантикой покоординатного применения:


data Triple a = Tr a a a  deriving (Eq,Show)

instance Functor Triple where
    fmap f (Tr x y z) = Tr (f x) (f y) (f z)

-- >>> (^2) <$> Tr 1 (-2) 3
-- Tr 1 4 9
-- >>> Tr (^2) (+2) (*3) <*> Tr 2 3 4
-- Tr 4 5 12
instance Applicative Triple where
    pure x = Tr x x x
    Tr f g h <*> Tr x y z = Tr (f x) (g y) (h z)
