module TransformerReaderT where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class (lift)



newtype Arr2T e1 e2 m a = Arr2T { getArr2T :: e1 -> e2 -> m a }
newtype Arr3T e1 e2 e3 m a = Arr3T { getArr3T :: e1 -> e2 -> e3 -> m a }


-------------------------------------------------------------------------------
-- Exercise 1
-- | В задачах из предыдущих модулей мы сталкивались с типами данных
{-
newtype Arr2 e1 e2 a = Arr2 { getArr2 :: e1 -> e2 -> a }
newtype Arr3 e1 e2 e3 a = Arr3 { getArr3 :: e1 -> e2 -> e3 -> a }
-}
--  расширить их до трансформеров:
-- >>> (getArr2T $ arr2 (+)) 33 9 :: [Integer]
-- [42]
arr2 :: Monad m => (e1 -> e2 -> a) -> Arr2T e1 e2 m a
arr2 f = Arr2T $ \a b -> return (f a b)

-- >>> (getArr3T $ arr3 foldr) (*) 1 [1..5] :: Either String Integer
-- 120
-- import Data.Functor.Identity
-- >>> runIdentity $ (getArr2T $ arr2 (+)) 33 9
-- 42

arr3 :: Monad m => (e1 -> e2 -> e3 -> a) -> Arr3T e1 e2 e3 m a
arr3 f = Arr3T $ \a b c -> return (f a b c)
-------------------------------------------------------------------------------
-- Exercise 2
-- Сделайте трансформеры  Arr2T, Arr3T представителями класса типов Functor в предположении,
-- что m является функтором:

-- a2l = Arr2T $ \e1 e2 -> [e1,e2,e1+e2]
-- >>> (getArr2T $ succ <$> a2l) 10 100
-- [11,101,111]

-- a3e = Arr3T $ \e1 e2 e3 -> Right (e1+e2+e3)
-- >>> (getArr3T $ sqrt <$> a3e) 2 3 4
-- Right 3.0


instance Functor m => Functor (Arr2T e1 e2 m) where
--    fmap :: (a -> b) -> f a -> f b
    fmap f m2 = Arr2T $ \a b -> fmap f (getArr2T m2 a b)


instance Functor m => Functor (Arr3T e1 e2 e3 m) where
    fmap f m3 = Arr3T $ \a b c -> fmap f (getArr3T m3 a b c)
