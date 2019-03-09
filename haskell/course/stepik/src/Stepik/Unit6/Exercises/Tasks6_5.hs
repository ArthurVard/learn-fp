{-# LANGUAGE TypeOperators #-}
module Stepik.Unit6.Exercises.Tasks6_5 where

import Control.Applicative

import Stepik.Unit6.Applicatives.CompositionOnTypes

-------------------------------------------------------------------------------
-- Exercise 6.5.1

type A   = ((,) Integer |.| (,) Char) Bool

type B t = ((,,) Bool (t -> t) |.| Either String) Int

type C   = (|.|) ((->) Bool) ((->) Integer) Integer


-- :t A => (|.|) ((,) Integer) ((,) Char) Char
-- :t B => (|.|) ((,,) Bool (t -> t)) (Either String) Int
-- :t C => (|.|) ((->) Bool) ((->) Integer) Integer

a :: A
a = Cmps (1, ('a', False))

b :: B Int
b = Cmps (True, (+2), Left "dara")


c :: C
-- c = Cmps (\x -> \y -> 4)

c = Cmps (\x -> (+2))

-- c = Cmps $ flip const


-----------------------------------------------------------------
-- Exercise 6.5.2

newtype Cmps3 f g h a = Cmps3 { getCmps3 :: f (g (h a)) }
  deriving (Eq,Show)

-- >>> fmap (^2) $ Cmps3 [[[1],[2,3,4],[5,6]],[],[[7,8],[9,10,11]]]
-- Cmps3 {getCmps3 = [[[1],[4,9,16],[25,36]],[],[[49,64],[81,100,121]]]}

-- | представителем класса типов Functor при условии,
-- что первые его три параметра являются функторами:
instance (Functor f, Functor g, Functor h) => Functor (Cmps3 f g h) where
    fmap k (Cmps3 x) = Cmps3 $ fmap (fmap (fmap k)) x


-----------------------------------------------------------------
-- Exercise 6.5.3

unCmps3 :: Functor f => (f |.| g |.| h) a -> f (g (h a))
unCmps3 =  (fmap getCmps) . getCmps
unCmps4 :: (Functor f2, Functor f1) => (f2 |.| f1 |.| g |.| h) a -> f2 (f1 (g (h a)))
-- unCmps4 = fmap (fmap getCmps) . unCmps3
unCmps4 = fmap (fmap getCmps) . unCmps3

-- >>> pure 42 :: ([] |.| [] |.| []) Int
-- Cmps {getCmps = [Cmps {getCmps = [[42]]}]}
-- >>> unCmps3 (pure 42 :: ([] |.| [] |.| []) Int)
-- [[[42]]]
-- >>> unCmps3 (pure 42 :: ([] |.| Maybe |.| []) Int)
-- [Just [42]]
-- >>> unCmps4 (pure 42 :: ([] |.| [] |.| [] |.| []) Int)
-- [[[[42]]]]
