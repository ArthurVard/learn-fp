{-# LANGUAGE ViewPatterns #-}
module StepikFP.ControlEffects.FunctorLaws where

import Prelude hiding (Functor, fmap)
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck.Property
--import Test.QuickCheck.Property.Common.Internal

{-
The Functor class is used for types that can be mapped over.
Instances of Functor should satisfy the following laws:

> fmap id id
> fmap (f ◦ g) fmap f ◦ fmap g
-}


class Functor f where
  fmap :: (a -> b) -> f a -> f b


instance Functor IO where
  fmap h f = f >>= (pure . h)

instance Functor ((->) e) where
  fmap = (.)

data T a = T

prop_functorid :: (Functor f, Eq (f a)) => T (f a) -> f a -> Bool
prop_functorid T x = fmap id x == x

prop_FunctorId :: (Functor f, Eq (f a)) => T (f a) -> f a -> Bool
prop_FunctorId T f = f == fmap id f


prop_functorcompose :: (Functor f, Eq (f c)) => T (f a) -> T b -> T c -> f a -> Fun a b -> Fun b c -> Bool
prop_functorcompose T T T x (apply -> g) (apply -> h) =
  fmap (h . g) x == (fmap h . fmap g) x

instance Show (a -> b) where
  show a= "function"

{-prop_function :: IO ()
prop_function = do
  quickCheck $ prop_functorid (T :: T (String -> String))
  quickCheck $ prop_functorcompose (T :: T (String -> String)) (T :: T String) (T :: T String)

prop_io :: IO ()
prop_io = do
  quickCheck $ prop_functorid (T :: T (IO String))
  quickCheck $ prop_functorcompose (T :: T (IO String)) (T :: T String) (T :: T String)
-}
