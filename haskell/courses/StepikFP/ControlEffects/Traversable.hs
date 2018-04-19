module StepikFP.ControlEffects.Traversable where

import Data.Foldable
import Data.Traversable
import Control.Applicative


data Tree a = Nil | Branch (Tree a) a (Tree a) deriving (Eq, Show)

instance Foldable Tree where
    foldr f z Nil = z
    foldr f z (Branch l x r) = f x (foldr f (foldr f z r) l)


instance Functor Tree where
    fmap f Nil = Nil
    fmap f (Branch l x r) = Branch (fmap f l) (f x) (fmap f r)
