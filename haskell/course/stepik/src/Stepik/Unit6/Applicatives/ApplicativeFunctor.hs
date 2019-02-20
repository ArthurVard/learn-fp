{-# LANGUAGE LambdaCase #-}
module Stepik.Unit6.Applicatives.ApplicativeFunctor where

import Prelude hiding ((<*>))
{-
-- Functor Lows
1. fmap id m  == m
2. fmap f (fmap g m) == fmap (f . g) m
-}


{-
map _ []     = []
map g (x:xs) = g x : map g xs

fmap = map
-- 1. Low
fmap id [] = [] -- fmap def

-- induction step: IH: for xs the assumption is true
fmap id (x:xs)  -- def map
(id x) : fmap id xs -- def id
x : fmap id xs -- IH
x : xs


-- 2. Low
fmap f (fmap g xs) = fmap (f . g) xs
-- [] case
- fmap f (fmap g []) -- map def
  fmap f [] -- map def
  []

- fmap (f . g) [] -- def map
  []


- fmap f (fmap g (x:xs)) -- map def
  fmap f (g x : fmap g xs) -- map def
  f (g x) : fmap f (fmap g xs)
  (f . g) x :

- fmap (f . g) (x:xs) -- def map
  (f . g) x : fmap (f . g) xs



-}


-- | Applicatives

class Functor m => Pointed m where
    pure :: a -> m a -- aka singleton, return, unit, point

instance Pointed Maybe where
    pure x = Just x


instance Pointed [] where
    pure x = [x]


instance Pointed (Either s) where
    pure x = Right x


instance Pointed ((->) e) where
    -- pure :; a -> ((->) e a) == a -> e -> a
    -- pure  = const
    -- pure x e = x
    pure x = \_ -> x


instance Monoid s => Pointed ((,) s) where
    pure x = (mempty, x)

{-
  закон для касса типа Pointed

  fmap g (pure x) == pure (g x)
-}


-- class Functor m =>



{-
fmap2arg :: Functor f => (a -> b -> c) -> f a -> f b -> f c
fmap3arg :: Functor f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
-}
fmap2arg :: Functor f => (a -> b -> c) -> f a -> f b -> f c
fmap2arg g as bs = undefined


infixl 4 <*>

class Functor f => Apply f where
    (<*>) :: f (a -> b) -> f a -> f b

instance Apply [] where
    (g:gs) <*> (x:xs) = g x : (gs <*> xs)
    _ <*> _ = []



fmap2 :: Apply f => (a -> b -> c) -> f a -> f b -> f c
fmap2 g as bs = fmap g as <*> bs
fmap3 :: Apply f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
fmap3 g as bs cs = fmap g as <*> bs <*> cs


{-
class Functor f => Applicative f where
   pure  :: a -> f a
   (<*>) :: f (a -> b) -> f a -> f b

instance Applicative Maybe where
   pure = Just
   Nothing <*> _ = Nothing
   (Just g) <*> x = fmap g x

-- the Low of connection Functor and Applicative

fmap g cont == pure g  <*> cont


-- Applicative Lows

-- 1 - 3 releated to non-effect essence of pure
1. Identity Low
pure id <*> v == v

2. Homomorphism Low
pure g <*> pure x == pure (g x)

3. Interchange Low
cont <*> pure x == pure ($ x) <*> cont


-- associative
4. Compossition
pure (.) <*> u <*> v <*> cont == u <*> (v <*> cont)



-}




{-
instance Applicative ((->) e) where
   pure a = \_ -> a
   (<*>) :: (e -> (a -> b)) -> (e -> a) -> (e -> b)
    (<*>) g h = \e -> g e (h e)


-}
