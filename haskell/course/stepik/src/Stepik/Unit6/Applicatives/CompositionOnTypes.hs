{-# LANGUAGE TypeOperators #-}

module Stepik.Unit6.Applicatives.CompositionOnTypes where
import Control.Applicative (liftA2)

-- | the composition of functors also Functor
infixr 9 |.|

newtype (|.|) f g a = Cmps {getCmps :: f(g a) } deriving (Eq, Show)

-- λ> :t Cmps (Just "asd")
-- Cmps (Just "asd") :: (|.|) Maybe [] Char


instance (Functor h, Functor g) => Functor ((|.|) h g) where
    -- fmap (a -> b) -> (f |.| g) a -> (f |.| g) b
    fmap f (Cmps x) = Cmps $ fmap (fmap f) x

{-
x :: f (g a)
phi :: g a -> g b
fmap phi x :: f (g b)
fmap h :: g a -> g b
-}

newtype Matrix a = Matrix [[a]]

m = [[1..3],[1..3]]

double_m = fmap (*2) $ Cmps m


-- | the Lows of Functors is true for composition of Functors

{-
1. fmap id cont <=> cont
   fmap id cont <=> id cont
   fmap id      <=> id

fmap id (Cmps x)             -- def fmap (Cmps)
== Cmps $ fmap (fmap id) x   -- (1) fmap (g)
== Cmps $ fmap id x          -- (1) fmap (f)
== Cmps x

2. fmap h2 (fmap h1 (Cmps x)) = fmap (h2 . h1) (Cmps x)

fmap h2 (fmap h1 (Cmps x))
== (fmap h2 . fmap h1) (Cmps x)     -- function composition
== (fmap (h2 . h1)) (Cmps x)        -- the (1) low for functors h1 and h2
== fmap (h2 . h1) (Cmps x)


-}

-- | is the composition of 2 applicative functors is still applicative functor ?
-- Yes

instance (Applicative f, Applicative g) => Applicative (f |.| g) where
    -- pure :: a -> (|.|) f g a
    pure a = Cmps $ (pure . pure)  a
    -- (<*>) :: (|.|) f g (a -> b) -> (|.|) f g a -> (|.|) f g b
    (Cmps h) <*> (Cmps a) = Cmps $ fmap (<*>) h <*>  a
--    Cmps f1 <*> Cmps f2 = Cmps $ liftA2 (<*>) f1 f2
{-
h :: f (g (a -> b))
a :: f (g a)
left <*>           :: g (a -> b)     -> (g a -> g b)
fmap (<*>)         :: f (g (a -> b)) -> f (g a -> g b)
fmap (<*>) h       ::                   f (g a -> g b)
a                  :: f (g a)
fmap (<*>) h <*> a :: f (g b)



-}
