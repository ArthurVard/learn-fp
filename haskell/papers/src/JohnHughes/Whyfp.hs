{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- https://www.youtube.com/watch?v=Z35Tt87pIpg
-- https://github.com/Risto-Stevcev/haskell-church-encodings/blob/master/RankNTypes/Church.hs
-- https://en.wikipedia.org/wiki/Church_encoding
module JohnHughes.Whyfp where


-- UnicodeSyntax #-
-- decr :: (∀ α . Nat α) -> Nat a

import Prelude hiding (succ, pred, and, or, not, exp, div, head, tail)

-- | Church logical operator

type ChurchBool = forall a. a -> a -> a
{-
-- Church Boolean (True)
-- λt.λf.t
true :: ChurchBool
true = \t -> \f -> t

false :: ChurchBool
false x y = y

and p q = p q false

or p q = p true q

ifte bool a b = bool a b


type NumType = (Int -> Int) -> Int -> Int

instance Show NumType where
    show n = show $ n (+1) 0

-- zero :: NumType
zero f x = x

-- one :: NumType
one f x = f x

 -- two :: NumType
two f x = f ( f x)

-- >>> add two two
-- 4
add m n f x = m f (n f x)

-- >>> mul (add two two) two
-- 8
mul m n f x = m (n f) x

-- factorial
fact :: (forall a. (a -> a) -> a -> a ) -> (a -> a) -> a -> a
fact n =
  ifte (isZero n)
       one
       (mul n (fact (decr n)))


isZero n = n (\_ -> false) true
decr
  :: (Num t1, Num a) =>
     ((((((a -> a) -> t1 -> t2) -> t2) -> (p1 -> p2 -> p2) -> t3)
       -> (t3 -> t4) -> p3 -> t4)
      -> (p4 -> p5 -> p5) -> (p6 -> p6) -> (p7 -> p8 -> p8) -> t5)
     -> t5

decr n =
    n (\m f x -> f (m incr zero))
      zero
      (\x -> x)
      zero

incr n = n (+1) 1
-}


type Logical a = a -> a -> a
type Nat a = (a->a) -> a->a

-- boolean
true, false :: Logical a
true x y = x
false x y = y

ifte :: Logical a -> a -> a -> a
ifte = id

incr :: Nat a -> Nat a
incr n f = f . n f

-- integer “literals”
zero, one, two, three :: Nat a
three = incr two
two   = incr one
one   = incr zero
zero _ = id

-- addition and multiplication
add, mul :: Nat a -> Nat a -> Nat a
add m n f = m f . n f
mul m n f = m $ n f

-- zero check
isZero :: (forall a. Nat a) -> Logical a
isZero n = n (const false) true


type ANat = forall a . Nat a

fact :: ANat -> Nat a
fact n = ifte (isZero n)
      one
      (mul n $ fact (decr n))

decr :: ANat -> Nat a
decr n =
    n (\m f x -> f (m incr zero))
      zero
      (\x -> x)
      zero
