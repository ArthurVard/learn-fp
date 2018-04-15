{-# LANGUAGE EmptyDataDecls
  , TypeOperators  #-}

module Algebra where

-- Things
data Unit = Unit deriving Show -- number one, one value
data Void  -- zero has any constractor, imposible to construct value

data a :+ b = AddL a | AddR b deriving Show -- set(a) + set(b) (Either type)

data a :* b = Mul a b -- set(a) * set(b)


data Boll = False | True
type Two  = Unit :+ Unit


{-
Notation
 Void => 0
 Unit => 1
 Bool => 2
 Addition => a + b
 Multiplication => a * b
-}


-- | Laws
-- 0 + x = x
-- Either Void x ~ x -- semantic equality

-- 0 * x     = 0
-- (Void, x) ~ Void

-- 1 * x   = x
-- ((), x) ~ x


-- x + y = y + x
-- Either x y ~ Either y x


-- X * y = Y * x
-- (x, y) ~ (y, x)


-- | Functions
--f ::  Domain -> Range
data a :-> b =  (->) a  b

-- count of functions from Bool -> Bool is 4
-- size(Range) ^ size(Domain) functions


-- Function Laws

-- a :-> Unit ~ Unit -- 1 ^ a = 1

-- Unit -> a ~ a     -- a ^ 1 = a

-- a -> (b,c) ~ (a -> b, a -> c) -- (b :* c) ^ a = b ^ a :* c ^ a

-- curry-uncurry
-- (a,b) -> c ~ a -> b -> c  -- c ^ (b * a) = (c ^ b) ^ a


-- | Recursive Types

-- Lists
data List x = Nil | Cons x (List x)

{-
-- translation to algebra
list either unit which is empty, something with one constructor
with no values or (+) is a value of  a head  of the list and then
another list aftewards that
L(x) = 1 + x * L(x)
L = 1 + x * L
to solve
1) L = 1 + x ( 1 + x L)
   L = 1 + x + x^2 ( 1 + x L)
   L = 1 + x + x^2 + x^3 + x^4 + ...

2) L (1 - x) = 1
   L = 1 / (1 - x) -- tales theorem
   L = 1 + x + x^2 + x^3 + x^4 + ...

-}


-- Trees
data Tree x = Tip | Node (Tree x) x (Tree x)
{-
Algebra translation

T = 1 +  x T^2
x T^2 - T + 1 = 0 -- Quadratic equation
T = (1 - sqrt(1 - 4x)) / 2x - what is power series of this
T = 1 + x + 2x^2 + 5x^3 + 14x^4 + ...

-}



-- | Problem
{-
Navigate and modify a data  structure (e.g. a list) efficiently
Solution: The zipper
-}

xs = [1,2,3,4,5,6]

-- Zipper
{-
have focus element and data
where you are
https://www.st.cs.uni-saarland.de/edu/seminare/2005/advanced-fp/docs/huet-zipper.pdf
-}
data Zip a = Zip [a] a [a]

-- z = Zip [2,1] 3 [4,5,6]
-- >>> right z
-- Zip [3,2,1] 4 [5,6]
-- >>> right z
-- Zip [4,3,2,1] 5 [6]
right :: Zip a -> Zip a
right = undefined

-- Use example in XMonad


-- | One-Hole Context

-- One-hole context: Data Srtucture with a hole
-- [1,2,3] * [5,6]
