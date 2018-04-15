module JohnHughes.Whyfp where

-- | Glueing Functions Together

-- | List processing examples

data List a = Nil | Cons a (List a) deriving (Show)

append' :: List a -> List a -> List a
append' xs ys = foldr Cons ys xs


range :: Int -> List Int
range 0 = Nil
range n = append' (range (n-1)) (Cons n Nil)

sum' :: List Int -> Int
sum' Nil = 0
sum' (Cons x xs) = x + sum' xs

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Foldable List where

    foldr f z = go
      where
        go Nil = z
        go (Cons x xs ) =  x `f` go xs

    foldl _ z Nil = z
    foldl f z (Cons x xs)= foldl f (f z x) xs

reverse' :: List a -> List a
reverse' =  foldr Cons  Nil

reverse'' :: [a] -> [a]
reverse'' = foldr (:) []

-- |
-- >>> foldr (\acc a -> concat ["(", a, "+", acc, ")"]) "0" $ fmap show (range 10)
-- "((((((((((0+10)+9)+8)+7)+6)+5)+4)+3)+2)+1)"

myfoldr            :: (a -> b -> b) -> b -> [a] -> b
myfoldr k z = go
          where
            go []     = z
            go (y:ys) = y `k` go ys


fib n = take n fib'
  where
    -- fib' :: Int -> Int
    fib' = 1:1:zipWith (+) fib' (tail fib')

-- |
-- >>> reduce (\a b -> concat ["(",a,"+", b, ")"]) "0" $ fmap show ls
-- "(1+(2+(3+(4+(5+(6+(7+(8+(9+(10+0))))))))))"
-- >>> foldr (\a b -> concat ["(",a,"+", b, ")"]) "0" $ fmap show ls
-- "(1+(2+(3+(4+(5+(6+(7+(8+(9+(10+0))))))))))"
-- >>> foldl (\a b -> concat ["(",a,"+", b, ")"]) "0" $ fmap show ls
-- "((((((((((0+1)+2)+3)+4)+5)+6)+7)+8)+9)+10)"

reduce :: (a -> b -> b) -> b -> List a -> b
reduce f z Nil = z
reduce f z (Cons x xs) = f x (reduce f z xs)

ls :: List Int
ls = range 10

prod :: List Int -> Int
prod = reduce (*) 0

sum :: List Int -> Int
sum = reduce (+) 0

doubleall :: List Int -> List Int
doubleall = reduce doubleandcons Nil
  where
    doubleandcons :: Int -> List Int -> List Int
    doubleandcons num list = Cons (2*num) list


doubleandcons = fandcons double
  where double n = 2*n
        fandcons f el list = Cons (f el) list

doubleall' = reduce (Cons . double) Nil
  where
    double n = 2 * n

map :: (a->b) -> List a -> List b
map f = reduce (Cons . f) Nil


-- tree processing examples
data Tree a = Node a (List(Tree a)) deriving (Show)


-- redetree function analogous to reduce Recall that
-- reduce took two arguments, something to replace cons with, and something to
-- replace nil with. Since trees are built using node, cons and nil, redtree must
-- take three arguments - something to replace each of these with. Since trees and
-- lists are of different types, we will have to define two functions, one operating
-- on each type.

-- f replaces Node
-- g replaces Cons
-- z replaces Nil

redtree :: (a -> b -> c) -> (c -> b -> b) -> b -> Tree a -> c
redtree f g z (Node label subtrees) = f label (redtree' f g z subtrees)

redtree' :: (a -> b -> c) -> (c -> b -> b) -> b -> List (Tree a) -> b
redtree' f g z (Cons subtree rest) = g (redtree f g z subtree) (redtree' f g z rest)
redtree' f g z Nil = z

{--
Node 1
  (Cons (Node 2 Nil)
   (Cnns (Node 3
          (Cons (Node 4 Nil) Nil))
    Nil))
--}
tree :: Tree Integer
tree =
  Node 1
  (Cons (Node 2 Nil)
   (Cons (Node 3
          (Cons (Node 4 Nil) Nil))
    Nil))

add :: Integer -> Integer -> Integer
add = (+)

sumtree :: Tree Integer -> Integer
sumtree = redtree add add 0

-- | Taking the tree we wrote down earlier as an example, sumtree gives
{--
add 1
  (add (add 2 0)
   (add (add 3
         (add (add 4 0) 0))
    0))
--}


-- | A list of all the labels in a tree can be computed using

labels :: Tree a -> List a
labels = redtree Cons append' Nil
-- | The same example gives
{--
Cons 1
  (append' (Cons 2 Nil)
   (append' (Cons 3
            (append' (Cons 4 Nil) Nil))
    Nil))
--}


-- | Finally, one can define a function analogous to map which applies a function f
-- to all the labels in a tree:

maptree :: (a -> a1) -> Tree a -> Tree a1
maptree f = redtree (Node . f) Cons Nil

{--
(Node . f) 1
  (Cons ( (Node . f) 2 Nil)
   (Cnns ( (Node . f) 3
          (Cons ((Node . f) 4 Nil) Nil))
    Nil))
--}


-- | -------------------------------------------------------------------

-- | 4 Glueing Programs Together

-- 4.1 Newton-Raphson Square Roots

-- | This algorithm computes the square root of a number N by starting
-- from an initial approximation a0 and computing better and better ones using
-- the rule a(n+1) = (a(n) + N/a(n)) / 2
