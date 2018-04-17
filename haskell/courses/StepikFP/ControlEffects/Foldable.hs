module StepikFP.ControlEffects.Foldable where

import Prelude hiding(Foldable, foldr, foldl, fold, foldMap, sum, product)
import Data.Foldable hiding(Foldable, foldl, foldr, fold, foldMap, sum, product)
import Data.Monoid
import Data.Char(isDigit)

class Foldable t where
    foldr :: (a -> b -> b) -> b -> t a -> b
    foldl :: (b -> a -> b) -> b -> t a -> b

    fold  :: Monoid m => t m -> m
    fold  = foldr mappend mempty

    foldMap :: Monoid m => (a -> m) -> t a -> m
    -- foldMap f cont = fold (fmap f cont) wrong
    foldMap f cont = foldr (mappend . f) mempty cont

    sum :: Num a => t a -> a
    sum = getSum . foldMap Sum

    product :: Num a => t a -> a
    product = getProduct . foldMap Product

    null :: t a -> Bool
    null = foldr(\_ _ -> False) True

    toList :: t a -> [a]
    toList = foldr (:) []

    length :: t a -> Int
    length = foldr (\_ n -> 1 + n) 0

    maximum :: Ord a => t a -> a
    minimum :: Ord a => t a -> a
    elem :: Eq a => t a -> Bool

-- | Foldable instance for []
instance Foldable [] where
    foldr f z [] = z
    foldr f z (x:xs) = x `f` (foldr f z xs)

    foldl f z [] = z
    foldl f z (x:xs) = foldl f (f z x) xs


-- | Foldable instance for Maybe type
-- >>> foldr (*) 3 (Just 14)
-- 42
instance Foldable Maybe where
    foldr f z Nothing = z
    foldr f z (Just x) = f x z

    foldl f z Nothing = z
    foldl f z (Just x) = f z x


-- | Foldable instance for Either type
-- >>> foldr (*) 3 (Right 14)
-- 42
-- >>> foldr (*) 3 (Left 14)
-- 3
instance Foldable (Either a) where
    foldr f z (Left _) = z
    foldr f z (Right x) = f x z

    foldl f z (Left _) = z
    foldl f z (Right x) = f z x


-- | Foldable instance for (,) type
-- >>> foldr (*) 3 (13,14)
-- 42
-- >>> foldr (*) 3 ("Ok",14)
-- 42
instance Foldable ((,) a) where
    foldr f z (x, y) = f y z

    foldl f z (x, y) = f z y


-- ex 1. Сделайте тип представителем класса типов Foldable:
data Triple a = Tr a a a  deriving (Eq,Show)

-- >>> foldr (++) "!!" (Tr "ab" "cd" "efg")
-- "abcdefg!!"
-- >>> foldl (++) "!!" (Tr "ab" "cd" "efg")
-- "!!abcdefg"
instance Foldable Triple where
    foldr f z (Tr a b c) = f a (f b (f c z))

    foldl f z (Tr a b c) = f (f (f z a) b) c




-- | Tree


data Tree a = Nil | Branch (Tree a) a (Tree a) deriving (Eq, Show)

testTree = Branch (Branch (Branch Nil 1 Nil) 2 (Branch Nil 3 Nil)) 4 (Branch Nil 5 Nil)
{-
      4
     / \
    2   5
   / \
  1   3
-}

-- >>> foldr (\t z -> concat [show t, "->",  z]) "end"  testTree
-- [4,2,1,3,5] - pre-order from left to right
-- [4,5,2,3,1] - pre-order from right to left
-- [1,2,3,4,5] - in-order from left to right
-- [5,4,3,2,1] - in-order from right to left
instance Foldable Tree where
    foldr f z Nil = z
--    foldr f z (Branch l x r) = f x (foldr f (foldr f z r) l) -- pre-order  from left  to right
--    foldr f z (Branch l x r) = f x (foldr f (foldr f z l) r) -- pre-order  from right to left
    foldr f z (Branch l x r) = (\i -> (foldr f i l)) . f x . (\i -> (foldr f i r)) $ z
--    foldr f z (Branch l x r) = foldr f (f x (foldr f z r)) l -- in-order   from left  to right
--    foldr f z (Branch l x r) = foldr f (f x (foldr f z l)) r -- in-order   from right to left
--    foldr f z (Branch l x r) = foldr f ((foldr f z r)) l   -- post-order from left  to right
--    foldr f z (Branch l x r) = foldr f (f x (foldr f z l)) r -- post-order from right to left

    foldl f z Nil = z
    foldl f z (Branch l x r) = f (foldl f (foldl f z l) r) x


treeToList :: Tree a -> [a]
treeToList = foldr (:) []


-- Ex 2. https://stepik.org/lesson/%D0%9A%D0%BB%D0%B0%D1%81%D1%81-%D1%82%D0%B8%D0%BF%D0%BE%D0%B2-Foldable-30427/step/6?unit=11044

newtype Preorder a   = PreO   (Tree a)    deriving (Eq, Show)
newtype Postorder a  = PostO  (Tree a)    deriving (Eq, Show)
newtype Levelorder a = LevelO (Tree a)    deriving (Eq, Show)

{-
      3
    /  \
  1     4
    \
      2
-}
tree = Branch (Branch Nil 1 (Branch Nil 2 Nil)) 3 (Branch Nil 4 Nil)
-- >>> foldr (:) [] tree
-- [1,2,3,4]
-- >>> foldr (:) [] $ PreO tree
-- [3,1,2,4]
-- >>> foldr (:) [] $ PostO tree
-- [2,1,4,3]
-- >>> foldr (:) [] $ LevelO tree
-- [3,1,4,2]
instance Foldable Preorder where
    foldr f z (PreO Nil) = z
--    foldr f z (PreO (Branch l x r)) = f x (foldr f (foldr f z r) l)
    foldr f i (PreO (Branch l x r)) = x' . l' . r' $ i
        where l' = \acc -> foldr f acc (PreO l)
              r' = \acc -> foldr f acc (PreO r)
              x' = f x


    foldl f z (PreO Nil) = z
    foldl f z (PreO (Branch l x r)) = f (foldl f (foldl f z r) l) x


instance Foldable Postorder where
    foldr f z (PostO Nil) = z
    foldr f z (PostO (Branch l x r)) = foldr f (foldr f (f x z) (PostO r)) (PostO l)

    foldl f z (PostO Nil) = z
    foldl f z (PostO (Branch l x r)) = f (foldl f (foldl f z r) l) x


instance Foldable Levelorder  where
    foldr f z (LevelO Nil) = z
    foldr f z (LevelO (Branch l x r)) = (\acc -> foldr f acc l) . f x . (\acc -> foldr f acc r) $ z

    foldl f z (LevelO Nil) = z
    foldl f z (LevelO (Branch l x r)) = foldl f (f (foldl f z r) x) l


-- from comments
flatTree :: Tree a -> [a]
flatTree Nil = []
flatTree (Branch l x r) =
 --  flatTree l ++ [x] ++ flatTree r  -- In-order   -- foldr f (f x (foldr f z r)) l
-- [x] ++ flatTree l ++ flatTree r  -- Pre-order  -- f x (foldr f (foldr f z r) l)
 flatTree l ++ flatTree r ++ [x]  -- Post-order -- foldr f (foldr f (f x z) r) l


-- | Monoid
sumM :: Sum Integer
sumM = foldMap Sum [1,2,3,4]

prodM ::  Product Integer
prodM = foldMap Product [1,2,3,4]

treeM = foldMap Sum tree
