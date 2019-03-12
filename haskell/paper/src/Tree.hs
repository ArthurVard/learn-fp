module Tree where

import           Control.Monad
import           JohnHughes.MonadLows
import           Test.QuickCheck

data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = frequency [(2, liftM Leaf arbitrary),
                           (1, liftM2 Branch arbitrary arbitrary)
                          ]


shrink' (Branch l r) = [l,r] ++ map (Branch l) (shrink' r) ++ map (`Branch`r) (shrink' l)
shrink' (Leaf a) = map Leaf (shrink a)


prop_TreeLeftUnit = prop_LeftUnit :: PropLeftUnit Tree
prop_TreeRightUnit = prop_RightUnit :: PropRightUnit Tree
prop_TreeAssoc = prop_Assoc :: PropAssoc Tree

treeMap :: (t -> a) -> Tree t -> Tree a
treeMap f (Leaf a)     = Leaf (f a)
treeMap f (Branch l r) = Branch (treeMap f l) (treeMap f r)

instance Functor Tree where
    fmap = treeMap

instance Applicative Tree where
    pure a = Leaf a
    Leaf f <*> t = fmap f t
    Branch l r <*> Leaf a = Branch (l <*> (Leaf a)) (r <*> (Leaf a))
    Branch l r <*> Branch l' r' = Branch (l <*> l') (r <*> r')


instance Monad Tree where
    return = pure
    (Leaf a) >>= k = k a
    Branch l r >>= k = Branch (l >>= k) (r >>= k)

t1 = Branch (Branch (Branch (Leaf 0) (Branch (Leaf 0) (Leaf 0))) (Branch (Leaf 0) (Leaf 0)))
            (Branch (Branch (Leaf 0) (Branch (Leaf 0) (Leaf 0))) (Branch (Leaf 0) (Leaf 0)))

tf = Branch (Branch (Leaf (*4)) (Branch (Leaf (*2)) (Leaf (*10)))) (Branch (Leaf (*5)) (Leaf (+1)))
appTest = tf <*> t1
monadTest = tf >>= \f ->  t1
