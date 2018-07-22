module StepikFP.ControlEffects.TraversableLaws where

import Test.QuickCheck
import Test.QuickCheck.Function

import Data.Functor.Identity
import Data.Functor.Compose

-- http://www.cs.uu.nl/research/techreps/repo/CS-2012/2012-008.pdf
-- http://austinrochford.com/posts/2014-05-27-quickcheck-laws.html
-- type PropLeftUnit m = Integer -> Fun Integer (m Integer) -> Bool

prop_LeftUnit x (Fun _ f) = (return x >>= f) == f x

-- type PropRightUnit m = m Integer -> Bool

prop_RightUnit m = (m >>= return) == m

-- type PropAssoc m = m Integer -> Fun Integer (m Integer) -> Fun Integer (m Integer) -> Bool


prop_Assoc m (Fun _ f) (Fun _ g) = ((m >>= f) >>= g) == (m >>= \x -> f x >>= g)
-- >>> quickCheck prop_MaybeAssoc
prop_MaybeAssoc :: Maybe Integer ->
                   Fun Integer (Maybe Integer) ->
                   Fun Integer (Maybe Integer) ->
                   Bool
prop_MaybeAssoc = prop_Assoc

-- | 1. Law Identity
-- traverse without effect could not change container
prop_TraversableId m = traverse Identity m == Identity m

prop_1 :: [Int] -> Bool
prop_1 = prop_TraversableId

-- | 2. Composition
--
prop_TraversableCompose (Fun _ g1) (Fun _ g2) m = traverse ( Compose . fmap g2 . g1) m == Compose (fmap (traverse g2) (traverse g1 m))

prop_MaybeTraversableCompose
    :: Fun Integer (Maybe Integer) ->
       Fun Integer (Maybe Integer) ->
       Maybe Integer ->
       Bool
prop_MaybeTraversableCompose = prop_TraversableCompose

prop_ListTraversableCompose
    :: Fun Int [Int] ->
       Fun Int [Int] ->
       [Int] ->
       Bool
prop_ListTraversableCompose = prop_TraversableCompose

-- | (3) naturality
-- t . traverse g == traverse (t . g)

-- | (4) sequnceA . fmap Identity == Identity
-- | (5) sequnceA . fmap Compose == Compose . fmap sequnceA . sequnceA -- composition
-- | (6) t . sequnceA == sequnceA . fmap t
-------------------------------------------------------------------------------
-- Exercise


prop_Rev, prop_RevRev :: [Integer] -> Bool

prop_Rev xs = reverse xs == xs

-- >>> quickCheck prop_RevRev
-- >>> quickCheck prop_Rev
prop_RevRev xs = reverse (reverse xs) == xs
