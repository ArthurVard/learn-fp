{-# LANGUAGE LambdaCase #-}
module Stepik.Unit6.Applicatives.ApplicativeExamples where


-- | LIst as Applicative
--
-- semantic:
-- there 2 semantics
-- non determinisitc calculation, each with each
-- like zipp

fs = [\x -> 2 * x, \x -> 3 + x, \x -> 4 - x]
xs = [1,2]


-- | [2,4,4,5,3,2]
ap1 = fs <*> xs

{-
-- the standard implementation is

instance Applicative [] where
   pure x = [x]
   gs <*> xs = [ g x| g < gs, x <- xs ]
-}



-- | another semantix of applicative for [] type

newtype ZipList a = ZipList {getZipList :: [a]}

instance Functor ZipList where
    fmap f (ZipList xs) = ZipList (fmap f xs)

instance Applicative ZipList where
    pure x = ZipList [x]
    ZipList gs <*> ZipList xs = ZipList $ zipWith ($) gs xs


-- this implementation breaks the applicative first low, maybe others as well
-- if we change pure to
-- pure x = XipList (repeat x)
-- then all lows will be hold


-- | Either as applicative
{-
instance Applicative (Either e) where
   pure           = Right
   Left e <*> _   = Left e
   Right f <*>  r = fmap f r
-}


-- | (,) as  applicative

{-
instance Monoid e => Appicative ((,) e) where
   pure x = (mempty, x)
   (u, g) <*> (v, x) = (u `mappend` v, g x)
-}
