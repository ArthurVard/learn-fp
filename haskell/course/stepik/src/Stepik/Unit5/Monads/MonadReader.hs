{-# LANGUAGE LambdaCase #-}
module Stepik.Unit5.Monads.MonadReader where

import Control.Monad


{-
instance Functor ((->) e) where
  fmap :: (a -> b) -> f a -> f b
  fmap :: (a -> b) -> (e -> a) -> (e -> b)
-- example
 :t fmap (^2) length
 [a] -> Int
 -- the environmnet is list
 -- we will look how to combne such cimputations iwth monads

-- simple reader monad
instance Monad ((->) e) where
    return :: a -> (e -> a)
    return a = \_ -> a

    (>>=) :: (e -> a) -> (a -> (e -> b)) -> (e -> b)
    (>>=) :: (e -> a) -> (a -> e -> b) -> e -> b
    m >>= k  = \e -> k (m e) e

-}

safeHead = do
  b <- null
  if b
  then return Nothing
  else do
    h <- head
    return $ Just h


safeHead' = do
  e <- id
  if null e
  then return Nothing
  else return $ Just (head e)

safeHead'' :: [a] -> Maybe a
safeHead'' =
    null >>= \case
         True -> return Nothing
         False -> Just . head

safeHead''' :: [a] -> Maybe a
safeHead''' =
    id   >>=
    return . null >>= \case -- make arrow of Kleisli from null
                True -> return Nothing
                False -> Just . head


safeHead'''' :: [a] -> Maybe a
safeHead'''' =
    id >>= \case
         [] -> return Nothing
         _ -> Just . head


newtype Reader r a = Reader {runReader :: (r -> a)}

instance Functor (Reader r) where
    fmap f m = Reader $ \e -> f (runReader m e)

instance Applicative (Reader r) where
    pure = return
    m <*> r = Reader $ \e -> let f = runReader m e
                                 a = runReader r e
                             in f a
instance Monad (Reader r) where
    return a = Reader $ \_ -> a
    m >>= k = Reader $ \e -> let a = runReader m e
                             in runReader (k a) e


ask :: Reader r r
ask = Reader id


asks :: (r -> a) -> Reader r a
asks  = Reader
