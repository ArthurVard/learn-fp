{-# LANGUAGE LambdaCase #-}
module Stepik.Unit5.Monads.MonadWriter where

import Control.Monad


newtype Writer w a = Writer { runWriter :: (a, w)}


-- instance Monoid a  Monad ((,) w) where
--    return a = (w,a)

trace :: (String, Int)
trace = do
  a <-  return $ 2
  log' "product by 3"
  b  <- return $ a*3
  log' "plus by 3"
  return $ b + 4


log' :: String -> (String, ())
log' str = (str, ())

tell :: Monoid w => w -> Writer w ()
tell s = Writer ((), s)

instance Functor (Writer w) where
    fmap f m = let (a, l) =  runWriter m
               in Writer (f a, l)

instance Monoid w => Applicative (Writer w) where
    pure a = Writer (a, mempty)
    g <*> m = let (f, l) = runWriter g
                  (a, l') = runWriter m
              in Writer (f a, l <> l')


instance Monoid w => Monad (Writer w) where
    return a = Writer (a, mempty)
    m >>= k = let (a, l) = runWriter m
                  (b, l') = runWriter (k a)
              in Writer (b, l <> l')
