module StateMonad where

import Control.Monad
import Test.QuickCheck

newtype State s a = MkState {runState :: s -> (a,s)}
instance Functor (State s) where
    fmap f m = MkState $ \s ->
               let (a, s') = runState m s
               in (f a, s')

instance Applicative (State s) where
    pure a = MkState $ \s -> (a, s)
    g <*> m = MkState $ \s ->
              let (f, s') = runState g s
                  (a, s'') = runState m s'
              in (f a, s'')

instance Monad (State s) where
    return x = MkState (\s -> (x,s))
    MkState f >>= g = MkState $ \s ->
                      let (a,s') = f s
                      in runState (g a) s'

get :: State s s
get = MkState $ \s -> (s,s)


put :: s -> State s ()
put s = MkState $ \_ -> ((), s)


(===:) :: Eq a => State Integer a -> State Integer a -> Integer -> Bool
(f ===: g) s = runState f s == runState g s


prop_get_get =
  (get >>= \x ->
  get >>= \y ->
  return (x,y)) ===: (get >>= \x -> return (x,x))
