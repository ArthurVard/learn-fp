{-# LANGUAGE LambdaCase #-}
module Stepik.Unit5.Monads.MonadState where

import Control.Monad

----------------------------------------------------------------
--  Monad State


newtype State s a = State {runState :: s -> (a, s)}


instance Functor (State s) where
    fmap f st = State $  \s -> let (a,s') = runState st s
                            in (f a, s')


instance Applicative (State s) where
    pure a = State $ \s -> (a, s)
    g <*> m = State $ \s -> let  (f, s') = runState g s
                                 (a, s'') = runState m s'
                            in (f a, s'')


instance Monad (State s) where
    return a = State $ \s -> (a, s)
    m >>= k  = State $ \s -> let (a, s') = runState m s
                                 m' = k a
                             in runState m' s'



get :: State s s
get = State $ \s -> (s, s)


put :: s -> State s ()
put s = State $ \_ -> ((), s)

execState :: State s a -> s -> s
execState st s = snd $ runState st s

evalState :: State s a -> s -> a
evalState st s = fst $ runState st s

tick :: State Int Int
tick = do
  n <- get
  put (n+1)
  return n


modify :: (s -> s) -> State s ()
modify f = do --  State $ \s -> ((), f s)
  s <- get
  put (f s)


succ' :: Int -> Int
succ'  = execState tick

plus n x = execState (sequence $ replicate n tick) x


{-
replicateM :: (Monad m) => Int -> m a -> m [a]
replicateM n = sequence . replicate n
-}
