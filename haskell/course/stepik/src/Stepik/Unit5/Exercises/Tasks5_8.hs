{-# LANGUAGE OverloadedStrings #-}
module Stepik.Unit5.Exercises.Tasks5_8 where

import Data.Monoid
import Stepik.Unit5.Monads.MonadReader
import Stepik.Unit5.Monads.MonadState
import Stepik.Unit5.Monads.MonadWriter

-------------------------------------------------------------------------------
-- Exercise 5.8.1


-- | Давайте убедимся, что с помощью монады State можно эмулировать монаду Reader.
-- Напишите функцию readerToState, «поднимающую» вычисление из монады Reader в монаду State:


-- evalState (readerToState $ asks (+2)) 4
readerToState :: Reader r a -> State r a
readerToState r = State $ \s ->
                  let a = runReader r s
                  in (a, s)

-------------------------------------------------------------------------------
-- 5.8.2


-- >>> runState (writerToState $ tell "world") "hello,"
-- ((),"hello,world")
-- >>> runState (writerToState $ tell "world") mempty
-- ((),"world")
writerToState :: Monoid w => Writer w a -> State w a
writerToState w = State $ \s ->
                  let (a, l) = runWriter w
                  in (a, s <> l)



-------------------------------------------------------------------------------
-- 5.8.3

fibStep :: State (Integer, Integer) ()
fibStep  = do
  (a,b) <- get
  put (b, a+b)
  return ()

execStateN :: Int -> State s a -> s -> s
execStateN n st s = execState (sequence $ replicate n st) s


fib :: Int -> Integer
fib n = fst $ execStateN n fibStep (0, 1)


-------------------------------------------------------------------------------
-- 5.8.4

data Tree a = Leaf a
            | Fork (Tree a) a (Tree a)
              deriving (Show)

-- | В этой задаче вам дано значение типа Tree (),
-- иными словами, вам задана форма дерева. Требуется пронумеровать
-- вершины дерева данной формы, обойдя их in-order (то есть, сначала
-- обходим левое поддерево, затем текущую вершину, затем правое поддерево):

-- >>> numberTree (Leaf ())
-- Leaf 1
-- >>> numberTree (Fork (Leaf ()) () (Leaf ()))
--- Fork (Leaf 1) 2 (Leaf 3)
numberTree :: Tree () -> Tree Integer
numberTree tree = evalState (tickTree tree) (fromInteger 0)


tickTree (Leaf _) = do
  s <- get
  let s' = s + 1
  put s'
  return $ Leaf s'
tickTree (Fork l _ r) = do
  s <- get
  let s' = s + 1
  put s'
  return $ Fork (evalState (tickTree l) s') s' (evalState (tickTree r) (s' + 1))
