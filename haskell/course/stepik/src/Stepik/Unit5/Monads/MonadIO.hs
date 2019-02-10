{-# LANGUAGE LambdaCase #-}
module Stepik.Unit5.Monads.MonadIO where

import Control.Monad

------------------------------------------------------------
--

main = do
  putStrLn "What is your name"
  name <- getLine
  putStrLn $ "Hello, " ++ name ++ "!"


{-
type IO a = RealWorld -> (RealWorld, a)

return :: a -> IO a
return :: a -> RealWorld -> (RealWorld, a)

(>>=) :: IO a -> (a -> IO b) -> IO b
(>>=) :: RealWorld -> (RealWorld, a)
      -> (a -> RealWorld -> (RealWorld, a))
      -> RealWorld -> (RealWorld, a)


instance Monad IO where
    return a = \a -> (w, a)

    (>>=) m k = \w -> case m w of (w', a) -> k a w'

-}

getLine' :: IO String
getLine' = do
  c <- getChar
  if c /= '\n'
  then  do
    cs <- getLine'
    return (c:cs)
  else return  []


getLine'' :: IO String
getLine'' = do
  c <- getChar
  True <- return (c /= '\n')
  cs <- getLine'
  return (c:cs)


getLine''' :: IO String
getLine''' =
  getChar >>= \c ->
  return (c /= '\n') >>= \case
         True -> getLine' >>= \cs -> return (c:cs)
         _ -> return []



putStr' :: String -> IO ()
putStr' []     = return ()
putStr' (x:xs) = putChar x >> putStr' xs


putStr'' :: String -> IO ()
putStr'' []     = return ()
putStr'' (x:xs) = do
  putChar x
  putStr'' xs



-------------------------------------------------------------------------------
-- Helper functions

{-
sequence_ :: (Foldable t, Monad m) => t (m a) -> m ()
sequence_ :: Monad m => [m a] -> m ()
sequence_ = foldr (>>) (return ())
-}

putStr_ :: String -> IO ()
putStr_ = sequence_ . map putChar


{-
mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
mapM_ f = sequence_ . map f
-}

putStrM_ :: String -> IO ()
putStrM_ = mapM_ putChar


{-
sequence :: (Traversable t, Monad m) => t (m a) -> m (t a)
sequence :: Monad m => [m a] -> m [a]
sequence ms = foldr k (return []) ms
  where
    k :: Monad m => m a -> m [a] -> m [a]
    k m m' = do
      x <- m
      xs <- m'
      return (x:xs)

mapM :: (Foldable t, Monad m) => (a -> m b) -> t a -> m b
mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f = sequence . map f

-}
