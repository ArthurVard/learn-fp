{-# LANGUAGE FlexibleContexts #-}
module StepikFP.Monads.ExceptMonad where

import Control.Monad (liftM, ap, MonadPlus(mzero,mplus), guard, msum)
import Control.Applicative(Alternative(empty, (<|>)))
import Text.Read
import Control.Monad.Trans.Except

{-
newtype Except e a = Except {runExcept :: Either e a} deriving Show


except :: Either e a -> Except e a
except = Except


throwE :: e -> Except e a
throwE = except . Left




instance Functor (Except e) where
    fmap = liftM

instance Applicative (Except e) where
    pure = return
    (<*>) = ap

instance Monad (Except e) where
    return a =  Except $ Right  a
    m >>= k = case runExcept m of
                Left e -> except $ Left e
                Right a -> k a



withExcept :: (e -> e') -> Except e a -> Except e' a
withExcept f ex  = case (runExcept ex) of
                     Left e -> except $ Left (f e)
                     Right a -> except $ Right a



catchE :: Except e a -> (e -> Except e' a) -> Except e' a
m `catchE` h =
    case runExcept m of
      Left e ->  h e
      Right r -> except (Right r)

-}
{-
usage
do {action1; action2; action3} `catchE` handler'

Law:
catchE h (throwE e) = h e

-}

-- Example

data DivByError = ErrZero String | ErrOther deriving (Show)

(/?) :: Double -> Double -> Except DivByError Double
x /? 0 = throwE $ ErrZero (show x ++ "/0;")
x /? y = return $ x / y

example0 :: Double -> Double -> Except DivByError String
example0 x y = action `catchE` handler
    where
      action = do
        q <- x /? y
        return $ show q
      handler = \err -> return $ show err


-------------------------------------------------------------------------------
-- Exercise 2.

data ListIndexError = ErrIndexTooLarge Int | ErrNegativeIndex deriving (Eq, Show)


-- >>> runExcept $ [1..100] !!! 5
-- Right 6
-- >>> (!!!!) xs n = runExcept $ xs !!! n
-- >>> [1,2,3] !!!! 0
-- Right 1
-- >>> [1,2,3] !!!! 42
-- Left (ErrIndexTooLarge 39)
-- >>> [1,2,3] !!!! (-3)
-- Left ErrNegativeIndex
infixl 9 !!!
(!!!) :: [a] -> Int -> Except ListIndexError a

xs !!! n = if n < 0
           then  throwE  ErrNegativeIndex
           else case (xs, n) of
                  ([], n) -> throwE (ErrIndexTooLarge n)
                  ((x:xs), 0) ->  return x
                  ((x:xs), n) -> xs !!! (n - 1)


-------------------------------------------------------------------------------
-- Exercise 3.


data ReadError = EmptyInput | NoParse String deriving Show

-- >>> runExcept (tryRead "5" :: Except ReadError Int)
-- Right 5
-- >>> runExcept (tryRead "5" :: Except ReadError Double)
-- Right 5.0
-- >>> runExcept (tryRead "5zzz" :: Except ReadError Int)
-- Left (NoParse "5zzz")
-- >>> runExcept (tryRead "(True, ())" :: Except ReadError (Bool, ()))
-- Right (True,())
-- >>> runExcept (tryRead "" :: Except ReadError (Bool, ()))
-- Left EmptyInput
-- >>> runExcept (tryRead "wrong" :: Except ReadError (Bool, ()))
-- Left (NoParse "wrong")

tryRead :: Read a => String -> Except ReadError a
tryRead str = if null str
              then throwE EmptyInput
              else case readMaybe str of
                     Nothing -> throwE $  NoParse str
                     Just n -> return n


-------------------------------------------------------------------------------
-- Exercise 4.

data SumError = SumError Int ReadError deriving Show

-- >>> map runExcept ((map (tryRead) ["12", "23", ""] ) :: [Except ReadError Int])

-- >>> runExcept $ trySum ["10", "20", "30"]

-- >>> runExcept $ trySum ["10", "20", ""]

-- >>> runExcept $ trySum ["10", "two", "30"]


{-
trySum :: [String] -> Except SumError Int
trySum xs = withExcept (\e -> SumError 2 e) (go 1 xs)
    where
      go n (x:xs) = case runExcept tryRead x of
                      Right n -> n + go (n + 1) xs
                      Left e  -> Left e         --  `catchE` handler
--      handler = \err -> withExcept (\e -> SumError 2 e) err
-}
