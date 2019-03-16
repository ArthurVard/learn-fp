{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Stepik.Unit8.Exercises.Tasks8_1 where


-- import Control.Monad.Trans.Except
import Control.Monad
import Stepik.Unit8.MonadAndEffects.ExceptMonad
import Text.Read

-------------------------------------------------------------------------------
-- Exercise 8.1.1

-- | Реализуйте функцию , позволящую, если произошла ошибка, применить к ней заданное преобразование.
withExcept :: (e -> e') -> Except e a -> Except e' a
withExcept f (Except x) = except $ case x of
                                     Left e  -> Left (f e)
                                     Right x -> Right x

-------------------------------------------------------------------------------
-- Exercise 8.1.2

data ListIndexError = ErrIndexTooLarge Int | ErrNegativeIndex
  deriving (Eq, Show)


-- >>> runExcept $ [1..100] !!! 5
-- Right 6
-- >>> (!!!!) xs n = runExcept $ xs !!! n
-- >>> [1,2,3] !!!! 0
-- Right 1
-- >>> [1,2,3] !!!! 42
-- Left (ErrIndexTooLarge 42)
-- >>> [1,2,3] !!!! (-3)
-- Left ErrNegativeIndex

(!!!) :: [a] -> Int -> Except ListIndexError a
xs !!! n | n < 0 = except .Left $ ErrNegativeIndex
         | otherwise = elemAt xs n n

elemAt (x:xs) 0 _ = except $ Right x
elemAt [] _ k     = except . Left $ ErrIndexTooLarge k
elemAt (x:xs) n k = elemAt xs (n - 1) k

(!!!!!) :: [a] -> Int -> Except ListIndexError a
xs !!!!! n | n < 0 = except . Left $ ErrNegativeIndex
         | n >= length xs = except . Left $ ErrIndexTooLarge n
         | otherwise = except . Right $ xs !! n


-------------------------------------------------------------------------------
-- Exercise 8.1.3

data ReadError = EmptyInput | NoParse String
  deriving Show

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
tryRead str | null str = throwE EmptyInput
            | otherwise = case readMaybe str of
                            Nothing -> throwE $ NoParse str
                            Just n  -> return n


-------------------------------------------------------------------------------
-- Exercise 8.1.4

data SumError = SumError Int ReadError
  deriving Show
-- EmptyInput | NoParse String
re2sume :: Int ->  ReadError -> SumError
re2sume n EmptyInput  = SumError n EmptyInput
re2sume n (NoParse s) = SumError n (NoParse s)

-- >>> runExcept $ trySum ["10", "20", "30"]
-- Right 60
-- >>> runExcept $ trySum ["10", "20", ""]
-- Left (SumError 3 EmptyInput)
-- >>> runExcept $ trySum ["10", "two", "30"]
-- Left (SumError 2 (NoParse "two"))

-- trySum :: [String] -> Except SumError Integer
-- trySum :: [String] -> [Except ReadError Int]
trySum :: [String] -> Except ReadError Int
-- trySum xs = foldr (pure (1)) (except $ Right 0) (map tryRead xs)
trySum =  undefined

-- runExcept . withExcept re2sume

-------------------------------------------------------------------------------
-- Exercise 8.1.5

newtype SimpleError = Simple { getSimple :: String }
  deriving (Eq, Show)


-- >>> toSimple = runExcept . withExcept lie2se
-- >>> xs = [1,2,3]
-- >>> toSimple $ xs !!! 42
-- Left (Simple {getSimple = "[index (42) is too large]"})
-- >>> toSimple $ xs !!! (-2)
-- Left (Simple {getSimple = "[negative index]"})
-- >>> toSimple $ xs !!! 2
-- Right 3
-- >>> import Data.Foldable (msum)
-- >>> toSimpleFromList = runExcept . msum . map (withExcept lie2se)
-- >>> toSimpleFromList [xs !!! (-2), xs !!! 42]
-- Left (Simple {getSimple = "[negative index][index (42) is too large]"})
-- >>> toSimpleFromList [xs !!! (-2), xs !!! 2]
-- Right 3


-- data ListIndexError = ErrIndexTooLarge Int | ErrNegativeIndex


lie2se :: ListIndexError -> SimpleError
lie2se (ErrIndexTooLarge n) = Simple $ "[index (" ++ (show n) ++ ") is too large]"
lie2se ErrNegativeIndex     = Simple "[negative index]"


instance Semigroup SimpleError where
    Simple s1 <> Simple s2 = Simple (s1 ++ s2)

instance Monoid SimpleError where
    mempty = Simple ""
    mappend = (<>)

toSimple :: Except ListIndexError a -> Either SimpleError a
toSimple = runExcept . withExcept lie2se

xs = [1,2,3]

toSimpleFromList :: [Except ListIndexError a] -> Either SimpleError a
toSimpleFromList = runExcept . msum . map (withExcept lie2se)


-----------------------------------------------------------------
-- Exercise


newtype Validate e a = Validate { getValidate :: Either [e] a }



collectE :: Except e a -> Validate e a
collectE = undefined

validateSum :: [String] -> Validate SumError Integer
validateSum = undefined

-- >>> getValidate $ validateSum ["10", "20", "30"]
-- Right 60
-- >>> getValidate $ validateSum ["10", "", "30", "oops"]
-- Left [SumError 2 EmptyInput,SumError 4 (NoParse "oops")]
