
{-# LANGUAGE GADTs, DataKinds #-}
{-# LANGUAGE  ExistentialQuantification  #-}
{-# LANGUAGE KindSignatures #-}
module Queens where


import Control.Monad

safe :: (Num a, Enum a, Eq a) => a -> [a] -> p -> Bool
safe p [] _ = True
safe q qs n   = and [q /= c && q /= c + r && q /= c - r | (r,c) <- zip [1..] qs]

queens :: (MonadPlus m, Enum a, Eq t, Eq a, Num t, Num a) => t -> m [a]
queens 0 = return []
queens n = do
  qs <- queens (n-1)
  q <- foldr1 mplus (map return [1..8])
  if safe q qs 8
  then return (q:qs)
  else mzero


-- queens1 :: (MonadPlus m, Enum a, Eq t, Eq a, Num t, Num a) => t -> m [a]
queens1 0 = return []
queens1 n = do
  queens1 (n-1) >>= \qs ->
--      foldr1 mplus (map return [1..8]) >>= \q ->
--      [[1],[2],[3],[4],[5],[6],[7],[8]] >>= \q ->
        [1..3] >>= \q -> return (q:qs)
--           if safe q qs 8
  --         then return (q:qs)
    --       else mzero



newtype Wrap a = Wrap a

data STy ty
    = (ty ~ Int) => SInt
    | (ty ~ Bool ) => SBool
    | (ty ~ () ) => SUnit
    | forall a b . (ty ~ (a -> b)) => SFun (a -> b)
    | forall ty' . (ty ~ Maybe ty') => SMaybe  (Maybe ty')
    | forall ty' . (ty ~  [ty']) => SList  [ty']
    | (ty ~  Wrap ty) => SWrap  ty

-- | A type-indexed representation of a type
{-
data STy ty where
  SIntTy   :: STy Int
  SBoolTy  :: STy Bool
  SMaybeTy :: STy a -> STy (Maybe a)
-}

zero :: STy ty -> ty
zero SInt = 0
zero SBool = False
zero (SMaybe _) = Nothing
zero (SList _) = []
-- zero (SWrap a) = Wrap (zero a)
zero (SFun f) = f


data Nat = S Nat | Z

data MyMaybe a = N | J a


-- https://www.schoolofhaskell.com/user/k_bx/playing-with-datakinds
data JobDescription = JobOne { n :: Int }
                    | JobTwo
                    | JobThree { n :: Int }
  deriving (Show, Eq)

taskOneWorker :: JobDescription -> IO ()
taskOneWorker t = do
    putStrLn $ "n: " ++ (show $ n t)


main :: IO ()
main = do
  -- this runs ok:
  taskOneWorker (JobOne 10)

  -- this fails at runtime:
  taskOneWorker JobTwo

  -- this works, but we didn't want it to:
  -- taskOneWorker (JobThree 10)
