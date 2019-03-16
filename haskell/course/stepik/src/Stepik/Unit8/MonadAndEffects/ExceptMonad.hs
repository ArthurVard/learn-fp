{-# LANGUAGE TypeOperators #-}

module Stepik.Unit8.MonadAndEffects.ExceptMonad where
import Data.Monoid ((<>))

import Control.Applicative (Alternative(empty, (<|>)))
import Control.Monad       (MonadPlus(mplus, mzero), ap, guard, liftM, msum)


newtype Except e a = Except {runExcept :: Either e a}
    deriving (Show)

except :: Either e a -> Except e a
except = Except


instance Functor (Except e) where
    fmap f (Except x) = Except (fmap f x)
{-
    fmap f me = except $ case runExcept me of
                           Left e  -> Left e
                           Right a -> Right (f a)
-}

   -- fmap  = liftM
instance Applicative (Except e) where
    pure = except . Right
    (Except f) <*> (Except x) = Except (f <*> x)
{-
    mf <*> me = let f = runExcept mf
                in  case f of
                      Left e  -> except $ Left e
                      Right g -> fmap g me
-}
   -- pure = return
   -- (<*>) = ap

instance Monad (Except e) where
    return = pure
    m >>= k = let x = runExcept m
              in case x of
                   Left e  -> except $ Left e
                   Right x -> k x


-- | how rise exception
throwE :: e -> Except e a
throwE = except . Left

-- pass params monad and handler
catchE :: Except e a -> (e -> Except e' a) -> Except e' a
m `catchE` h =
    case runExcept m of
      Left e  -> h e
      Right r -> except (Right r)


{-
-- usage:
do {action1; action2; action3} `catchE` handler

-- Low:
catchE (throwE e) h <=> h e
-}


-- | example

data DivByError = ErrZero String | ErrOther deriving (Eq,Show)

(/?) :: Double -> Double -> Except DivByError Double
x /? 0 = throwE $ ErrZero ( show x ++ "/0;")
x /? y = return $ x / y

example0 :: Double -> Double -> Except DivByError String
example0 x y = action `catchE` handler where
    action = do
      q <- x /? y
      return $ show q
    handler = \err -> return $ show err



-- | MonadPlus

instance Monoid e => Alternative (Except e) where
    empty = mzero
    (<|>) = mplus


instance Monoid e => MonadPlus (Except e) where
    mzero = except $ Left mempty
    Except x `mplus` Except y = Except $
                                case x of
                                  Left e -> either (Left . mappend e) Right y
                                  r      -> r

instance Semigroup DivByError where
    ErrZero s1  <> ErrZero s2 = ErrZero (s1 ++ s2)
    ErrZero s1 <> ErrOther = ErrZero s1
    ErrOther <> ErrZero s2 = ErrZero s2
    ErrOther <> ErrOther = ErrOther

instance Monoid DivByError where
    mempty = ErrOther
    mappend = (<>)




example2 :: Double -> Double -> Except DivByError String
example2 x y = action `catchE` handler where
    action = do
      q <- x /? y
      guard $ y >= 0
      return $ show q
    handler (ErrZero s) = return s
    handler ErrOther    =  return "NONNEGATIVE GUARD"


-- runExcept $ msum [2/?0, 4/?0]
-- Left (ErrZero "2.0/0;4.0/0;")
