{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase    #-}

-- http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html

module FreeMonad where

-- data Fix f = Fix (f (Fix f))
data FixE f e = Fix (f (FixE f e)) | Throw e

data IncompleteException = IncompleteException

data Toy b next = Output b next
                | Bell next
                | Done

instance Functor (Toy b) where
    fmap f (Output x next) = Output x (f next)
    fmap f (Bell     next) = Bell     (f next)
    fmap f  Done           = Done

-- output 'A'
-- done
cmd1 :: Toy Char (Toy b next)
cmd1 = Output 'A' Done

-- bell
-- output 'A'
-- done
cmd2 :: Toy b1 (Toy Char (Toy b2 next))
cmd2 = Bell (Output 'A' Done)


cmd3 :: FixE (Toy Char) e
cmd3 = Fix (Output 'A' (Fix Done))

cmd4 :: FixE (Toy Char) e
cmd4 = Fix (Bell (Fix (Output 'A' (Fix Done))))


catch ::
    (Functor f) => FixE f e1 -> (e1 -> FixE f e2) -> FixE f e2
catch (Fix x) f   = Fix (fmap (flip catch f) x)
catch (Throw e) f = f e


-- output 'A'
-- throw IncompleteException
subroutine :: FixE (Toy Char) IncompleteException
subroutine = Fix (Output 'A' (Throw IncompleteException))


-- try {subroutine}
-- catch (IncompleteException) {
--     bell
--     done
-- }
program :: FixE (Toy Char) e
program = subroutine `catch` (\_ -> Fix (Bell (Fix Done)))



-------------------------------------------------------------------------------
-- https://www.youtube.com/watch?v=JxC1ExlLjgw


data AddLang = AddLangIntLit Integer
             | Add AddLang AddLang
             deriving (Show, Eq)


interpAddLang :: AddLang -> Integer
interpAddLang = \case
                AddLangIntLit i -> i
                Add l r -> interpAddLang l + interpAddLang r


addLangExp :: AddLang
addLangExp = Add (AddLangIntLit 1) (AddLangIntLit 3)



data MulLang = MulLangIntLit Integer
             | Mul MulLang MulLang
             deriving (Show, Eq)


interpMulLang :: MulLang -> Integer
interpMulLang = \case
                MulLangIntLit i -> i
                Mul l r -> interpMulLang l * interpMulLang r


mulLangExp :: MulLang
mulLangExp = Mul (MulLangIntLit 1) (MulLangIntLit 3)


data AddF x = AddI Integer | AddF x x deriving (Show, Eq, Functor)
data MulF x = MulI Integer | MulF x x deriving (Show, Eq, Functor)


type AddLang' = FixE AddF
type MulLang' = FixE MulF


addLangExp' :: AddLang'
addLangExp' = FixE (AddF (FixE (AddI 1)) (FixE (AddI 1)))
