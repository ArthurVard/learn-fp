{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase    #-}

-- http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html

module TaglessDsl where



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



newtype Fix f = Fix {unFix :: f (Fix f)}

fixFold :: Functor f => (f a -> a) -> (Fix f -> a)
fixFold phi = go where go = phi . fmap go . unFix

data AddF x = AddI Integer | AddF x x deriving (Show, Eq, Functor)
data MulF x = MulI Integer | MulF x x deriving (Show, Eq, Functor)


type AddLang' = Fix AddF
type MulLang' = Fix MulF


addLangExp' :: AddLang'
addLangExp' = Fix (AddF (Fix (AddI 1)) (Fix (AddI 1)))
