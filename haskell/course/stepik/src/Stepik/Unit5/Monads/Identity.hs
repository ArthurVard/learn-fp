module Stepik.Unit5.Monads.Identity where



newtype Identity a = Identity {runIdentity :: a}
    deriving (Eq, Show)

instance Functor Identity where
    -- fmap :: (a -> b) -> f a -> f b
--    fmap  = (=<<) $ a  (.) return
    fmap  = (=<<) . (return .)

instance Applicative Identity where
    pure = return
    (<*>) = undefined


instance Monad Identity where
    return = Identity
    Identity x >>= k = k x

-- Kleisly arrow of Identity Мonad
wrap'n'succ :: Integer -> Identity Integer
wrap'n'succ n = Identity (succ n)
