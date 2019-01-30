
module MonadLows where
import Test.QuickCheck
import Test.QuickCheck.Function

{-
Instances of Monad should satisfy the following laws:

- return a>>=k k a
- m >>=return m
- m >>= (λx → k x>>=h) (m>>=k)>>=h

Instances of both Monad and Functor should additionally satisfy
the law:
- fmap f xs xs>>=return ◦ f

-}

type PropLeftUnit m = Integer -> Fun Integer (m Integer) -> Bool

prop_LeftUnit x (Fun _ f) = (return x >>= f) == f x

type PropRightUnit m = m Integer -> Bool

prop_RightUnit m = (m >>= return) == m

type PropAssoc m = m Integer -> Fun Integer (m Integer) -> Fun Integer (m Integer) -> Bool


prop_Assoc m (Fun _ f) (Fun _ g) = ((m >>= f) >>= g) == (m >>= \x -> f x >>= g)
