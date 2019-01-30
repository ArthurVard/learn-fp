module Stepik.Unit5.Monads.Definition where

import Prelude hiding (($), (&))

-------------------------------------------------------------------------------
-- Exercise 5.2

data Log a = Log [String] a deriving Show

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f str = \x -> Log [str] (f x)

-- >>> add1Log 3
-- Log ["added one"] 4
add1Log = toLogger (+1) "added one"

-- >>> mult2Log 3
-- Log ["multiplied by 2"] 6
mult2Log = toLogger (* 2) "multiplied by 2"

-- >>> execLoggers 3 add1Log mult2Log
-- Log ["added one","multiplied by 2"] 8
execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers x f g = let (Log xs x') = f x
                        (Log ys x'') = g x'
                    in Log (xs ++ ys) x''




{-
class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b -- bind

infixl 1 >>=
-}


toKleisli :: Monad m => (a -> b) -> (a -> m b)
-- toKleisli f = \x -> return (f x)
toKleisli = (.) return



returnLog :: a -> Log a
returnLog = \x -> Log [] x


infixr 0 $
($) :: (a -> b) -> a -> b
f $ x = f x

infixl 1 &
(&) :: a -> (a -> b) -> b
x & f = f x


calc_01 = (+1) $ (*3) $ (+2) $ 5
calc_02 = 5 & (+1) & (*3) & (+2)


-- if we move `m` from the tyep signature of (>>=) then we notice that
-- bind works like (&)
--

-- flip fmap :: Functor f => f a -> (a -> b)   -> f b
--     (>>=) :: Monad m   => m a -> (a -> m b) -> m b




-- | Реализуйте фукцию bindLog
-- >>> Log ["nothing done yet"] 0 `bindLog` add1Log
-- Log ["nothing done yet","added one"] 1
-- >>> Log ["nothing done yet"] 3 `bindLog` add1Log `bindLog` mult2Log
-- Log ["nothing done yet","added one","multiplied by 2"] 8
bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log xs x) f = let (Log ys y) = f x
                       in Log (xs ++ ys) y

-- | Реализованные ранее returnLog и bindLog позволяют объявить тип Log
 -- представителем класса Monad:
instance Monad Log where
    return = returnLog
    (>>=) = bindLog

instance Functor Log where
    fmap f (Log xs x) = Log xs (f x)
instance Applicative Log where
    pure = return
    (<*>) = undefined
-- >>> execLoggersList 3 [add1Log, mult2Log, \x -> Log ["multiplied by 100"] (x * 100)]
-- Log ["added one","multiplied by 2","multiplied by 100"] 800
execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList e fx = foldl bindLog (return e) fx
