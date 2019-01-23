module Stepik.Monads.Definition where


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
