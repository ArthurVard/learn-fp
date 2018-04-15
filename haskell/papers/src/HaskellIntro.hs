-- {-#LANGUAGE DatatypeContexts#-}
module HaskellIntro where



data DivResult = DivByZero | Success Double deriving (Show, Eq)

safeDiv ::  Double -> Double -> DivResult
safeDiv a b = if b == 0
              then DivByZero
              else Success (a /  b)


safeDiv' ::  Double -> Double -> DivResult
safeDiv'  _ 0 = DivByZero
safeDiv'  a b = Success (a /  b)


resultToString :: DivResult -> String
resultToString DivByZero = "Division by zero"
resultToString (Success n) = show n

data List a = Nil | Cons a (List a) deriving (Show, Eq)

xs = Cons 1 (Cons 2 (Cons 3 Nil))

-- >>> sumList xs
-- 6
sumList :: (Num a) => List a -> a
sumList xs = case xs of
               Nil        ->  0
               Cons x xs  ->  x + sumList xs


prodList :: (Num a) => List a -> a
prodList xs = case xs of
               Nil        ->  1
               Cons x xs  ->  x * prodList xs


reduceL :: (b -> a -> b) -> b -> List a -> b
reduceL _ z Nil         = z
reduceL f z (Cons x xs) = reduceL f (f z x) xs


reduceR :: (a -> b -> b) -> b -> List a -> b
reduceR _ z Nil         = z
reduceR f z (Cons x xs) = x `f` reduceR f z xs


data Tree a = Leaf | Tree (Tree a) a (Tree a) deriving (Show, Eq)

tree = Tree (Tree Leaf 1 Leaf) 2 Leaf

treeMap f Leaf = Leaf
treeMap f (Tree l a r) = Tree (treeMap f l) (f a) (treeMap f r)

data Exp = I Int | Add Exp Exp | Mul Exp Exp deriving (Show, Eq)

data Val = Int

eval exp =
    case exp of
      I n       -> n
      Add e1 e2 -> eval e1 + eval e2
      Mul e1 e2 -> eval e1 * eval e2



divideList' :: (Show a, Fractional a) => [a] -> (String,a)
divideList' []     = ("1.0", 1.0)
divideList' (x:xs) = (/) <$> (concat ["<-",show x, "/"], x) <*> divideList' xs


newtype Arr2 e1 e2 a = Arr2 { getArr2 :: e1 -> e2 -> a }
newtype Arr3 e1 e2 e3 a = Arr3 { getArr3 :: e1 -> e2 -> e3 -> a }

instance Functor (Arr2 e1 e2) where
  fmap f  fa =  Arr2 $ \e1 -> \e2 -> let f' = getArr2 fa in (f (f' e1 e2))
