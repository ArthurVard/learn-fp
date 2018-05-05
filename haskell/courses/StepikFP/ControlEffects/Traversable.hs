{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}
module StepikFP.ControlEffects.Traversable where

import Data.Foldable
import Data.Traversable
import Control.Applicative
import Data.Monoid

data Tree a = Nil | Branch (Tree a) a (Tree a) deriving (Eq, Show)

instance Foldable Tree where
    foldr :: (a -> b -> b) -> b -> Tree a -> b
    foldr f z Nil = z
    foldr f z (Branch l x r) = f x (foldr f (foldr f z r) l)

    fold :: Monoid m => Tree m -> m
    fold = foldr mappend mempty


instance Functor Tree where
    fmap f Nil = Nil
    fmap f (Branch l x r) = Branch (fmap f l) (f x) (fmap f r)


testTree = Branch (Branch (Branch Nil 1 Nil) 2 (Branch Nil 3 Nil)) 4 (Branch Nil 5 Nil)
{-
      4
     / \
    2   5
   / \
  1   3

-}

{-

fold :: (Foldable t, Monoid m) => t m -> m
fold = foldr mappend mempty


-- asum դա նույն բան է անում Alternative կոնտեյների համար ինչ, որ fold-ը Monoid-i համար
asum :: (Foldable t, Alternative f) => t (f a) -> f a
asum = foldr (<|>) empty
-}


-- >>> asum $ fmap Just testTree
-- Just 4
-- >>> asum $ fmap (\_ -> Nothing) testTree
-- Nothing


{-
sequenceA_ ::(Foldable t, Applicative f) => t (f a) -> f ()
sequenceA_ = foldr (*>) (pure ())

--  (*>) Այս ֆունկցիան իրականացնում է էֆեկտներ, դեն նետելով արժեքները
(*>) :: f a -> f () -> f ()

sequenceA_ [ap1, ap2, ap3] = ap1 *> (ap2 *> (ap3 *> pure ()))
-}


-- >>> sequenceA_ $ fmap Just testTree
-- Just ()
-- >>> sequenceA_ [("A",1), ("B", 2)]
-- ("AB",())
-- >>>  sequenceA_ [[1,2,3], [4,5]]
-- [(),(),(),(),(),()]



{-

foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap f = foldr (mappend . f) mempty

traverse_ ::  (Foldable t, Applicative f) => (a -> f b) -> t a -> f ()
traverse_ f = foldr ((*>) . f) (pure ())

traverse_ f [x1, x2, x3] = f x1 *> (f x2 *> (f x3 *> pure ()))
traverse_ f (\x -> [x+10, x+20]) [1,2,3]
= [11,21] *> ([12,22] *> ([13,23] *> [()] ))
= [11,21] *> ([12,22] *> ([(),()] ))
= [11,21] *> ([(),(),(),()])
= [(),(),(),(),(),(),(),()]

-}

-- >>> traverse_ (\x -> (show x, x * 2)) [1,2,3]
-- ("123",())


-- | ինչպես անել որպեսզի էֆեկտի հետ մի ասին վերադարձնենք նաև արժեքը
-- այս ֆունկցիայի դըպքում արժեքները վերադարձվում են ցուցակ կոնտեյների մեջ
-- եթե ուզում ենք պահպանել նախնական կոնտեյները այդ ժամանակ է մեզ պետք Traversable
-- foldr չի կարոշ դա անել
sequenceA2List :: (Foldable t, Applicative f) => t (f a) -> f [a]
sequenceA2List = foldr (\x y -> pure (:) <*> x <*> y ) (pure [])

-- ընդհանուր ֆունկցիա որ պահպանում է կոնտեյները
-- sequenceA2List :: (Foldable t, Applicative f) => t (f a) -> f (t a)
-- sequenceA2List = foldr (\x y -> pure (:) <*> x <*> y ) (pure [])

-- >>> sequenceA2List [("A",1), ("B",2)]
-- ("AB",[1,2])
-- >>> sequenceA2List [Right 1, Right 2]
-- Right [1,2]
-- >>> sequenceA2List [Right 1, Left 2]
-- Left 2
-- >>> sequenceA2List $ fmap ((,) "A") testTree
-- ("AAAAA",[4,2,1,3,5])
-- >>> sequenceA_ $ fmap ((,) "A") testTree
-- ("AAAAA",())


-- Ex 1.
-- >>> traverse2list (\x -> [x+10,x+20]) [1,2,3]
-- [[11,12,13],[11,12,23],[11,22,13],[11,22,23],[21,12,13],[21,12,23],[21,22,13],[21,22,23]]
-- >>> traverse2list (\x -> [x+10,x+20]) $ Branch (Branch Nil 1 Nil) 2 (Branch Nil 3 Nil)
-- [[12,11,13],[12,11,23],[12,21,13],[12,21,23],[22,11,13],[22,11,23],[22,21,13],[22,21,23]]
traverse2list :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f [b]
traverse2list f = foldr (\x y -> pure (:) <*> f x <*> y) (pure [])



-- | Traversable
-- Ծամոթանանաք կլասսի տիպի հետ, որը օժտված է երկու հատկություննով
-- 1). Հնարավորություն է տալիս, անցնել կառւոցվածքի վրայով կւոտակելով՝ այդ կառւոցվածքի բոլոր տարերի, որոնք
-- ապլիկատիվ ֆունկտորներ, բոլոր հնարավոր հնարավոր էֆեկտները:
-- 2). աշխատանքի ընթացքում այս կասի տիպը, ոչ միայն փաթաթում է այդ կառուցվածքը այլ նաև՝ պահպանում է եղած
-- կառուցվածքը հաշվի առնելով այն էֆեկտները որոնք տեղի են ունենում, քանի որ տարերը ապլիակտիվներ են:

{-
class (Functor t, Foldable t) => Traversable t where
    sequenceA :: Applicative f => t (f a) -> f (t a)
    sequenceA = traverse id -- id :: f b -> f b
    traverse  :: Applicative f => (a -> f b) -> t a -> f (t b)
    traverse g = sequenceA . fmap g

քանի որ, sequenceA և traverse ֆունկցիաները արտահայտվում են մեկը մյուսով, հետևաբար կոնկրետ տիպի համար
այս  կլաս տիպի իրականացման համար բավական է սահմանել կամ sequenceA կամ traverse

instance Traversable Maybe where
    traverse  :: Applicative f => (a -> f b) -> Maybe a -> f (Maybe b)
    traverse _ Nothing = pure Nothing
    traverse g (Just x) = pure Just <*> g x



instance Functor Maybe where
    fmap  :: (a -> b) -> Maybe a -> Maybe b
    fmap _ Nothing = Nothing
    fmap g (Just a) =  Just (g a)

-- Քանի որ Functor և Traversable իրականացման միջև նմանությունը ակնհայտ է, պետք է օգտվել

instance Functor ((,) s) where
    fmap :: (a -> b) -> (s,a) -> (s,b)
    fmap g (x,y) = (,) x (g y)

instance Traversable ((,) s) where
    traverse :: Applicative f => (a -> f b) -> (s,a) -> f (s,a)
    traverse g (x,y) = pure ((,) x) <*>  g y
-}

-- Ex 2.
data Triple a = Tr a a a  deriving (Eq,Show)

instance Functor Triple where
    fmap f (Tr a b c) = Tr (f a) (f b) (f c)

-- >>> foldl (++) "!!" (Tr "ab" "cd" "efg")
-- "!!abcdefg"
instance Foldable Triple where
    foldr f z (Tr a b c) = f a ( f b ( f c z))

instance Applicative Triple where
    pure a = Tr a a a
    (Tr f g h) <*> (Tr a b c) = Tr (f a) (f b) (f c)


-- >>> traverse (\x -> if x>10 then Right x else Left x) (Tr 12 14 16)
-- Right (Tr 12 14 16)
-- >>> traverse (\x -> if x>10 then Right x else Left x) (Tr 12 8 4)
-- Left 8
-- >>> sequenceA (Tr (Tr 1 2 3) (Tr 4 5 6) (Tr 7 8 9))
-- Tr (Tr 1 4 7) (Tr 1 4 8) (Tr 1 4 9)
instance Traversable Triple where
    traverse f (Tr a b c) = Tr <$> (f a) <*> f b <*> f c


-- Ex 3.

data Result a = Ok a | Error String deriving (Eq,Show)

instance Functor Result where
    fmap f (Error s) = Error s
    fmap f (Ok a) = Ok (f a)

instance Applicative Result where
    pure = Ok
    Error s <*> fa = Error s
    Ok f <*> fa = f <$> fa

instance Foldable Result where
    foldr f z (Error s) = z
    foldr f z (Ok a) = f a z

-- >>> traverse (\x->[x+2,x-2]) (Ok 5)
-- [Ok 7,Ok 3]
-- >>>
instance Traversable Result where
    traverse f (Error s) = pure(Error s)
    traverse f (Ok a) = Ok <$> (f a)


-------------------------------------------------------------------------------
-- Ex 4.

-- Traversable սահմանելու համար ուղակի կրկնում ենք Functor-ի սահմանումը բարձրացնելով դեպի Applicative
-- որպես օրինակ նաենք ցուցակը,  []
{-
instance Functor [] where
  fmap :: (a -> b) -> [a] -> [b]
  fmap _ [] = []
 -- fmap f (x:xs) = f x : fmap xs
  fmap f (x:xs) = (:) (f x)  (fmap xs)

instance Traversable [] where
  traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
  traverse _ [] = pure []
  traverse f (x:xs) = (:) <$> f x <*> traverse f xs


-- >>> traverse (\x -> [x+10, x+20]) [1,2,3]
-- >> [[11,12,13],[11,12,23],[11,22,13],[11,22,23],[21,12,13],[21,12,23],[21,22,13],[21,22,23]]

-- examine
traverse (\x -> [x+10, x+20]) [1,2,3] =
(:) <$> [11,21] ((:) <$> [12,22] <*> ((:) <$> [13,23] <*> pure []))

-}

{-
data Tree a = Nil | Branch (Tree a) a (Tree a)  deriving (Eq, Show)

instance Functor Tree where
  fmap _ Nil = Nil
  fmap f (Branch l x r) = Branch (fmap f l) (f x) (fmap f r)

instance Foldable Tree where
  foldr f z Nil = z
  foldr f z (Branch l x r) = f x (foldr f (foldr f z r) l)

-}

-- >>> traverse (\x -> if odd x then Right x else Left x) (Branch (Branch Nil 1 Nil) 3 Nil)
-- Right (Branch (Branch Nil 1 Nil) 3 Nil)
-- >>> traverse (\x -> if odd x then Right x else Left x) (Branch (Branch Nil 1 Nil) 2 Nil)
-- Left 2
-- >>> sequenceA $ Branch (Branch Nil [1,2] Nil) [3] Nil
-- [Branch (Branch Nil 1 Nil) 3 Nil,Branch (Branch Nil 2 Nil) 3 Nil]
instance Traversable Tree where
    traverse _ Nil = pure  Nil
    traverse f (Branch l a r) = Branch <$> traverse f l  <*> f a <*> traverse f r


-------------------------------------------------------------------------------
-- Ex 5.

infixr 9 |.|
newtype (|.|) f g a = Cmps { getCmps :: f (g a) }  deriving (Eq,Show)


instance (Functor f, Functor g) =>  Functor ((|.|) f g)  where
    fmap f (Cmps h) = Cmps $  fmap (fmap f) h

instance (Foldable f, Foldable g) => Foldable ((|.|) f g) where
    foldMap f (Cmps t) = foldMap (foldMap f) t

instance (Traversable f, Traversable g) => Traversable ((|.|) f g) where
    traverse f (Cmps t) = Cmps <$> traverse (traverse f) t


-- >>> sequenceA (Cmps [Just (Right 2), Nothing])
-- Right (Cmps {getCmps = [Just 2,Nothing]})
-- >>> sequenceA (Cmps [Just (Left 2), Nothing])
-- Left 2

instance (Applicative f, Applicative g) => Applicative ((|.|) f g) where
    pure x = Cmps (pure (pure x))
    Cmps f <*> Cmps x = Cmps (liftA2 (<*>) f x)
    liftA2 f (Cmps x) (Cmps y) =
      Cmps (liftA2 (liftA2 f) x y)


{-instance Traversable ((|.|) f g) where
    -- traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
    traverse f (Cmps h) = undefined --traverse f  h
-}
