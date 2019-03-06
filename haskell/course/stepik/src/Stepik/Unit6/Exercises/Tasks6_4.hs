module Stepik.Unit6.Exercises.Tasks6_4 where

import Control.Applicative
import Data.Char

-------------------------------------------------------------------------------
-- Exercise 6.4.1

newtype Prs a = Prs { runPrs :: String -> Maybe (a, String) }

-- runPrs (digitToInt <$> anyChr) "BCD"
instance Functor Prs where
   fmap f p = Prs $ \s -> case runPrs p s of
                               Nothing      -> Nothing
                               Just (a, s') -> Just (f a, s')
-- runPrs anyChr ""
anyChr :: Prs Char
anyChr = Prs f
    where
      f []     = Nothing
      f (c:cs) = Just (c,cs)



satisfyM :: (Char -> Bool) -> Prs Char
satisfyM pred = Prs f where
    f [] = Nothing
    f (c:cs) | pred c = Just (c, cs)
             | otherwise = Nothing

-------------------------------------------------------------------------------
-- Exercise 6.4.2

-- runPrs ((,,) <$> anyChr <*> anyChr <*> anyChr) "ABCDE"
-- runPrs (anyChr *> anyChr) "ABCDE"
instance Applicative Prs where
    pure a = Prs $ \s -> Just (a,s)
    pf <*> pv = Prs $ \s -> do
                           (g, s')  <- runPrs pf s
                           (a, s'') <- runPrs pv s'
                           return $ (g a, s'')



-------------------------------------------------------------------------------
-- Exercise 6.4.3

-- | Рассмотрим более продвинутый парсер, позволяющий возвращать
--   пользователю причину неудачи при синтаксическом разборе:
newtype PrsE a = PrsE { runPrsE :: String -> Either String (a, String) }

charE :: Char -> PrsE Char
charE  = satisfyE . (==)

-- >>> runPrsE (charE 'A') "ABC"
-- Right ('A',"BC")
-- >>> runPrsE (charE 'A') "BCD"
-- "unexpected B"
-- >>> runPrsE (charE 'A') ""
-- Left "unexpected end of input"
satisfyE :: (Char -> Bool) -> PrsE Char
satisfyE pred = PrsE f where
    f [] = Left "unexpected end of input"
    f (c:cs) | pred c = Right (c, cs)
             | otherwise = Left $ "unexpected " ++ [c]


-------------------------------------------------------------------------------
-- Exercise 6.4.4

instance Functor PrsE where
    fmap g pr = PrsE $ \s -> case runPrsE pr s of
                             Left a        -> Left a
                             Right (c, cs) -> Right (g c, cs)

instance Applicative PrsE where
    pure a = PrsE $ \s -> Right (a, s)
    p <*> k = PrsE $ \s -> do
           (f, s') <- runPrsE p s
           (a, s'') <- runPrsE k s'
           return (f a, s'')


-------------------------------------------------------------------------------
-- Exercise 6.4.5

-- | instance of Alternative
char :: Char -> Prs Char
char = satisfyM . (==)


-- >>> runPrs (char 'A' <|> char 'B') "ABC"
-- Just ('A',"BC")
-- >>> runPrs (char 'A' <|> char 'B') "BCD"
-- Just ('B',"CD")
-- >>> runPrs (char 'A' <|> char 'B') "CDE"
-- Nothing
instance Alternative Prs where
    empty = Prs (const  Nothing)
    p1 <|> p2 = Prs $ \s -> let r  = runPrs p1 s
                            in case r of
                                 Nothing -> runPrs p2 s
                                 _       -> r



-------------------------------------------------------------------------------
-- Exercise 6.4.6

many0 :: Prs a -> Prs [a]
many0 p = (:) <$> p <*> many0 p <|> pure []

-- Потребляйте один экземпляр p, без альтернативы в случае сбоя
-- Потребляйте 0 или более экземпляров p, возвращая [] когда это не удается.
many1 :: Prs a -> Prs [a]
many1 p = (:) <$> p <*> many0 p


-------------------------------------------------------------------------------
-- Exercise 6.4.7

nat :: Prs Int
nat  = read <$> many1 (satisfyM isDigit)

mult :: Prs Int
mult = (*) <$> nat <* char '*' <*> nat
