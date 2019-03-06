{-# LANGUAGE FlexibleContexts #-}
module Stepik.Unit6.Applicatives.ApplicativeParser where

import Control.Applicative
import Data.Char
------------------------------------------------------------
--
------------------------------------------------------------

-- | the simplest parser type is String -> a
-- read :: Read a => String -> a

newtype Parser a = Parser {apply :: String -> [(a, String)]}

parse :: Parser a -> String -> a
parse p = fst . head . apply p

-- >>> apply anyChar "asdasd"
anyChar :: Parser Char
anyChar = Parser f
    where
      f []     = []
      f (c:cs) = [(c,cs)]


-- apply (digitToInt <$> anyChar) "1232asdas"
instance Functor Parser where
    -- fmap :: (a -> b) -> String -> [(a,String)] -> String -> [(b,String)]
    fmap f (Parser p) = Parser $ \s -> map (\(a,s) -> (f a, s)) (p s)


instance Applicative Parser where
    pure a = Parser $ \s -> [(a, s)]
    f <*> p = Parser $ \s ->
              [ (g a, s'') | (g, s') <- apply f s, (a, s'') <- apply p s']

--  apply ((,) <$> anyChar <*> anyChar) "asdasd"
--  apply (anyChar *> anyChar) "asdasd"
--  apply (anyChar <* anyChar) "asdasd"

satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = Parser f where
    f [] = []
    f (c:cs) | pred c = [(c,cs)]
             | otherwise = []

-- apply lower "asda"
lower :: Parser Char
lower = satisfy isLower

char :: Char -> Parser Char
char c = satisfy (==c)

digit :: Parser Int
digit  = digitToInt <$> satisfy isDigit


-- apply multiplication "2*4"
multiplication :: Parser Int
multiplication = (*) <$> digit <* char '*' <*> digit




-- | typeclass Alternative

{-
class Monoid a where
    mempty :: a
    mappend :: a -> a -> a


instance Monoid a =>  Monoid (Maybe a) where
    mempty :: Nothing
    Nothing `mappend` m = m
    m `mappend` Nothing = m
    Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)

instance  Monoid (Maybe a) where
    mempty :: Nothing
    Nothing `mappend` m = m
    m `mappend` _ = m

newtype First a = First { getFirst :: Maybe a }


class Applicative f => Alternative f where
   empty :: f a
   (<|>) :: f a -> f a -> f a

infixl 3 <|>

instance Alternative [] where
   empty = []
   (<|>) = (++)

instance Alternative Maybe where
   empty = Nothing
   Nothing <|> r = r
   l <|> _ = l
-}

-- | Lows of Alternative type class instance

-- (<*>)  - բազմապատկում
-- (<|>)  - գումարում
{-
Monoid Lows


1. Right distributivity of <*>
(f <|> g) <*> a <=> (f <*> a) <|> (g <*> a)

2. Right absorption for <*>
empty <*> a <=> empty

3. Left distributivity of fmap
f <$> (a <|> b) <=> (f <$> a) <|> (f <$> b)

4. Left absorption for fmap
f <$> empty <=> empty
-}

instance Alternative Parser where
    empty = Parser (const [])
    p1 <|> p2 = Parser $ \s -> let r =  apply p1 s
                               in case r of
                                 [] ->  apply p2 s
                                 _  -> r



-- |


lowers :: Parser String
lowers = pure (:) <*> lower <*> lowers <|> pure ""


many' :: Parser a -> Parser [a]
many' p = (:) <$> p <*> many' p <|> pure []

type Prs = Parser
many1' :: Prs a -> Prs [a]
many1' p = (:) <$> p <*> (many1' p <|> pure [])


many0 :: Prs a -> Prs [a]
many0 p = (:) <$> p <*> many0 p <|> pure []

-- Потребляйте один экземпляр p, без альтернативы в случае сбоя
-- Потребляйте 0 или более экземпляров p, возвращая [] когда это не удается.
many1 :: Prs a -> Prs [a]
many1 p = (:) <$> p <*> many0 p
