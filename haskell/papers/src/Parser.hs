module Parser where

import Data.Char(digitToInt, isDigit, isLower)

-- | backtracking parser
newtype Parser a = Parser {runParser :: String -> [(a, String)]}

-- | maybe parser
newtype Prs a = Prs { runPrs :: String -> Maybe (a, String) }

-- | позволяющий возвращать пользователю причину неудачи при синтаксическом разборе:
newtype PrsE a = PrsE { runPrsE :: String -> Either String (a, String) }

instance Functor PrsE where
  fmap f p = PrsE $ \s -> case runPrsE p s  of
                            Left e -> Left e
                            Right (a, s') -> Right (f a, s')
instance Applicative PrsE where
  pure a  = PrsE $ \s -> Right (a, s)
  pf <*> p = PrsE $ \s -> do
               (f, s') <- runPrsE pf s
               (a, s'') <- runPrsE p s'
               return (f a, s'')


instance Functor Parser where
    fmap f p = Parser $ \s ->  [(f a, s') | (a,s')<- runParser p s]


instance Applicative Parser where
    pure a   = Parser $ \s -> [(a, s)]
    pf <*> p = Parser $ \s -> [(g a, s'') | (g,s') <- runParser pf s, (a, s'') <- runParser p s' ]

instance Monad Parser where
    return   = pure
    p >>= k = Parser $ \s -> [ (b, s'') | (a, s') <- runParser p s, (b, s'') <- runParser (k a) s']

instance Functor Prs where
    fmap f p = Prs $ \s -> do
                       (a, s') <- runPrs p s
                       return (f a, s')
instance Applicative Prs where
  pure a = Prs $ \s -> Just (a, s)
  (<*>) pf pa = Prs fun where
         fun s = do
             (f, s') <- runPrs pf s
             (a, s'') <- runPrs pa s'
             return (f a, s'')

anyChar :: Parser Char
anyChar = Parser f where
    f "" = []
    f (x:xs) = [(x, xs)]

anyChr :: Prs Char
anyChr = Prs f where
    f "" = Nothing
    f (x:xs) = Just (x, xs)


satisfy :: (Char -> Bool) -> Parser Char
satisfy pr =Parser f where
    f "" = []
    f (c:cs) | pr c = [(c, cs)]
             | otherwise = []
satisfyE :: (Char -> Bool) -> PrsE Char
satisfyE pr = PrsE f where
    f "" = Left "unexpected end of input"
    f (c:cs) | pr c = Right (c, cs)
             | otherwise = Left $ "unexpected " ++ [c]


-- >>> anyE = satisfyE (const True)
-- >>> runPrsE ((,) <$> anyE <* charE 'B' <*> anyE) "ABCDE"
-- > Right (('A','C'),"DE")
-- >>> runPrsE ((,) <$> anyE <* charE 'C' <*> anyE) "ABCDE"
-- > Left "unexpected B"
-- >>> runPrsE ((,) <$> anyE <* charE 'B' <*> anyE) "AB"
-- >>> Left "unexpected end of input"


-- >>> runPrsE (charE 'A') "ABC"
-- > Right ('A',"BC")
-- >>> runPrsE (charE 'A') "BCD"
-- > Left "unexpected B"
-- >>> runPrsE (charE 'A') ""
-- > Left "unexpected end of input"
charE :: Char -> PrsE Char
charE c = satisfyE (== c)


lower :: Parser Char
lower = satisfy isLower

char :: Char -> Parser Char
char c = satisfy (== c)

digit :: Parser Int
digit = digitToInt <$> satisfy isDigit

multiplication :: Parser Int
multiplication = (*) <$> digit <* char '*' <*> digit
-- many1 :: Parser a
-- many1 = runParser



newtype Op f g a = Cmps {getCmps :: f (g a)} deriving (Show, Eq)


type A   = Op ((,) Integer)  ((,) Char) Bool


a :: A
a = Cmps(1,('c', True)) :: A

instance (Functor h, Functor j) => Functor (Op h j) where
    fmap f (Cmps x) = Cmps $ fmap (fmap f) x


instance (Applicative  h, Applicative j) => Applicative (Op h j) where
    pure x = Cmps $ pure  (pure x)
    (Cmps f) <*> (Cmps a) = Cmps $ fmap (<*>) f <*> a
