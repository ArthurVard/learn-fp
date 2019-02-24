{-# LANGUAGE FlexibleContexts #-}
module Stepik.Unit6.Applicatives.Parsec where

import Control.Applicative ((*>), (<*))

import Text.Parsec
import Text.Parsec.Char
import Text.ParserCombinators.Parsec.Combinator

-- >> parseTest vowel "asd"
-- >> parse vowel "" "asd"
vowel :: Parsec [Char] u Char
vowel = oneOf "aeiof"


-- >>> parseTest getList "1;234;56"

getList :: Parsec String u [String]
getList =  (many1 digit `sepBy` (char ';'))
-- parseTest (sepEndBy (many1 digit) (char ';')) "112;;121;dasd"

p0 :: Parsec [Char] u ([Char], [Char])
p0 = pure (,) <*> many1 letter <*> many1 digit


p1 :: Parsec [Char] u ([Char], [Char])
p1 = (,) <$> many1 letter <*> (many space *> many1 digit)


ignoreBraces' :: Parsec [Char] u [Char]
ignoreBraces' = (count 2 (char '[')) *> many anyChar <* (count 2 (char ']'))


test = ignoreBraces (string "[[") (string "]]") (many1 letter)

ignoreBraces :: Parsec [Char] u a -> Parsec [Char] u b -> Parsec [Char] u c -> Parsec [Char] u c
ignoreBraces ps pe pget = ps *> pget <* pe
