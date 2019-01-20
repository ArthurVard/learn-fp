module JohnHughes.ChurchDraft where

-- | lgocial operations

true a b = a

false a b = b

and p q = p true q

ifte bool a b = bool a b


-- | Numerals

zero f x = x

one f x = f (f x)
