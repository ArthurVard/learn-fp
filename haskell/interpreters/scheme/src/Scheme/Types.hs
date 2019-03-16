module Scheme.Types where


data LispVal = Atom String
             -- ^ stores a String naming the atom
             | List [LispVal]
             -- ^ stores a list of other LispVals (Haskell lists are denoted by brackets); also called a proper list
             | DottedList [LispVal] LispVal
             -- ^ representing the Scheme form (a b . c); also called an improper list.
             -- This stores a list of all elements but the last, and then stores the last element as another field
             | Number Integer
             -- ^ containing a Haskell Integer
             | String String
             -- ^ containing a Haskell String
             | Bool Bool
             -- ^ containing a Haskell boolean value
