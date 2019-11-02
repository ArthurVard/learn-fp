{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}

module RWH.Ch18.CountEntriesT where



data Nat = S Nat | Z

data Vec :: Nat -> * where
    Nil  :: Vec Z
    Cons :: Int -> Vec n -> Vec (S n)

data G
data Prm = G



data JobDescription = JobOne
                    | JobTwo
                    | JobThree
  deriving (Show, Eq)
