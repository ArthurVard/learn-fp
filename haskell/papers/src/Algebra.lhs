>{-# LANGUAGE EmptyDataDecls
>  , TypeOperators  #-}

> module Algebra where


> data Unit = Unit -- number one, one value

> data a :+ b = AddL a | AddR b -- set(a) + set(b) (Either type)

> data a :* b = Mul a b -- set(a) * set(b)
> data Void  -- zero has any constractor, imposible to construct value

data Boll = False | True
type Two  = Unit :+ Unit


Notation
 Void => 0
 Unit => 1
 Bool => 2
 Addition => a + b
 Multiplication => a * b
