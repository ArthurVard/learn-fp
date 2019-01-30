module Stepik.Unit5.Monads.Functor where

import Prelude hiding (Functor, fmap)


------------------------------------------------------------
--


class Functor f where
    fmap :: (a -> b) -> f a -> f b
