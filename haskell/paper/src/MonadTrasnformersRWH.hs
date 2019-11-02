{-
  Created       : 2019 Mar 16 (Sat) 08:27:11 AM by Arthur Vardanyan.
  Last Modified : 2019 Mar 16 (Sat) 08:27:39 AM by Arthur Vardanyan.
-}

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MonadTransformersRWH where


import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

-- import           Control.Monad.Error    (Error (..))

import Data.Maybe

import Data.Text (pack)

import qualified Data.Map as Map

-- | Real World Haskell
--
-- chapter 18. - http://book.realworldhaskell.org/read/monad-transformers.html
--


------------------------------------------------------------
-- the example for using monad transformers will be an interpreter for the little language
------------------------------------------------------------
