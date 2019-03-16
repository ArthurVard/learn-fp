module Main where

import System.Environment

import Scheme

------------------------------------------------------------
--
------------------------------------------------------------

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn (readExpr expr)
