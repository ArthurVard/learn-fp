module Main where

import qualified Adapter.HTTP.Main as HTTP
import           Lib

main :: IO ()
main = withState $  \port state -> do
         let runner = run state
         HTTP.main port runner
