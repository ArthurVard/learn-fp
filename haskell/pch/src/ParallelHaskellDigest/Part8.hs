module ParallelHaskellDigest.Part8 where

import           Control.Concurrent
import           Data.ByteString            (unpack)
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as E

import GetURL
main = do
    m <- newEmptyMVar

    forkIO $ do
      r <- getURL "http://en.wikipedia.org/wiki/Shovel"
      putMVar m r

    print "doSomethingElse"

    r <- takeMVar m
    mapM_ print $ T.words . T.take 100 $ E.decodeUtf8  r
