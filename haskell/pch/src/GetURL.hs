-- (c) Simon Marlow 2011, see the file LICENSE for copying terms.

-- Simple wrapper around HTTP, allowing proxy use


-- | original source https://github.com/simonmar/parconc-examples/blob/master/GetURL.hs
module GetURL (getURL) where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import qualified Data.ByteString.Lazy as L
import           Network.HTTP.Conduit

getURL :: String -> IO ByteString
getURL url = L.toStrict <$> simpleHttp url
