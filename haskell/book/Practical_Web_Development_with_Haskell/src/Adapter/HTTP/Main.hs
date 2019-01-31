{-# LANGUAGE TemplateHaskell #-}
module Adapter.HTTP.Main where

import ClassyPrelude                        hiding (delete)
import Data.ByteString.Builder
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Middleware.Gzip
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Web.Cookie
import Web.Scotty.Trans

import           Text.Digestive.Form  ((.:))
import qualified Text.Digestive.Form  as DF
import qualified Text.Digestive.Types as DT

import qualified Adapter.HTTP.API.Auth as AuthAPI
import           Adapter.HTTP.Common

import Domain.Auth
-------------------------------------------------------------------------------
--

main :: ( MonadIO m, AuthRepo m, EmailVerificationNotif m, SessionRepo m)
 => Int -> (m Response -> IO Response) -> IO ()
main port runner = scottyT port runner routes


routes :: ( MonadIO m, AuthRepo m, EmailVerificationNotif m, SessionRepo m)
          => ScottyT LText m ()
routes = do
  middleware $ gzip $ def { gzipFiles = GzipCompress }
  AuthAPI.routes
  defaultHandler $ \e -> do
-- lift $ $(logTM) ErrorS $ "Unhandled error: " <> ls (showError e)
    status status500
    json ("InternalServerError" :: Text)
