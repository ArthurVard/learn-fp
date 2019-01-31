module Adapter.HTTP.ScottyExamples where

import ClassyPrelude                        hiding (delete)
import Data.ByteString.Builder
import Network.HTTP.Types
import Network.Wai.Middleware.Gzip
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Web.Cookie
import Web.Scotty.Trans

import           Text.Digestive.Form  ((.:))
import qualified Text.Digestive.Form  as DF
import qualified Text.Digestive.Types as DT

import Domain.Auth
-------------------------------------------------------------------------------
--

main :: IO ()
main =
 scottyT 3000 id routes




routes :: (MonadIO m) => ScottyT LText m ()
routes = do
  get "/" $ text "home"
  get "/hello/:name" $ do
           name <- param ":name"
           text $ "Hello, " <> name
  post "/users" $ text "adding user"
  put "/users/:id" $ text "updating user"
  patch "/users/:id" $ text "partially updating users"
  delete "/users/:id" $ text "deleting user"
  matchAny "/admin" $ text "I don't care about your HTTP verb"
  options (regex ".*") $ text "CORS usually use this"
  notFound $ text "404"




routes' :: (MonadIO m) => ScottyT LText m ()
routes' = do
  get "/users/:userId/books/:bookId" $ do
            userId <- param "userId"
            bookId <- param "bookId"
            text $ userId <> " - " <> bookId
  get (regex "^/users/(.+)/investments/(.+)$") $ do
            fullPath <- param "0"
            userId <- param "1"
            investmentId <- param "2"
            text $ fullPath <> " : " <> userId <> " - " <> investmentId


routes'' :: (MonadIO m) => ScottyT LText m ()
routes'' =
    get "/hello" $ do
      status unauthorized401
      addHeader "serverName" "gandalfService"
      text "you shall not pass!"


-------------------------------------------------------------------------------
-- Middleware usage

routesM :: (MonadIO m) => ScottyT LText m ()
routesM = do
 -- gzip
  middleware $ gzip (def { gzipFiles = GzipCompress })
  -- request logging
  middleware logStdout
  -- serve static files
  middleware static

  get "/hello" $
      text "Hello!"



-------------------------------------------------------------------------------
-- Cookie
-- https://hackage.haskell.org/package/cookie

setCookie :: (ScottyError e, Monad m) => SetCookie -> ActionT e m ()
setCookie =
 addHeader "Set-Cookie" . decodeUtf8 . toLazyByteString . renderSetCookie



getCookie :: (ScottyError e, Monad m) => Text -> ActionT e m (Maybe Text)
getCookie key = do
  mCookieStr <- header "Cookie"
  return $ do
    cookie <- parseCookies . encodeUtf8 . toStrict <$> mCookieStr
    let bsKey = encodeUtf8 key
    val <- lookup bsKey cookie
    return $ decodeUtf8 val



-------------------------------------------------------------------------------
-- Input Validation


authForm :: (Monad m) => DF.Form [Text] m Auth
authForm =
    Auth <$> "email" .: emailForm
         <*> "password" .: passwordForm
 where
   emailForm = DF.validate (toResult . mkEmail) (DF.text Nothing)
   passwordForm = DF.validate (toResult . mkPassword) (DF.text Nothing)


toResult :: Either e a -> DT.Result e a
toResult = either DT.Error DT.Success
