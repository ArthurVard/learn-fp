{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonadFailDesugaring        #-}
{-# LANGUAGE OverloadedStrings          #-}
module Lib (
withState
, run
           )where

import ClassyPrelude
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Either

import qualified Control.Monad.Catch as E
import qualified Control.Monad.Fail  as Fail

import qualified Adapter.HTTP.Main       as HTTP
import qualified Adapter.InMemory.Auth   as M
import qualified Adapter.PostgreSQL.Auth as PG
import qualified Adapter.Redis.Auth      as Redis

import Domain.Auth


type State = (PG.State, Redis.State, TVar M.State)
newtype App a = App
    { unApp :: ReaderT State IO a
    } deriving ( Applicative, Functor, Monad, MonadReader State, MonadIO
               , Fail.MonadFail, E.MonadThrow)



run :: State -> App a -> IO a
run state = flip runReaderT state . unApp



--
-- we create instances of AuthRepo, EmailVerificationNotif, and SessionRepo for App.
-- These instances are the glue between in-memory implementation and domain logic.
-- In general, we just delegate the calls to in-memory implementations.
instance AuthRepo App where
    addAuth = PG.addAuth
    setEmailAsVerified = PG.setEmailAsVerified
    findUserByAuth = PG.findUserByAuth
    findEmailFromUserId = PG.findEmailFromUserId

instance EmailVerificationNotif App where
    notifyEmailVerification = M.notifyEmailVerification


instance SessionRepo App where
    newSession = Redis.newSession
    findUserIdBySessionId = Redis.findUserIdBySessionId
-- instance Fail.MonadFial App where

withState :: (Int -> State -> IO ()) -> IO ()
withState action = do
    mState <- newTVarIO M.initialState
    PG.withState pgCfg $ \pgState ->
        Redis.withState redisCfg $ \redisState ->
            let state =  (pgState, redisState, mState)
            in action port state

 where
--   mqCfg = "amqp://guest:guest@localhost:5672/%2F"
   redisCfg = "redis://localhost:6379/0"
   pgCfg = PG.Config
           { PG.configUrl = "postgresql://localhost/hauth"
           , PG.configStripeCount = 2
           , PG.configMaxOpenConnPerStripe = 5
           , PG.configIdleConnTimeout = 10
           , PG.config'host = "localhost"
           , PG.config'port = 5432
           , PG.config'dbname = "practical_web"
           , PG.config'dbuser = "pweb"
           , PG.config'dbpassword = "123"
           }
   port = 3008


main :: IO ()
main = withState $  \port state -> do
         let runner = run state
         HTTP.main port runner




--  let’s write a simple program using it to see it in action.
someFunc :: IO ()
someFunc = do
   mState <- newTVarIO M.initialState
--   run state action
   PG.withState pgCfg $ \pgState ->
       Redis.withState redisCfg $ \redisState ->
           run (pgState, redisState, mState) action
 where
   redisCfg = "redis://localhost:6379/0"
   pgCfg = PG.Config
           { PG.configUrl = "postgresql://localhost/hauth"
           , PG.configStripeCount = 2
           , PG.configMaxOpenConnPerStripe = 5
           , PG.configIdleConnTimeout = 10
           , PG.config'host = "localhost"
           , PG.config'port = 5432
           , PG.config'dbname = "practical_web"
           , PG.config'dbuser = "pweb"
           , PG.config'dbpassword = "123"
           }
   port = 3008


action :: App ()
action = do
   let email = either undefined id $ mkEmail "eckqy11@test.com"
       passw = either undefined id $ mkPassword "1234ABCDefgh"
       auth = Auth email passw
   res <-  addAuth auth
   case res of
     Left er -> print  $ show er
     Right (_, vCode) -> do
              let email = authEmail auth
--           lift $ notifyEmailVerification email vCode
              _ <- verifyEmail vCode
              return ()
   Right session <- login auth
   Just uId <- resolveSessionId session
   Just registeredEmail <- getUser uId
   print (session, uId, registeredEmail)


-- | in memory action
actionM :: App ()
actionM = do
   let email = either undefined id $ mkEmail "eckqy1@test.com"
       passw = either undefined id $ mkPassword "1234ABCDefgh"
       auth = Auth email passw
   liftIO $ print auth
   register auth
   code <- M.getNotificationsForEmail email
   case code of
     Nothing    -> error (show email ++ "---err")
     Just vCode -> verifyEmail vCode
   Right session <- login auth
   Just uId <- resolveSessionId session
   Just registeredEmail <- getUser uId
   print (session, uId, registeredEmail)
