{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonadFailDesugaring        #-}
{-# LANGUAGE OverloadedStrings          #-}
module Lib where

import ClassyPrelude
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Either

import qualified Control.Monad.Catch as E
import qualified Control.Monad.Fail  as Fail

import qualified Adapter.InMemory.Auth   as M
import qualified Adapter.PostgreSQL.Auth as PG

import Domain.Auth


type State = (PG.State, TVar M.State)
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
    newSession = M.newSession
    findUserIdBySessionId = M.findUserIdBySessionId
-- instance Fail.MonadFial App where


--  let’s write a simple program using it to see it in action.
someFunc :: IO ()
someFunc = do
   mState <- newTVarIO M.initialState
--   run state action
   PG.withState pgCfg $ \pgState -> run (pgState, mState) action
 where
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
