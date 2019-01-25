{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonadFailDesugaring        #-}
module Lib where

import ClassyPrelude
import Control.Monad.IO.Class
import Control.Monad.Reader

import qualified Control.Monad.Fail as Fail

import qualified Adapter.InMemory.Auth as M
import           Domain.Auth


type State = TVar M.State
newtype App a = App
    { unApp :: ReaderT State IO a
    } deriving (Applicative, Functor, Monad, MonadReader State, MonadIO, Fail.MonadFail)



run :: State -> App a -> IO a
run state = flip runReaderT state . unApp



--
-- we create instances of AuthRepo, EmailVerificationNotif, and SessionRepo for App.
-- These instances are the glue between in-memory implementation and domain logic.
-- In general, we just delegate the calls to in-memory implementations.
instance AuthRepo App where
    addAuth = M.addAuth
    setEmailAsVerified = M.setEmailAsVerified
    findUserByAuth = M.findUserByAuth
    findEmailFromUserId = M.findEmailFromUserId

instance EmailVerificationNotif App where
    notifyEmailVerification = M.notifyEmailVerification


instance SessionRepo App where
    newSession = M.newSession
    findUserIdBySessionId = M.findUserIdBySessionId
-- instance Fail.MonadFial App where


--  let’s write a simple program using it to see it in action.
someFunc :: IO ()
someFunc = do
   state <- newTVarIO M.initialState
   run state action

action :: App ()
action = do
   let email = either undefined id $ mkEmail "ecky@test.com"
       passw = either undefined id $ mkPassword "1234ABCDefgh"
       auth = Auth email passw
   register auth
   Just vCode <- M.getNotificationsForEmail email
   verifyEmail vCode
   Right session <- login auth
   Just uId <- resolveSessionId session
   Just registeredEmail <- getUser uId
   print (session, uId, registeredEmail)
