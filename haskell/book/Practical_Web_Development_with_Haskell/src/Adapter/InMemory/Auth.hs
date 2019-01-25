{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module Adapter.InMemory.Auth where

import ClassyPrelude
import Control.Monad.Except
import Data.GUID
import Data.Has
import Data.Monoid

import qualified Domain.Auth as D

import Domain.Auth

-------------------------------------------------------------------------------
--

type InMemory r m = (Has (TVar State) r, MonadReader r m, MonadIO m)

data State = State
    { stateAuths            :: [(D.UserId, D.Auth)]
    , stateUnverifiedEmails :: Map D.VerificationCode D.Email
    , stateVerifiedEmails   :: Set D.Email
    , stateUserIdCounter    :: Int
    , stateNotifications    :: Map D.Email D.VerificationCode
    , stateSessions         :: Map D.SessionId D.UserId
    } deriving (Show, Eq)

initialState :: State
initialState = State
               { stateAuths = []
               , stateUnverifiedEmails = mempty
               , stateVerifiedEmails = mempty
               , stateUserIdCounter = 0
               , stateNotifications = mempty
               , stateSessions = mempty
               }

-------------------------------------------------------------------------------
-- AuthRepo  Implementation
-------------------------------------------------------------------------------

findEmailFromUserId :: InMemory r m => UserId -> m (Maybe Email)
findEmailFromUserId uId = do
  tvar <- asks getter
  state <- liftIO $ readTVarIO tvar
  let mayAuth = map snd . find ((uId ==) . fst) $ stateAuths state
  return $ D.authEmail <$> mayAuth


findUserByAuth :: InMemory r m => D.Auth -> m (Maybe (D.UserId, Bool))
findUserByAuth auth = do
  tvar <- asks getter
  state <- liftIO $ readTVarIO tvar
  let mayUserId = map fst . find ((auth ==) . snd) $ stateAuths state
  case mayUserId of
    Nothing -> return Nothing
    Just uId -> do
      let verifies = stateVerifiedEmails state
          email = D.authEmail auth
          isVerified = elem email verifies
      return $ Just (uId, isVerified)



-- | the basic idea is to look up an Email in stateUnverifiedEmails from the given VerificationCode
-- and move it into stateVerifiedEmails. Since VerificationCode might not map to any Email,
-- we may throw EmailVerificationErrorInvalidCode
setEmailAsVerified :: InMemory r m => VerificationCode -> m (Either EmailVerificationError ())
setEmailAsVerified vCode = do
  tvar <- asks getter
  atomically . runExceptT $ do
    state <- lift $ readTVar tvar
    let unverifieds = stateUnverifiedEmails state
        verifieds = stateVerifiedEmails state
        mayEmail = lookup vCode unverifieds
    case mayEmail of
      Nothing -> throwError D.EmailVerificationErrorInvalidCode
      Just email -> do
           let newUnverifieds = deleteMap vCode unverifieds
               newVerifieds = insertSet email verifieds
               newState = state
                 { stateUnverifiedEmails = newUnverifieds
                 , stateVerifiedEmails = newVerifieds
                 }
           lift $ writeTVar tvar newState


addAuth :: InMemory r m
           => Auth -> m (Either RegistrationError VerificationCode)
addAuth auth = do
  tvar <- asks getter

  -- gen verification code
  vCode <- liftIO genText
  atomically . runExceptT $ do
    state <- lift $ readTVar tvar
    -- check whether the given email is duplicate
    let auths = stateAuths state
        email = D.authEmail auth
        isDuplicate = any (email ==) . map (D.authEmail . snd) $ auths
    when isDuplicate $ throwError D.RegistrationErrorEmailTaken
    -- update the state
    let newUserId = stateUserIdCounter state + 1
        newAuths = (newUserId, auth) : auths
        unverifieds = stateUnverifiedEmails state
        newUnverifieds = insertMap vCode email unverifieds
        newState = state
          { stateAuths = newAuths
          , stateUserIdCounter = newUserId
          , stateUnverifiedEmails = newUnverifieds
          }
    lift $ writeTVar tvar newState
    return vCode






-------------------------------------------------------------------------------
-- EmailVerificationNotif Implementation
-------------------------------------------------------------------------------

-- |  insert the given VerificationCode to stateNotifications.
notifyEmailVerification :: InMemory r m => Email -> VerificationCode -> m ()
notifyEmailVerification email vCode = do
  tvar <- asks getter
  atomically $ do
    state@State{..} <- readTVar tvar
    let newNotifications = insertMap email vCode stateNotifications
        newState = state { stateNotifications = newNotifications }
    writeTVar tvar newState

-- | for debugging
getNotificationsForEmail :: InMemory r m => D.Email -> m (Maybe D.VerificationCode)
getNotificationsForEmail email = do
   tvar <- asks getter
   state <- liftIO $ readTVarIO tvar
   return $ lookup email $ stateNotifications state

-------------------------------------------------------------------------------
-- SessionRepo Implementation
-------------------------------------------------------------------------------

-- | generate sessionId and insert it in stateSessions
newSession :: InMemory r m => UserId -> m SessionId
newSession uId = do
  tvar <- asks getter
  sId <- liftIO $ ((tshow uId) <>) <$> genText
  atomically $ do
     state@State{..} <- readTVar tvar
     let newSessions = insertMap sId uId stateSessions
         newState = state { stateSessions = newSessions }
     writeTVar tvar newState
     return sId


findUserIdBySessionId :: InMemory r m => SessionId -> m (Maybe UserId)
findUserIdBySessionId sId = do
  tvar <- asks getter
  liftIO $ lookup sId . stateSessions <$> readTVarIO tvar
