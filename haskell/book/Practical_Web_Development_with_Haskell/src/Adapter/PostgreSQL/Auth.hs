{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
module Adapter.PostgreSQL.Auth where

import ClassyPrelude
import Data.GUID
import Data.Has
import Data.Pool
import Data.Time
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Migration
import Database.PostgreSQL.Simple.SqlQQ

import qualified Control.Monad.Catch as E
import qualified Domain.Auth         as D
-------------------------------------------------------------------------------
--

type State = Pool Connection

migrate :: State -> IO ()
migrate pool = withResource pool $ \conn -> do
  result <- withTransaction conn (runMigrations False conn cmds)
  case result of
     MigrationError err -> throwString err
     _                  -> return ()
 where
   cmds =  [ MigrationInitialization
           , MigrationDirectory "src/Adapter/PostgreSQL/Migrations"
           ]


data Config = Config
 { configUrl                  :: ByteString
 , configStripeCount          :: Int
 , configMaxOpenConnPerStripe :: Int
 , configIdleConnTimeout      :: NominalDiffTime
 }


withPool :: Config -> (State -> IO a) -> IO a
withPool Config{..} action =
    bracket initPool cleanPool action
    where
      initPool = createPool openConn closeConn
                 configStripeCount
                 configIdleConnTimeout
                 configMaxOpenConnPerStripe

      cleanPool = destroyAllResources
      openConn = connectPostgreSQL configUrl
      closeConn = close


withState :: Config -> (State -> IO a) -> IO a
withState cfg action =
     withPool cfg $ \state -> do
       migrate state
       action state


type PG r m = (Has State r, MonadReader r m, MonadIO m, E.MonadThrow m)

-------------------------------------------------------------------------------
-- AuthRepo  Implementation
-------------------------------------------------------------------------------

withConn :: PG r m => (Connection -> IO a) -> m a
withConn action = do
  pool <- asks getter
  liftIO . withResource pool $ \conn -> action conn


addAuth :: PG r m => D.Auth -> m (Either D.RegistrationError (D.UserId, D.VerificationCode))
addAuth (D.Auth email pass) = do
  let rawEmail = D.rawEmail email
      rawPassw = D.rawPassword pass
  -- generate vCode
  vCode <- liftIO $ do
    r <- genText
    return $ (tshow rawEmail) <> "_" <> r
  -- issue query
  result <- withConn $ \conn ->
     try $ query conn qry (rawEmail, rawPassw, vCode)
  -- interpret result
  case result of
    Right [Only uId] -> return $ Right (uId, vCode)
    Right _ -> throwString "Should not happen: PG doesn't return userId"
    Left err@SqlError{sqlState = state, sqlErrorMsg = msg} ->
      if state == "23505" && "auths_email_key" `isInfixOf` msg
         then return $ Left D.RegistrationErrorEmailTaken
         else throwString $ "Unhandled PG exception: " <> show err
  where
     qry = [sql| insert into auths
                (email, pass, email_verification_code, is_email_verified)
                values (?, crypt(?, gen_salt('bf')), ?, 'f') returning id"
                |]



setEmailAsVerified :: PG r m => D.VerificationCode -> m (Either D.EmailVerificationError (D.UserId, D.Email))
setEmailAsVerified vCode = do
  result <- withConn $ \conn -> query conn qry (Only vCode)
  case result of
     [(uId, mail)] -> case D.mkEmail mail of
                        Right email -> return $ Right (uId, email)
                        _ -> throwString $ "Should not happen: email in DB is not valid: " <> unpack mail
     _ -> return $ Left D.EmailVerificationErrorInvalidCode
  where
    qry = [sql|update auths
               set is_email_verified = 't'
               where email_verification_code = ?
               returning id, cast (email as text)
               |]


findUserByAuth :: PG r m => D.Auth -> m (Maybe (D.UserId, Bool))
findUserByAuth (D.Auth email pass) = do
   let rawEmail = D.rawEmail email
       rawPassw = D.rawPassword pass
   result <- withConn $ \conn -> query conn qry (rawEmail, rawPassw)
   return $ case result of
     [(uId, isVerified)] -> Just (uId, isVerified)
     _                   -> Nothing
   where
     qry = [sql|select id, is_email_verified
                from auths
                where email = ? and pass = crypt(?, pass)
                |]



findEmailFromUserId :: PG r m => D.UserId -> m (Maybe D.Email)
findEmailFromUserId uId = do
   result <- withConn $ \conn -> query conn qry (Only uId)
   case result of
      [Only mail] -> case D.mkEmail mail of
                       Right email -> return $ Just email
                       _ -> throwString $ "Should not happen: email in DB is not valid:  " <> unpack mail
      _ ->  return Nothing
   where
     qry = [sql|select cast(email as text)
                from auths
                where id = ?
                |]
