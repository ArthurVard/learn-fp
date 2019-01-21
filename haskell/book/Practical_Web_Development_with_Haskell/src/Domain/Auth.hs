{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes      #-}

module Domain.Auth (
  -- * Types
  Auth(..),
  Email,
  mkEmail,
  rawEmail,
  Password,
  mkPassword,
  rawPassword,
  UserId,
  VerificationCode,
  SessionId,
  RegistrationError(..),
  EmailVerificationError(..),
  LoginError(..),

  -- * Ports
  AuthRepo(..),
  EmailVerificationNotif(..),
  SessionRepo(..),

  -- * Use cases
  register,
  verifyEmail,
  login,
  resolveSessionId,
  getUser
) where


import ClassyPrelude
import Control.Monad.Except
import Data.Either
import Domain.Validation
import Text.Regex.PCRE.Heavy

import Data.Text (Text)
-------------------------------------------------------------------------------
--
data Auth = Auth
    { authEmail    :: Email
    , authPassword :: Password
    } deriving (Show, Eq)

newtype Email = Email
    { emailRaw :: Text
    } deriving (Show, Eq)

newtype Password = Password
    { passwordRaw :: Text
    } deriving (Show, Eq)

type UserId = Int
type SessionId = Text

data LoginError = LoginErrorInvalidAuth
                | LoginErrorEmailNotVerified
                  deriving (Show, Eq)


data RegistrationError  = RegistrationErrorEmailTaken
                          deriving (Show, Eq)

type VerificationCode = Text

class Monad m => AuthRepo m where
    addAuth :: Auth -> m (Either RegistrationError VerificationCode)
    setEmailAsVerified :: VerificationCode -> m (Either EmailVerificationError ())
    findUserByAuth :: Auth -> m (Maybe (UserId, Bool))
    findEmailFromUserId :: UserId -> m (Maybe Email)

class Monad m => EmailVerificationNotif m where
    notifyEmailVerification :: Email -> VerificationCode -> m ()

class Monad m => SessionRepo m where
    newSession :: UserId -> m SessionId
    findUserIdBySessionId :: SessionId -> m (Maybe UserId)


data EmailVerificationError = EmailVerificationErrorInvalidCode
                              deriving (Show, Eq)


-------------------------------------------------------------------------------
--

rawEmail :: Email -> Text
rawEmail = emailRaw

rawPassword :: Password -> Text
rawPassword = passwordRaw


-- >>> mkEmail "test"
-- Left ["Not a valid email"]
-- >>> mkEmail "test@example.com"
-- Right (Email {emailRaw = "test@example.com"})
mkEmail :: Text -> Either [Text] Email
mkEmail = validate Email
          [ regexMatches
            [re|^[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,64}$|]
            "Not a valid email" ]


-- >>>  mkPassword "ABC"
-- Left ["Should between 5 and 50","Should contain number","Should contain lowercase letter"]
-- >>> mkPassword "1234ABCdef"
-- Right (Password {passwordRaw = "1234ABCdef"})
mkPassword :: Text -> Either [Text] Password
mkPassword = validate Password
             [ lengthBetween 5 50 "Should between 5 and 50"
             , regexMatches [re|\d|] "Should contain number"
             , regexMatches [re|[A-Z]|] "Should contain uppercase letter"
             , regexMatches [re|[a-z]|] "Should contain lowercase letter"
             ]


-------------------------------------------------------------------------------
-- Registration





register ::
    (AuthRepo m, EmailVerificationNotif m) =>
    Auth -> m (Either RegistrationError ())
register auth = runExceptT $ do
                  vCode <- ExceptT $ addAuth auth
                  let email = authEmail auth
                  lift $ notifyEmailVerification email vCode


instance AuthRepo IO where
    addAuth (Auth email pass) = do
                       putStrLn $ "adding auth: " <> rawEmail email
                       return $ Right "fake verification code"

instance EmailVerificationNotif IO where
    notifyEmailVerification email vcode =
        putStrLn $ "Notify " <> rawEmail email <> " - " <> vcode


verifyEmail :: AuthRepo m => VerificationCode -> m (Either EmailVerificationError ())
verifyEmail = setEmailAsVerified




-- test
ex03 :: IO (Either RegistrationError ())
ex03 = do
  let Right email = mkEmail "test@example.com"
      Right password = mkPassword "1234ABCdef"
      auth = Auth email password
  register auth


-------------------------------------------------------------------------------
-- Login and Session
login :: (AuthRepo m, SessionRepo m) => Auth -> m (Either LoginError SessionId)
login auth = runExceptT $ do
               result <- lift $ findUserByAuth auth
               case result of
                 Nothing         -> throwError LoginErrorInvalidAuth
                 Just (_, False) -> throwError LoginErrorEmailNotVerified
                 Just (uId, _)   -> lift $ newSession uId


resolveSessionId :: SessionRepo m => SessionId -> m (Maybe UserId)
resolveSessionId = findUserIdBySessionId


getUser :: AuthRepo m => UserId -> m (Maybe Email)
getUser = findEmailFromUserId
