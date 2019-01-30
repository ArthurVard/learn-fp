module Adapter.HTTP.Common where

import ClassyPrelude
import Data.Aeson    hiding (json)

import Network.HTTP.Types.Status
import Web.Scotty.Trans

import qualified Text.Digestive.Aeson as DF
import qualified Text.Digestive.Form  as DF
import qualified Text.Digestive.Types as DF


-------------------------------------------------------------------------------
-- Common functionalities that may be
-- shared throughout other HTTP routes, for example, setting and
-- reading cookies


parseAndValidateJSON :: (ScottyError e, MonadIO m, ToJSON v)
 => DF.Form v m a -> ActionT e m a
parseAndValidateJSON form = do
  val <- jsonData `rescue` (\_ -> return Null)
  validationResult <- lift $ DF.digestJSON form val
  case validationResult of
    (v, Nothing) -> do
      status status400
      json $ DF.jsonErrors v
      finish
    (_, Just result) -> return result
