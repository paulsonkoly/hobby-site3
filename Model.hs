module Model where

import Prelude
import Yesod
import qualified Yesod.Auth.HashDB as HDB
import Data.Text (Text)
import Data.Time.Clock
import Data.Int
import Database.Persist.Quasi
import Lib.Accessibility


-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")


instance Eq Gallery where
   (==) a b = (galleryName a == galleryName b)


instance (PathPiece t) => PathPiece (Maybe t) where
   toPathPiece (Just galleryId) = toPathPiece galleryId
   toPathPiece Nothing          = "nothing"
   fromPathPiece "nothing"      = Nothing
   fromPathPiece text           = Just $ fromPathPiece $ text


instance HDB.HashDBUser (User) where
  userPasswordHash = Just . userHash
  userPasswordSalt = Just . userSalt 
  setSaltAndPasswordHash s h u = u { userHash = h
                                   , userSalt = s
                                   }


