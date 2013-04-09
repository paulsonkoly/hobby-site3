{-# LANGUAGE FlexibleInstances #-}

module Model where

import Prelude
import Yesod
import qualified Yesod.Auth.HashDB as HDB
import Data.Text (Text)
import Data.Time.Clock
import Data.Int
import Database.Persist.Quasi
import Database.Persist.Store
import Lib.Accessibility
import Control.Monad


-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share
   [ mkPersist sqlOnlySettings
   , mkMigrate "migrateAll"
   , mkDeleteCascade sqlOnlySettings
   ] $(persistFileWith lowerCaseSettings "config/models")


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


-- naming convetions here just follows the normal rules as these were normal
-- Entity fields

-- | the images directly inside an ImageGallery
imagesGallery ::
   ( YesodPersist master
   , PersistQuery (YesodPersistBackend master (GHandler sub master))
   , PersistMonadBackend (YesodPersistBackend master (GHandler sub master)) ~ PersistEntityBackend Image
   )
   => GalleryId -- ^ id of the gallery in which we inspect the contained images 
   -> GHandler sub master [Entity Image]
imagesGallery galleryId = do
   imageIds <- liftM (map $ imageGalleryImageId . entityVal) . runDB $
      selectList [ ImageGalleryGalleryId ==. galleryId ] []
   runDB $ selectList [ImageId <-. imageIds] []


-- | the children galleries of an ImageGallery
childrenGallery :: 
   ( YesodPersist master
   , PersistQuery (YesodPersistBackend master (GHandler sub master))
   , PersistMonadBackend (YesodPersistBackend master (GHandler sub master)) ~ PersistEntityBackend Gallery
   )
   => Maybe GalleryId      -- ^ id of the gallery in which we query the siblings
   -> [SelectOpt Gallery]  -- ^ query options
   -> GHandler sub master [Entity Gallery]
childrenGallery mGalleryId = runDB . selectList [ GalleryParentId ==. mGalleryId ]

