{- |
Module      :  $Header$
Description :  Gallery related handlers
Copyright   :  (c) Paul Sonkoly
License     :  AllRightsReserved

Maintainer  :  sonkoly.pal@gmail.com
Stability   :  stable
Portability :  portable
-}

module Handler.Gallery
   ( -- * Handlers
     getGalleryR
   , getGalleriesR
   -- ** Gallery manipulation
   , getManageGalleriesR
   , getNewGalleryR
   , getNewChildGalleryR
   , postNewGalleryR
   , getEditGalleryR
   , postEditGalleryR
   -- *** AJAX for the gallery manipulation (tree view) page
   , getGalleryTreeR
   , postMoveGalleryR
   , postMoveTopGalleryR
   , postDeleteGalleryR
   -- ** Gallery/images association
   , getImagesGalleryR
   -- *** AJAX for the image association page
   , postAcquireImagesR
   , postRemoveImagesR
   , postAddImagesR
   )
where

import Import

import Yesod.Auth
import Yesod.Form.Nic

import Text.Blaze.Html.Renderer.Text
import Data.Text (unpack)
import Control.Monad
import Data.Int
import Data.Maybe
import Data.Aeson.Types hiding (object)

import Lib.ImageType


catAuthorized :: (Owned t) => [ Entity t ] -> Handler [ Entity t ]
catAuthorized entities = liftM catMaybes $ forM entities $ \entity -> do
   read' <- toOwnership entity >>= canRead
   return $ if read' == Authorized then Just entity else Nothing


-- | The gallery browser
getGalleryR' :: Maybe GalleryId -> Handler RepHtml
getGalleryR' mGalleryId = do
   maid <- maybeAuthId
   mGallery <- maybe (return Nothing) (runDB . get) mGalleryId
   allChildren <- childrenGallery mGalleryId [ Asc GalleryWeight ]
   children <- liftM catMaybes $ forM allChildren $ \child -> do
         thumbnail <- thumbnailGallery maid $ entityKey child
         return $ maybe Nothing (\image -> Just (image, child)) thumbnail
   images <- maybe (return []) (\galleryId -> imagesGallery galleryId >>= catAuthorized) mGalleryId
   defaultLayout $(widgetFile "galleries")


-- | The gallery browser viewing the top level gallery
getGalleriesR :: Handler RepHtml
getGalleriesR = getGalleryR' Nothing


-- | The gallery browser showing the requested gallery
getGalleryR :: GalleryId -> Handler RepHtml
getGalleryR = getGalleryR' . Just


-- | Turns the in database gallery structure (forest) into an aeson Value.
--
-- Gets into infinite recursion if the structure is cyclic.
galleryTreeAeson :: Handler Value
galleryTreeAeson = do
   top <- childrenGallery Nothing [Asc GalleryWeight]
   liftM toJSON $ mapM galleryTreeAeson' top 
   where
      galleryTreeAeson' :: Entity Gallery -> Handler Value
      galleryTreeAeson' (Entity galleryId gallery) = do
         children <- childrenGallery (Just galleryId) [Asc GalleryWeight ]
         childrenAeson <- liftM toJSON $ mapM galleryTreeAeson' children
         return $ object
            [ "title"    .= galleryName gallery
            , "tooltip"  .= liftM renderHtml (galleryDescription gallery)
            , "isFolder" .= True
            , "children" .= childrenAeson
            , "key"      .= galleryId
            ]


-- | Serves the AJAX request to the gallery tree view widget
getGalleryTreeR :: Handler RepJson
getGalleryTreeR = galleryTreeAeson >>= jsonToRepJson


-- | Gallery tree view widget
treeWidget :: Widget
treeWidget = do 
   addScriptRemote "//code.jquery.com/jquery-1.9.1.min.js"
   addScript (StaticR js_dynatree_jquery_ui_custom_js)
   addScript (StaticR js_dynatree_jquery_cookie_js)
   addScript (StaticR js_dynatree_jquery_dynatree_js)
   $(widgetFile "treeWidget")


-- | Manage galleries.
getManageGalleriesR :: Handler RepHtml
getManageGalleriesR = defaultLayout $ do
   setTitle "Image gallery manager"
   $(widgetFile "manageGalleries")


-- | Fields that we can edit on a Gallery
data EditableGallery = EditableGallery
   { name        :: Text
   , description :: Maybe Html
   , parentId    :: Maybe GalleryId
   , weight      :: Int64
   } 


instance Eq EditableGallery where 
   (==) a b = name a == name b


-- | Adds sesnible size to textareas in a widget
--
-- Without this the nicHtml looks awful
sensibleTextarea :: Widget -> Widget
sensibleTextarea w = 
   toWidget [lucius| textarea { width: 600px; height: 200px; } |] <> w


-- | Form for an image gallery
galleryForm :: (RenderMessage master FormMessage, YesodNic master)
   => Maybe Gallery   -- ^ gallery for default values
   -> Maybe GalleryId -- ^ parentId to be passed in a hidden field
   -> Html -> MForm sub master (FormResult EditableGallery, GWidget sub master ())
galleryForm mgallery mparentId = renderDivs $ EditableGallery
      <$> areq textField "Name"           (Just $ maybe "" galleryName mgallery)
      <*> aopt nicHtmlField "Description" (liftM galleryDescription mgallery)
      <*> aopt hiddenField ""             (Just mparentId)
      <*> areq intField "Weigth"          (Just $ maybe 0 galleryWeight mgallery)


generateGalleryForm :: Maybe Gallery -> Maybe GalleryId -> Handler (Widget, Enctype)
generateGalleryForm mgallery mparentId = do
   (galleryWidget, enctype) <- generateFormPost $ galleryForm mgallery mparentId
   return (sensibleTextarea galleryWidget, enctype)


runGalleryForm ::  Maybe Gallery -> Handler ((FormResult EditableGallery, Widget), Enctype)
runGalleryForm mgallery = do
   ((res, galleryWidget), enctype) <- runFormPost $ galleryForm mgallery Nothing
   return ((res, sensibleTextarea galleryWidget), enctype)


formHandler :: Widget -> Enctype -> Html -> Route App -> Handler RepHtml
formHandler galleryWidget enctype title route = do
   renderer <- getUrlRender
   defaultLayout $ do
      setTitle title 
      let destination = renderer route
      $(widgetFile "galleryForm")


-- | Gallery creation form for a top level gallery.
getNewGalleryR :: Handler RepHtml
getNewGalleryR = getNewGalleryR' Nothing


-- | Gallery creation form for a gallery under the specified parent
getNewChildGalleryR
   :: GalleryId       -- ^ The parent gallery
   -> Handler RepHtml
getNewChildGalleryR = getNewGalleryR' . Just


getNewGalleryR' :: Maybe GalleryId -> Handler RepHtml
getNewGalleryR' mparentId = do
   (galleryWidget, enctype) <- generateGalleryForm Nothing mparentId
   formHandler galleryWidget enctype "Creating a new gallery" NewGalleryR


-- | Gallery creation POST handler
postNewGalleryR :: Handler RepHtml
postNewGalleryR = do
   Just authId <- maybeAuthId
   ((res, galleryWidget), enctype) <- runGalleryForm Nothing
   case res of 
      FormSuccess editable -> do
         _ <- runDB $ insert Gallery
            { galleryName        = name editable
            , galleryUserId      = authId
            , galleryDescription = description editable
            , galleryParentId    = parentId editable
            , galleryWeight      = weight editable
            }
         setMessage $ toHtml ("The gallery has successfully been created." :: Text)
         redirect GalleriesR
      _ -> formHandler galleryWidget enctype "Creating a new gallery" NewGalleryR


-- | Edit form for a gallery
getEditGalleryR :: GalleryId -> Handler RepHtml
getEditGalleryR galleryId = do
   gallery <- runDB $ get404 galleryId
   (galleryWidget, enctype) <- generateGalleryForm (Just gallery) Nothing
   formHandler galleryWidget enctype "Edit gallery" (EditGalleryR galleryId)


-- | Gallery edit POST handler
postEditGalleryR :: GalleryId -> Handler RepHtml
postEditGalleryR galleryId = do
   gallery <- runDB $ get404 galleryId
   ((res, galleryWidget), enctype) <- runGalleryForm (Just gallery)
   case res of 
      FormSuccess editable -> do
         runDB $ update galleryId
            [ GalleryName        =. name editable
            , GalleryDescription =. description editable
            , GalleryWeight      =. weight editable
            ]
         setMessage $ toHtml $ "The gallery " <> name editable <> " has successfully been updated."
         redirect GalleriesR
      _ -> formHandler galleryWidget enctype "Edit gallery" $ EditGalleryR galleryId


-- | Would it cause a cycle if we moved 'what' under 'whereTo'
detectCycle
   :: GalleryId       -- ^ 'what', the gallery to be moved
   -> Maybe GalleryId -- ^ 'whereTo', Maybe the destination gallery
   -> Handler Bool
detectCycle what (Just whereTo) = if what == whereTo
   then return True
   else do
      self <- runDB $ get whereTo
      let up = galleryParentId $ fromJust self
      detectCycle what up
detectCycle _ Nothing = return False


-- | Gallery move POST handler
-- 
-- We should refuse creating cycles in the paretial relations of galleries.
postMoveGalleryR :: GalleryId -> GalleryId -> Handler RepJson
postMoveGalleryR what whereTo = do
   cyclic <- detectCycle what (Just whereTo)
   response <- if cyclic 
      then return $ object [ "message" .= ("Cannot create a cycle in parential relations." :: Text) ]
      else do
         runDB $ update what [ GalleryParentId =. Just whereTo ]
         return $ toJSON ()
   jsonToRepJson response 


-- | Gallery move to the top POST handler
postMoveTopGalleryR :: GalleryId -> Handler RepJson
postMoveTopGalleryR what = do
   runDB $ update what [ GalleryParentId =. Nothing ]
   jsonToRepJson $ toJSON ()


deleteGallery :: GalleryId -> Handler ()
deleteGallery galleryId = do
   children <- childrenGallery (Just galleryId) []
   mapM_ (deleteGallery . entityKey) children
   runDB $ deleteCascadeWhere [ GalleryId ==. galleryId ]


-- | Handles gallery deletion requests.
--
-- TODO : Does not give any status information
postDeleteGalleryR :: GalleryId -> Handler RepJson
postDeleteGalleryR galleryId = do
   deleteGallery galleryId
   jsonToRepJson $ toJSON ()


-- | Serves the page where users can move images into their galleries
getImagesGalleryR :: GalleryId -> Handler RepHtml
getImagesGalleryR galleryId = do
   Just (Entity uid user) <- maybeAuth
   let uidCond = if userAdmin user then [] else [ ImageUserId ==. uid ] 
   galleryImages <- liftM (map entityKey) $ imagesGallery galleryId
   images <- liftM concat $ forM galleryImages (\iid ->
      runDB $ selectList (uidCond ++ [ ImageId ==. iid ]) [])
   defaultLayout $ do
      addScriptRemote "//code.jquery.com/jquery-1.9.1.min.js"
      $(widgetFile "imagesGallery")


-- | Acquires all images not present in any gallery
--
-- Images here are limited to true ownership ie admins don't pick up
-- other users images. However admins can run acquire on anybodies
-- galleries.
postAcquireImagesR :: GalleryId -> Handler RepJson
postAcquireImagesR galleryId = do
   Just (Entity uid _ ) <- maybeAuth
   -- this is horrendously bad
   adopteeIds <- liftM (map $ imageGalleryImageId . entityVal) . runDB $ selectList [] []
   orphanIds  <- liftM (map entityKey) . runDB $ selectList
      [ ImageId     /<-. adopteeIds
      , ImageUserId ==. uid
      ] []
   mapM_ ((runDB . insert_) . flip ImageGallery galleryId) orphanIds
   jsonToRepJson $ toJSON ()


-- | Removes specified images from the gallery 
--
-- JSON format @[ ImageId ]@
postRemoveImagesR :: GalleryId -> Handler RepJson
postRemoveImagesR galleryId = do
   reqData <- parseJsonBody
   response <- case reqData of
      Success dat -> liftM (toJSON . catMaybes) $ mapM (\pathPiece -> do
            let mImageId = fromPathPiece pathPiece
            maybe (return Nothing) (\imageId -> do
               runDB $ deleteWhere
                  [ ImageGalleryGalleryId ==. galleryId
                  , ImageGalleryImageId   ==. imageId
                  ]
               return $ Just $ toJSON imageId
               ) mImageId
         ) dat
      Error msg -> return $ object [ "message" .= msg ]
   jsonToRepJson response


-- | JSON format representation for AddImagesR
data AddImagesData = AddImagesData { whereTo' :: Text, imageIds :: [ImageId] }
instance FromJSON AddImagesData where
   parseJSON (Object v) = AddImagesData <$> v .: "gallery" <*> v .: "images"
   parseJSON _ = mzero


-- | Adds specified images to a gallery
--
-- JSON format : @{ gallery: Text, images: [ ImageId ] }@
-- TODO : passing the galleryId as argument
postAddImagesR :: Handler RepJson
postAddImagesR = do
   reqData <- parseJsonBody
   response <- case reqData of
      Success dat -> do
         Entity galleryId gallery <- runDB $ getBy404 $ UniqueGallery $ whereTo' dat
         authorization <- toOwnership (Entity galleryId gallery) >>= isOwner
         unless (authorization == Authorized) $ permissionDenied "You have to own the other gallery"
         forM_ (imageIds dat) $ \imageId ->
            runDB $ insert ImageGallery
               { imageGalleryImageId   = imageId
               , imageGalleryGalleryId = galleryId
               } 
         return $ toJSON ()
      Error msg   -> return $ object [ "message" .= msg ]
   jsonToRepJson response
