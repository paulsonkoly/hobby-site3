module Handler.Gallery
   ( getGalleriesR
   , getGalleryTreeR
   , getNewGalleryR
   , postNewGalleryR
   , getNewChildGalleryR
   , getEditGalleryR
   , postEditGalleryR
   , postMoveGalleryR
   , postMoveTopGalleryR
   , postDeleteGalleryR
   )
where

import Import

import Yesod.Auth

import Text.Lucius
import Text.Julius
import Control.Monad
import Data.Int
import Data.Maybe


-- | Turns the in database gallery structure (forest) into an aeson Value.
--
-- Gets into infinite recursion if the structure is cyclic.
galleryTreeAeson :: Handler Value
galleryTreeAeson = do
   top <- runDB $ selectList [GalleryParentId ==. Nothing] [Asc GalleryWeight]
   liftM toJSON $ mapM galleryTreeAeson' top 
   where
      galleryTreeAeson' :: (Entity Gallery) -> Handler Value
      galleryTreeAeson' (Entity galleryId gallery) = do
         children <- runDB $ selectList [ GalleryParentId ==. Just galleryId ] [Asc GalleryWeight]
         childrenAeson <- liftM toJSON $ mapM galleryTreeAeson' children
         return $ Import.object
            [ "title"      .= galleryName gallery
            , "tooltip"    .= galleryDescription gallery
            , "isFolder"   .= True
            , "children"   .= childrenAeson
            , "key"        .= galleryId
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
   toWidget $(luciusFile "templates/ui.dynatree.lucius")
   addScript (StaticR js_dynatree_jquery_dynatree_js)
   toWidget $(juliusFile "templates/treeWidget.julius")
   $(widgetFile "treeWidget")


-- | Browse or manage galleries.
getGalleriesR :: Handler RepHtml
getGalleriesR = do
   defaultLayout $ do
      setTitle "Image galleries"
      $(widgetFile "galleries")


-- | Fields that we can edit on a Gallery
data EditableGallery = EditableGallery
   { name        :: Text
   , description :: Maybe Text
   , parentId    :: Maybe GalleryId
   , weight      :: Int64
   } deriving Eq


-- | Form for an image gallery
-- galleryForm :: Maybe Gallery -> Html -> Form EditableGallery
galleryForm mgallery mparentId = renderDivs $ EditableGallery
      <$> areq textField "Name"        (Just $ maybe "" galleryName mgallery)
      <*> aopt textField "Description" (liftM galleryDescription mgallery)
      <*> aopt hiddenField ""          (Just mparentId)
      <*> areq intField "Weigth"       (Just $ maybe 0 galleryWeight mgallery)


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
   (galleryWidget, enctype) <- generateFormPost $ galleryForm Nothing mparentId
   renderer <- getUrlRender
   defaultLayout $ do
      setTitle "Creating a new gallery"
      let destination = renderer NewGalleryR
      $(widgetFile "galleryForm")


-- | Gallery creation POST handler
postNewGalleryR :: Handler RepHtml
postNewGalleryR = do
   Just authId <- maybeAuthId
   renderer <- getUrlRender
   ((res, galleryWidget), enctype) <- runFormPost $ galleryForm Nothing Nothing
   case res of 
      FormSuccess editable -> do
         _ <- runDB $ insert $ Gallery
            { galleryName = name editable
            , galleryUserId = authId
            , galleryDescription = description editable
            , galleryParentId = parentId editable
            , galleryWeight = weight editable
            }
         setMessage $ toHtml ("The gallery has successfully been created." :: Text)
         redirect $ GalleriesR
      _ -> defaultLayout $ do
         setTitle "Creating a new gallery"
--         setMessage "Please correct your form"
         let destination = renderer NewGalleryR
         $(widgetFile "galleryForm")


-- | Edit form for a gallery
getEditGalleryR :: GalleryId -> Handler RepHtml
getEditGalleryR galleryId = do
   gallery <- runDB $ get404 galleryId
   renderer <- getUrlRender
   (galleryWidget, enctype) <- generateFormPost $ galleryForm (Just gallery) Nothing
   defaultLayout $ do
      setTitle $ "Edit gallery"
      let destination = renderer $ EditGalleryR galleryId
      $(widgetFile "galleryForm")


-- | Gallery edit POST handler
postEditGalleryR :: GalleryId -> Handler RepHtml
postEditGalleryR galleryId = do
   gallery <- runDB $ get404 galleryId
   renderer <- getUrlRender
   ((res, galleryWidget), enctype) <- runFormPost $ galleryForm (Just gallery) Nothing
   case res of 
      FormSuccess editable -> do
         runDB $ update galleryId
            [ GalleryName =. name editable
            , GalleryDescription =. description editable
            , GalleryWeight =. weight editable
            ]
         setMessage $ toHtml $ "The gallery " <> name editable <> " has successfully been updated."
         redirect $ GalleriesR
      _ -> defaultLayout $ do
         setTitle "Edit gallery"
--         setMessage $ toHtml "Please correct your form"
         let destination = renderer $ EditGalleryR galleryId
         $(widgetFile "galleryForm")


-- | Would it cause a cycle if we moved 'what' under 'whereTo'
detectCycle
   :: GalleryId       -- ^ 'what', the gallery to be moved
   -> Maybe GalleryId -- ^ 'whereTo', Maybe the destination gallery
   -> Handler Bool
detectCycle what (Just whereTo) = do
   if what == whereTo
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
      then return $ object $ [ "message" .= ("Cannot create a cycle in parential relations." :: Text) ]
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
   children <- runDB $ selectList [ GalleryParentId ==. Just galleryId ] []
   mapM_ (deleteGallery . entityKey) children
   runDB $ delete galleryId


-- | Handles gallery deletion requests.
--
-- TODO : Does not give any status information
postDeleteGalleryR :: GalleryId -> Handler RepJson
postDeleteGalleryR galleryId = do
   deleteGallery galleryId
   jsonToRepJson $ toJSON ()

