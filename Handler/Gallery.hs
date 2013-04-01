module Handler.Gallery
   ( getGalleriesR
   , getNewGalleryR
   , postNewGalleryR
   , getNewChildGalleryR
   , getEditGalleryR
   , postEditGalleryR
   , postMoveGalleryR
   , postDeleteGalleryR
   )
where

import Import

import Yesod.Auth

import Text.Lucius
import Text.Julius
import Control.Monad
import Data.Int


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


treeWidget :: Value -> Widget
treeWidget galleriesAeson = do 
   addScriptRemote "//code.jquery.com/jquery-1.9.1.min.js"
   addScript (StaticR js_dynatree_jquery_ui_custom_js)
   addScript (StaticR js_dynatree_jquery_cookie_js)
   toWidget $(luciusFile "templates/ui.dynatree.lucius")
   addScript (StaticR js_dynatree_jquery_dynatree_js)
   toWidget $(juliusFile "templates/treeWidget.julius")
   $(widgetFile "treeWidget")


getGalleriesR :: Handler RepHtml
getGalleriesR = do
   galleriesAeson <- galleryTreeAeson
   defaultLayout $ do
      setTitle "Image galleries"
      $(widgetFile "galleries")


data EditableGallery = EditableGallery
   { name        :: Text
   , description :: Maybe Text
   , parentId    :: Maybe GalleryId
   , weight      :: Int64
   } deriving Eq


-- | form for an image gallery
-- galleryForm :: Maybe Gallery -> Html -> Form EditableGallery
galleryForm mgallery mparentId = renderDivs $ EditableGallery
      <$> areq textField "Name"        (Just $ maybe "" galleryName mgallery)
      <*> aopt textField "Description" (liftM galleryDescription mgallery)
      <*> aopt hiddenField ""          (Just mparentId)
      <*> areq intField "Weigth"       (Just $ maybe 0 galleryWeight mgallery)


getNewGalleryR :: Handler RepHtml
getNewGalleryR = getNewGalleryR' Nothing


getNewChildGalleryR :: GalleryId -> Handler RepHtml
getNewChildGalleryR = getNewGalleryR' . Just


getNewGalleryR' :: Maybe GalleryId -> Handler RepHtml
getNewGalleryR' mparentId = do
   (galleryWidget, enctype) <- generateFormPost $ galleryForm Nothing mparentId
   renderer <- getUrlRender
   defaultLayout $ do
      setTitle "Creating a new gallery"
      let destination = renderer NewGalleryR
      $(widgetFile "galleryForm")


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


getEditGalleryR :: GalleryId -> Handler RepHtml
getEditGalleryR galleryId = do
   gallery <- runDB $ get404 galleryId
   renderer <- getUrlRender
   (galleryWidget, enctype) <- generateFormPost $ galleryForm (Just gallery) Nothing
   defaultLayout $ do
      setTitle $ "Edit gallery"
      let destination = renderer $ EditGalleryR galleryId
      $(widgetFile "galleryForm")


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


postMoveGalleryR :: GalleryId -> GalleryId -> Handler RepJson
postMoveGalleryR what whereTo = do
   runDB $ update what [ GalleryParentId =. Just whereTo ]
   jsonToRepJson $ object $ [ "message" .= ("Gallery has been moved." :: Text) ]


postDeleteGalleryR :: GalleryId -> Handler RepJson
postDeleteGalleryR galleryId = do
   mgallery <- runDB $ get galleryId
   _ <- runDB $ delete galleryId
   jsonToRepJson $ object $ case mgallery of
      Just gallery -> [ "message" .= ("Gallery " <> galleryName gallery <> " has been deleted.") ]
      Nothing -> [ "message" .= ("Not found." :: Text) ]

