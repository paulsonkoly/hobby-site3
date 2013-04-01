module Handler.Gallery
   ( getGalleriesR
   , getNewGalleryR
   , postNewGalleryR
   )
where

import Import

import Yesod.Auth
--import Yesod.Form.Fields

import Text.Lucius
--import Text.Julius
--import Data.Monoid
--import Data.Aeson.Types
--import Data.Text (unpack)
import Control.Monad
import Data.Int
-- import Data.Maybe


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
            ]


treeWidgetScript :: Value -> Widget
treeWidgetScript galleriesAeson = toWidget $ [julius|
      $(function(){
         $("#tree").dynatree({
            onActivate: function(node) {
               alert("You activated " + node.data.title);
            },
            persist: true,
            checkbox: true,
            selectMode: 3,
            children: #{galleriesAeson}
         });
      });
   |]


treeWidget :: Value -> Widget
treeWidget galleriesAeson = do 
   addScriptRemote "//code.jquery.com/jquery-1.9.1.min.js"
   addScript (StaticR js_dynatree_jquery_ui_custom_js)
   addScript (StaticR js_dynatree_jquery_cookie_js)
   toWidget $(luciusFile "templates/ui.dynatree.lucius")
   addScript (StaticR js_dynatree_jquery_dynatree_js)
   treeWidgetScript galleriesAeson
   [whamlet|<div id="tree">|]


getGalleriesR :: Handler RepHtml
getGalleriesR = do
   galleriesAeson <- galleryTreeAeson
   defaultLayout $ do
      setTitle "Image galleries"
      $(widgetFile "galleries")


data EditableGallery = EditableGallery
   { name        :: Text
   , description :: Maybe Text
   , parent      :: Maybe (Entity Gallery)
   , weight      :: Int64
   } deriving Eq


-- | form for an image gallery
-- galleryForm :: Maybe Gallery -> Html -> Form EditableGallery
galleryForm mgallery = renderDivs $ EditableGallery
   <$> areq textField "Name" (Just $ maybe "" galleryName mgallery)
   <*> aopt textField "Description" (liftM galleryDescription mgallery)
   <*> aopt parentField "Parent gallery" Nothing
   <*> areq intField "Weigth" (Just $ maybe 0 galleryWeight mgallery)
   where
      parentOptions = optionsPersist [] [Desc GalleryWeight] galleryName
      parentField = selectField parentOptions


getNewGalleryR :: Handler RepHtml
getNewGalleryR = do
   (galleryWidget, enctype) <- generateFormPost $ galleryForm Nothing
   defaultLayout $ do
      setTitle "Creating a new gallery"
      $(widgetFile "galleryForm")


postNewGalleryR :: Handler RepHtml
postNewGalleryR = do
   Just authId <- maybeAuthId
   ((res, galleryWidget), enctype) <- runFormPost $ galleryForm Nothing
   case res of 
      FormSuccess editable -> do
         _ <- runDB $ insert $ Gallery
            { galleryName = name editable
            , galleryUserId = authId
            , galleryDescription = description editable
            , galleryParentId = liftM entityKey $ parent editable
            , galleryWeight = weight editable
            }
         setMessage $ toHtml ("The gallery has successfully been created." :: Text)
         redirect $ GalleriesR
      _ -> defaultLayout $ do
         setTitle "Please correct your form"
         $(widgetFile "galleryForm")

