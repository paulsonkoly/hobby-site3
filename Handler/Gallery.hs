module Handler.Gallery
   ( getGalleriesR
   , getNewGalleryR
   , postNewGalleryR
   )
where

import Import
import Text.Lucius
import Text.Julius
import Data.Monoid
import Data.Aeson.Types
import Data.Text (unpack)
import Control.Monad


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
            [ "title"    .= galleryName gallery
            , "isFolder" .= True
            , "children" .= childrenAeson
            ]


treeWidgetScript :: Value -> Widget
treeWidgetScript galleriesAeson = toWidget $ [julius|
      $(function(){
         $("#tree").dynatree({
            onActivate: function(node) {
               alert("You activated " + node.data.title);
            },
            persist: true,
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
   defaultLayout $ toWidget [whamlet|<h1>Galleries:</h1>^{treeWidget galleriesAeson}|]


getNewGalleryR :: Handler RepHtml
getNewGalleryR = notFound

postNewGalleryR :: Handler RepHtml
postNewGalleryR = notFound
