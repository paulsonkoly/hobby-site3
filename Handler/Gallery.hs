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
import Yesod.Form.Nic

import Text.Lucius
import Text.Julius
import Text.Blaze.Html.Renderer.Text
import Control.Monad
import Data.Int
import Data.Maybe


-- | select galleries that the user can see plus adding the specified condition 
--
-- Only works when user is logged in
selectGalleries :: [Filter Gallery] -> Handler [Entity Gallery]
selectGalleries filter' = do
   Just (Entity uid user) <- maybeUser
   let userFilter = if userAdmin user then [] else [ GalleryUserId ==. uid ]
   runDB $ selectList (userFilter ++ filter') [Asc GalleryWeight]



-- | Turns the in database gallery structure (forest) into an aeson Value.
--
-- Gets into infinite recursion if the structure is cyclic.
galleryTreeAeson :: Handler Value
galleryTreeAeson = do
   top <- selectGalleries [ GalleryParentId ==. Nothing ]
   liftM toJSON $ mapM galleryTreeAeson' top 
   where
      galleryTreeAeson' :: Entity Gallery -> Handler Value
      galleryTreeAeson' (Entity galleryId gallery) = do
         children <- selectGalleries [ GalleryParentId ==. Just galleryId ]
         childrenAeson <- liftM toJSON $ mapM galleryTreeAeson' children
         return $ Import.object
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
   toWidget $(luciusFile "templates/ui.dynatree.lucius")
   addScript (StaticR js_dynatree_jquery_dynatree_js)
   toWidget $(juliusFile "templates/treeWidget.julius")
   $(widgetFile "treeWidget")


-- | Browse or manage galleries.
getGalleriesR :: Handler RepHtml
getGalleriesR = defaultLayout $ do
   setTitle "Image galleries"
   $(widgetFile "galleries")


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

