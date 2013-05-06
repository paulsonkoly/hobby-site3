{- |
Module      :  $Header$
Description :  Image related handlers
Copyright   :  (c) Paul Sonkoly
License     :  AllRightsReserved

Maintainer  :  sonkoly.pal@gmail.com
Stability   :  stable
Portability :  portable
-}

module Handler.Image
   ( -- * Handlers
     getCreateImageR
   , postCreateImageR
   , getImageR
   , getImageFileR 
   , getEditImageR
   , postAccessibilityImageR
   , postEditImageR
   , postDeleteImageR
   )
where

import Import

import Yesod.Auth

import Data.Text (pack, unpack)

import Control.Monad

import Lib.Image
import Lib.ImageType
import Lib.Accessibility
import Lib.ToMarkup()


-- | @Image@ upload @Widget@
jQUploadWidget :: Widget
jQUploadWidget = do
   addStylesheet $ StaticR css_fileupload_jquery_fileupload_ui_css
   addScript $ StaticR js_fileupload_load_image_js
   addScript $ StaticR js_fileupload_tmpl_js
   addScript $ StaticR js_fileupload_canvas_to_blob_js
   addScript $ StaticR js_fileupload_tmpl_js
   addScript $ StaticR js_fileupload_jquery_ui_widget_js
   -- The Iframe Transport is required for browsers without support for XHR file uploads
   addScript $ StaticR js_fileupload_jquery_iframe_transport_js
   -- The basic File Upload plugin
   addScript $ StaticR js_fileupload_jquery_fileupload_js
   -- The File Upload file processing plugin
   addScript $ StaticR js_fileupload_jquery_fileupload_fp_js
   -- The File Upload user interface plugin
   addScript $ StaticR js_fileupload_jquery_fileupload_ui_js
   -- The main application script
   addScript $ StaticR js_fileupload_main_js
   $(widgetFile "uploadWidget")


data EditableImage = EditableImage { accessibility :: Accessibility }


-- | @Image@ edit @Form@
imageForm :: Image -> Form EditableImage
imageForm image = renderDivs $ EditableImage
   <$> areq (selectField optionsEnum) "Accessibility" (Just $ imageAccessibility image)


-- | serves an @Image@ file request
getImageFileR :: ImageType -- ^ the image file type
              -> String    -- ^ the image md5 hash
              -> Handler ()
getImageFileR i s = neverExpires >> sendFile typeJpeg (imageFilePath i s)


-- | @Image@ upload / creation
getCreateImageR :: Handler RepHtml
getCreateImageR = do
   let imageUploadWidget = jQUploadWidget
   defaultLayout $(widgetFile "imageUpload")


-- | handles @Image@ upload
postCreateImageR :: Handler RepJson
postCreateImageR = do
   files <- lookupFiles "files[]"
   Just uid <- maybeAuthId
   renderer <- getUrlRender
   jsonContent <- forM files $ \f -> do
         $(logDebug) $ "File upload request " <> fileName f
         eitherImage <- liftIO $ mkImage f
         either
            (\errMsg -> return $ object [ "error" .= pack (show errMsg) ])
            (\image -> do
               imageId  <- runDB $ insert $ (snd image)
                  { imageUserId = uid
                  , imageAccessibility = Public 
                  }
               return $ object
                  [ "name"          .= fileName f
                  , "size"          .= fst image
                  , "url"           .= renderer (ImageR imageId)
                  , "thumbnail_url" .= renderer (ImageFileR Thumbnail (unpack $ imageMd5Hash $ snd image))
                  , "delete_url"    .= renderer (DeleteImageR imageId)
                  , "delete_type"   .= ("POST" :: Text)
                  ]
            )
            eitherImage
   jsonToRepJson $ object [ "files" .= jsonContent ]


-- | shows a single @Image@
getImageR :: ImageId -> Handler RepHtml
getImageR imageId = do
   image <- runDB $ get404 imageId
   mentity <- maybeAuth
   let canEditDelete = decide image mentity
   defaultLayout $(widgetFile "image")
   where
      decide image mentity = case mentity of
         Just (Entity uid user) -> userAdmin user || uid == imageUserId image
         Nothing                -> False


-- | serves the edit @Form@ for an @Image@
getEditImageR :: ImageId -> Handler RepHtml
getEditImageR imageId = do
   image <- runDB $ get404 imageId
   (imageFormWidget, enctype) <- generateFormPost $ imageForm image
   defaultLayout $(widgetFile "editImage")


-- | sets @Accessibility@ on Image
postAccessibilityImageR :: ImageId -> Accessibility -> Handler RepJson
postAccessibilityImageR imageId accessibility' = do
   runDB $ update imageId [ ImageAccessibility =. accessibility' ]
   jsonToRepJson $ toJSON ()


-- | handles @Image@ edit post requests
postEditImageR :: ImageId -> Handler RepHtml
postEditImageR imageId = do
   image <- runDB $ get404 imageId
   ((res, imageFormWidget), enctype) <- runFormPost $ imageForm image
   case res of 
      FormSuccess edit -> do
         runDB $ update imageId [ ImageAccessibility =. accessibility edit ]
         setMessage $ toHtml ("The image has successfully been modified." :: Text)
         redirect $ ImageR imageId
      _ -> defaultLayout $ do
         setTitle "Please correct your entry form"
         $(widgetFile "editImage") 


-- | handles @Image@ delete post requests
--
-- TODO : this responds to JSON as well as Html requests. Yesod 1.2 has selectRep
-- to support this. Once we migrate to 1.2, we should fix this up
postDeleteImageR :: ImageId -> Handler RepHtml
postDeleteImageR imageId = do
   image <- runDB $ get404 imageId
   setMessage $ toHtml $ "Image " <> imageOrigName image <> " has been deleted."
   liftIO $ deleteImage $ unpack $ imageMd5Hash image
   runDB $ deleteCascadeWhere [ ImageId ==. imageId ]
   redirect GalleriesR
