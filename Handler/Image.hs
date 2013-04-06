module Handler.Image
   ( getImagesR
   , getCreateImageR
   , postCreateImageR
   , getImageR
   , getImageFileR 
   , getEditImageR
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


jQUploadWidget :: Widget
jQUploadWidget = do
   addStylesheet $ StaticR css_fileupload_jquery_fileupload_ui_css
   addStylesheet $ StaticR css_fileupload_style_css
   addScriptRemote "//code.jquery.com/jquery-1.9.1.min.js"
   addScriptRemote "//netdna.bootstrapcdn.com/twitter-bootstrap/2.3.1/js/bootstrap.min.js"
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


imageForm :: RenderMessage master FormMessage
          => Image
          -> Html 
          -> MForm sub master (FormResult EditableImage, GWidget sub master ())
imageForm image = renderDivs $ EditableImage
   <$> areq (selectField optionsEnum) "Accessibility" (Just $ imageAccessibility image)



getImagesR :: Handler RepHtml
getImagesR = do
   mentity <- maybeAuth
   images <- runDB $ selectList 
      ( case mentity of
         Just (Entity uid user) -> if userAdmin user
            then [] 
            else [ ImageAccessibility ==. Public ]
               ||. [ ImageAccessibility ==. Member, ImageUserId ==. uid ]
         Nothing -> [ ImageAccessibility ==. Public ]
      ) []
   defaultLayout $(widgetFile "images")


-- | serves an image file request
getImageFileR :: ImageType -- ^ the image file type
              -> String    -- ^ the image md5 hash
              -> Handler ()
getImageFileR i s = sendFile typeJpeg $ imageFilePath i s



getCreateImageR :: Handler RepHtml
getCreateImageR = do
   let imageUploadWidget = jQUploadWidget
   defaultLayout $(widgetFile "imageUpload")


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


getEditImageR :: ImageId -> Handler RepHtml
getEditImageR imageId = do
   image <- runDB $ get404 imageId
   (imageFormWidget, enctype) <- generateFormPost $ imageForm image
   defaultLayout $(widgetFile "editImage")


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


postDeleteImageR :: ImageId -> Handler RepHtml
postDeleteImageR imageId = do
   image <- runDB $ get404 imageId
   setMessage $ toHtml $ "Image " <> imageOrigName image <> " has been deleted."
   liftIO $ deleteImage $ unpack $ imageMd5Hash image
   runDB $ delete imageId
   redirect ImagesR
