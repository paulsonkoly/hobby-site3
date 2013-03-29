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
import Yesod.Form.JQueryUpload

import Data.Text (pack, unpack)
import Control.Monad

import Lib.Image
import Lib.ImageType
import Lib.Accessibility


data EditableImage = EditableImage { accessibility :: Accessibility }


--imageForm :: ( RenderMessage master FormMessage)
--   => Image
--   -> Text.Blaze.Internal.Markup
--   -> MForm sub master (FormResult EditableImage, GWidget sub master ())
imageForm image = renderDivs $ EditableImage
   <$> areq (selectField optionsEnum) "Accessibility" (Just $ imageAccessibility image)



getImagesR :: Handler RepHtml
getImagesR = do
	images <- runDB $ selectList [] []
	defaultLayout $(widgetFile "images")


-- | serves an image file request
getImageFileR :: ImageType -- ^ the image file type
              -> String    -- ^ the image md5 hash
              -> GHandler s a ()
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
         eitherImage <- liftIO $ newImage f
         either
            (\errMsg -> return $ object [ "error" .= pack (show $ errMsg) ])
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
   defaultLayout $ do
      $(widgetFile "image")


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
   setMessage $ toHtml $ "Image " <> (imageOrigName image) <> " has been deleted."
   liftIO $ deleteImage $ unpack $ imageMd5Hash image
   runDB $ delete $ imageId
   redirect ImagesR
