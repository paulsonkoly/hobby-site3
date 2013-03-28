module Handler.Image
   ( getImagesR
   , postImagesR
   , getCreateImageR
   , postCreateImageR
   , getImageR
   , getEditImageR
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


getImagesR :: Handler RepHtml
getImagesR = do
	images <- runDB $ selectList [] []
	defaultLayout $(widgetFile "images")


postImagesR :: Handler RepHtml
postImagesR = notFound


getCreateImageR :: Handler RepHtml
getCreateImageR = do
   let imageUploadWidget = jQUploadWidget
   defaultLayout $(widgetFile "imageUpload")


postCreateImageR :: Handler RepJson
postCreateImageR = do
   files <- lookupFiles "files[]"
   Just uid <- maybeAuthId
   jsonContent <- forM files $ \f -> do
         $(logDebug) $ "File upload request " <> fileName f
         eitherImage <- liftIO $ newImage f uid
         either
            (\errMsg -> return $ object [ "error" .= pack (show $ errMsg) ])
            (\image -> do
               imageId  <- runDB $ insert $ snd image
               renderer <- getUrlRender
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
      -- setTitle $ toHtml $  user
      $(widgetFile "image")



getEditImageR :: ImageId -> Handler RepHtml
getEditImageR _ = notFound


postDeleteImageR :: ImageId -> Handler RepHtml
postDeleteImageR imageId = do
   image <- runDB $ get404 imageId
   setMessage $ toHtml $ "Image " <> (imageOrigName image) <> " has been deleted."
   liftIO $ deleteImage $ unpack $ imageMd5Hash image
   runDB $ delete $ imageId
   redirect ImagesR
