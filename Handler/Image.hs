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

import Data.Text (append, unpack)
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
   infos <- forM files $ \f -> do
      $(logDebug) $ "File upload request " `Data.Text.append` fileName f
      imageData <- liftIO $ newImage f uid
      either (const $ return ()) (runDB . insert_ . snd) imageData
      return imageData
   jsonToRepJson $ object [ "files" .= zipWith toJsonFile files infos ]
   where
      toJsonFile file (Right info) = object
         [ "name" .= fileName file
         , "size" .= fst info
         ]
      toJsonFile _ (Left errMsg) = object
         [ "error" .= errMsg ]


getImageR :: ImageId -> Handler RepHtml
getImageR imageId = do
   image <- runDB $ get404 imageId
   defaultLayout $ do
      -- setTitle $ toHtml $  user
      $(widgetFile "image")


getEditImageR :: ImageId -> Handler RepHtml
getEditImageR _ = notFound

postDeleteImageR :: ImageId -> Handler RepHtml
postDeleteImageR _ = notFound
