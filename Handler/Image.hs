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
import Data.Text (unpack, append)
import Data.Conduit
import Data.Conduit.Binary
import Control.Monad

import Lib.ImageInfo

getImagesR :: Handler RepHtml
getImagesR = defaultLayout $ setTitle "image listing" 

postImagesR :: Handler RepHtml
postImagesR = notFound


uploadWidget = do
   addStylesheet   $ StaticR css_jquery_fileupload_ui_css
   addStylesheet   $ StaticR css_style_css
   addScriptRemote   "//code.jquery.com/jquery-1.9.1.min.js"
   addScriptRemote   "//netdna.bootstrapcdn.com/twitter-bootstrap/2.3.1/js/bootstrap.min.js"
   addScript       $ StaticR js_load_image_js
   addScript       $ StaticR js_tmpl_js
   addScript       $ StaticR js_canvas_to_blob_js
   addScript       $ StaticR js_vendor_jquery_ui_widget_js
   -- The Iframe Transport is required for browsers without support for XHR file uploads
   addScript       $ StaticR js_jquery_iframe_transport_js
   -- The basic File Upload plugin
   addScript       $ StaticR js_jquery_fileupload_js
   -- The File Upload file processing plugin
   addScript       $ StaticR js_jquery_fileupload_fp_js
   -- The File Upload user interface plugin
   addScript       $ StaticR js_jquery_fileupload_ui_js
   -- The main application script
   addScript       $ StaticR js_main_js
   $(widgetFile "fileUpload") 



getCreateImageR :: Handler RepHtml
getCreateImageR = do
   let imageUploadWidget = uploadWidget
   defaultLayout $(widgetFile "imageUpload") 


postCreateImageR :: Handler RepJson
postCreateImageR = do
   files <- lookupFiles "files[]"
   sizes <- forM files $ \f -> do
      $(logDebug) $ "File upload request " `Data.Text.append` fileName f
      liftIO $ runResourceT $ fileSource f $$ sinkFile $ filePath f 
      imageInfo <- liftIO $ getImageInfo $ filePath f 
      return $ byteSize imageInfo
   jsonToRepJson $ object [ "files" .= zipWith toJsonFile files sizes ]
   where
      filePath = unpack . fileName
      toJsonFile info size = object
         [ "name" .= fileName info
         , "size" .= toInteger size
         ]


getImageR :: ImageId -> Handler RepHtml
getImageR _ = notFound

getEditImageR :: ImageId -> Handler RepHtml
getEditImageR _ = notFound

postDeleteImageR :: ImageId -> Handler RepHtml
postDeleteImageR _ = notFound
