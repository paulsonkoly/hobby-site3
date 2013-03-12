module Lib.ImageInfo
   ( ImageInfo(..) 
   , getImageInfo
   )
where

import Prelude
import Numeric (showHex)
import Control.Monad

import Data.Time.Clock
import Data.Time.Format()
import Data.Digest.Pure.MD5 (MD5Digest)
import Data.Word (Word8)
import Data.Serialize (encode)

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Graphics.Exif as Exif
import System.Posix.Files

import Crypto.Conduit

toHex :: S.ByteString -> S.ByteString 
toHex = S.concatMap word8ToHex
   where
      word8ToHex :: Word8 -> S.ByteString
      word8ToHex w = S8.pack $ pad $ showHex w []

      -- We know that the input will always be 1 or 2 characters long.
      pad :: String -> String
      pad [x] = ['0', x]
      pad s   = s

data ImageInfo = ImageInfo
   { md5Hash                  :: String
   , byteSize                 :: Integer
   , make                     :: Maybe String
   , model                    :: Maybe String
   , xResolution              :: Maybe Int
   , yResolution              :: Maybe Int
   , resolutionUnit           :: Maybe String
   , dateTime                 :: Maybe UTCTime
   , compression              :: Maybe String
   , exposureTime             :: Maybe Rational
   , fNumber                  :: Maybe Rational
   , exposureProgram          :: Maybe String
   , iSOSpeedRatings          :: Maybe Int
   , exifVersion              :: Maybe String
   , dateTimeOriginal         :: Maybe UTCTime
   , dateTimeDigitized        :: Maybe UTCTime
   , exposureBiasValue        :: Maybe Float
   , subjectDistance          :: Maybe Float
   , meteringMode             :: Maybe String
   , flash                    :: Maybe String
   , focalLength              :: Maybe Float
   , subSecTimeOriginal       :: Maybe Int
   , subSecTimeDigitized      :: Maybe Int
   , focalPlaneXResolution    :: Maybe Float
   , focalPlaneYResolution    :: Maybe Float
   , focalPlaneResolutionUnit :: Maybe String
   , customRendered           :: Maybe String
   , exposureMode             :: Maybe String
   , whiteBalance             :: Maybe String
   , sceneCaptureType         :: Maybe String
   , flashPixVersion          :: Maybe String
   , colorSpace               :: Maybe String
   }


-- | Returns all image information about an image file including
--   1. md5 checksum
--   2. size in bytes
--   3. exif
getImageInfo :: FilePath -> IO ImageInfo
getImageInfo fp = do
   size                      <- liftM fileSize $ getFileStatus fp
   digest                    <- hashFile fp
   let hash = S8.unpack $ toHex . encode $ (digest :: MD5Digest)
   exif                      <- Exif.fromFile fp
   _make                     <- Exif.getTag exif "Make"
   _model                    <- Exif.getTag exif "Model"
   _xResolution              <- (liftM . liftM) read $ Exif.getTag exif "XResolution"
   _yResolution              <- (liftM . liftM) read $ Exif.getTag exif "YResolution"
   _resolutionUnit           <- Exif.getTag exif "ResolutionUnit"
   _dateTime                 <- (liftM . liftM) read $ Exif.getTag exif "DateTime"
   _compression              <- Exif.getTag exif "Compression"
   _exposureTime             <- (liftM . liftM) read $ Exif.getTag exif "ExposureTime"
   _fNumber                  <- (liftM . liftM) read $ Exif.getTag exif "FNumber"
   _exposureProgram          <- Exif.getTag exif "ExposureProgram"
   _iSOSpeedRatings          <- (liftM . liftM) read $ Exif.getTag exif "ISOSpeedRatings"
   _exifVersion              <- Exif.getTag exif "ExifVersion"
   _dateTimeOriginal         <- (liftM . liftM) read $ Exif.getTag exif "DateTimeOriginal"
   _dateTimeDigitized        <- (liftM . liftM) read $ Exif.getTag exif "DateTimeDigitized"
   _exposureBiasValue        <- (liftM . liftM) read $ Exif.getTag exif "ExposureBiasValue"
   _subjectDistance          <- (liftM . liftM) read $ Exif.getTag exif "SubjectDistance"
   _meteringMode             <- Exif.getTag exif "MeteringMode"
   _flash                    <- Exif.getTag exif "Flash"
   _focalLength              <- (liftM . liftM) read $ Exif.getTag exif "FocalLength"
   _subSecTimeOriginal       <- (liftM . liftM) read $ Exif.getTag exif "SubSecTimeOriginal"
   _subSecTimeDigitized      <- (liftM . liftM) read $ Exif.getTag exif "SubSecTimeDigitized"
   _focalPlaneXResolution    <- (liftM . liftM) read $ Exif.getTag exif "FocalPlaneXResolution"
   _focalPlaneYResolution    <- (liftM . liftM) read $ Exif.getTag exif "FocalPlaneYResolution"
   _focalPlaneResolutionUnit <- Exif.getTag exif "FocalPlaneResolutionUnit"
   _customRendered           <- Exif.getTag exif "CustomRendered"
   _exposureMode             <- Exif.getTag exif "ExposureMode"
   _whiteBalance             <- Exif.getTag exif "WhiteBalance"
   _sceneCaptureType         <- Exif.getTag exif "SceneCaptureType"
   _flashPixVersion          <- Exif.getTag exif "FlashPixVersion"
   _colorSpace               <- Exif.getTag exif "ColorSpace"
   return ImageInfo
      { md5Hash                  = hash
      , byteSize                 = fromIntegral size
      , make                     = _make                    
      , model                    = _model                   
      , xResolution              = _xResolution             
      , yResolution              = _yResolution             
      , resolutionUnit           = _resolutionUnit          
      , dateTime                 = _dateTime                
      , compression              = _compression             
      , exposureTime             = _exposureTime            
      , fNumber                  = _fNumber                 
      , exposureProgram          = _exposureProgram         
      , iSOSpeedRatings          = _iSOSpeedRatings         
      , exifVersion              = _exifVersion             
      , dateTimeOriginal         = _dateTimeOriginal        
      , dateTimeDigitized        = _dateTimeDigitized       
      , exposureBiasValue        = _exposureBiasValue       
      , subjectDistance          = _subjectDistance         
      , meteringMode             = _meteringMode            
      , flash                    = _flash                   
      , focalLength              = _focalLength             
      , subSecTimeOriginal       = _subSecTimeOriginal      
      , subSecTimeDigitized      = _subSecTimeDigitized     
      , focalPlaneXResolution    = _focalPlaneXResolution   
      , focalPlaneYResolution    = _focalPlaneYResolution   
      , focalPlaneResolutionUnit = _focalPlaneResolutionUnit
      , customRendered           = _customRendered          
      , exposureMode             = _exposureMode            
      , whiteBalance             = _whiteBalance            
      , sceneCaptureType         = _sceneCaptureType        
      , flashPixVersion          = _flashPixVersion         
      , colorSpace               = _colorSpace              
      }


