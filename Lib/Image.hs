{-# LANGUAGE  DeriveDataTypeable #-}
{- |
Module      :  $Header$
Description :  Functions related to IO during image upload
Copyright   :  (c) Paul Sonkoly
License     :  AllRightsReserved

Maintainer  :  sonkoly.pal@gmail.com
Stability   :  stable
Portability :  portable
-}

module Lib.Image
   (
   -- * Functions
     mkImage
   , deleteImage
   , imageFilePath
   )
where

import Import

import Numeric (showHex)
import Control.Monad
import Control.Exception

import System.IO
import System.Posix.Files
import System.Random
import System.Locale

import qualified Data.Text             as T
import qualified Data.ByteString       as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.Time.Format      as TF

import Data.Maybe
import Data.List
import Data.Word            (Word8)
import Data.Serialize       (encode)
import Data.Typeable
import Data.Conduit
import Data.Conduit.Binary
import Crypto.Conduit
import Data.Digest.Pure.MD5 (MD5Digest)
import Data.Time.Clock

import qualified Graphics.Exif as Exif
import qualified Graphics.GD   as GD

import Lib.ImageType

-----------------------------------------------------------------------
-- some constants
-----------------------------------------------------------------------
dirName :: FilePath
dirName = "images"

thumbSize   :: Int
thumbSize   = 100
largeSize   :: Int
largeSize   = 670
jpegQuality :: Int
jpegQuality = 95


imageFilePath :: ImageType -> String -> FilePath
imageFilePath i s = dirName ++ "/" ++ s ++ "_" ++ show i ++ ".jpeg"


toHex :: S.ByteString -> S.ByteString 
toHex = S.concatMap word8ToHex
   where
      word8ToHex :: Word8 -> S.ByteString
      word8ToHex w = S8.pack $ pad $ showHex w []

      -- We know that the input will always be 1 or 2 characters long.
      pad :: String -> String
      pad [x] = ['0', x]
      pad s   = s


-- | temporary file for sink.
sinkToTemp :: FileInfo -> IO FilePath
sinkToTemp f = do
   -- as it turns out openBinaryTempFile can generate the same
   -- file name from multiple threads. That's why I just stick
   -- a random bit at the front.
   rand <- getStdRandom random
   (temp, thandle) <- openBinaryTempFile "." $ "image_temp" ++ show (rand :: Int)
   runResourceT $ fileSource f $$ sinkHandle thandle
   hClose thandle
   return temp


-- | compute the MD5 hash
md5Hash :: FilePath -> IO String
md5Hash fn = do
   digest <- hashFile fn
   return $ S8.unpack $ toHex . encode $ (digest :: MD5Digest)


-- | Transforms (xsize, ysize) pair into a box fiting (max, max) retaining aspect ratio
retainAspectRatio :: Int        -- ^ max value for bounding box
                  -> (Int, Int) -- ^ (xsize, ysize)
                  -> (Int, Int)
retainAspectRatio r (x, y)
   | x <= y    = (x * r `div` y, r)
   | otherwise = (r, y * r `div` x)


-- | safe reads something wrapped in Maybe from 'Read' type class
--
-- safe reading means returning 'Nothing' instead of throwing exception
safeRead :: (Read a) => Maybe String -> Maybe a
safeRead (Just s) = fmap fst $ listToMaybe $ reads s
safeRead Nothing  = Nothing


-- | safe reads an exposure time from the form of "1/200 sec"
safeReadET :: Maybe String -> Maybe Double
safeReadET (Just s) = let first = listToMaybe $ reads s
                          second (Just p)       = fmap fst $ listToMaybe $ reads $ tail $ snd p
                          second Nothing        = Nothing
                          rat (Just a) (Just b) = Just $ a / b 
                          rat _ _               = Nothing
                      in rat (fmap fst first) (second first)
safeReadET Nothing  = Nothing


-- | safe reads an f value from the form of "f/0.3"
safeReadF :: Maybe String -> Maybe Double
safeReadF (Just s)
   | length s > 2 = fmap fst $ listToMaybe $ reads $ tail $ tail s 
   | otherwise    = Nothing
safeReadF Nothing = Nothing



-- | safe reads time
safeReadT :: Maybe String -> Maybe UTCTime
-- it reads out from exif in this weird format, and I honestly don't know why.
-- also don't know why defaultTimeLocale is needed here.
safeReadT (Just s) = TF.parseTime defaultTimeLocale "%Y:%m:%d %T" s 
safeReadT Nothing  = Nothing


-- | creates an Image from Exif
getExif :: FilePath -> IO Image
getExif original = do
   exif                      <- Exif.fromFile original
   make                      <- Exif.getTag exif "Make"
   model                     <- Exif.getTag exif "Model"
   xResolution               <- Exif.getTag exif "XResolution"
   yResolution               <- Exif.getTag exif "YResolution"
   resolutionUnit            <- Exif.getTag exif "ResolutionUnit"
   dateTime                  <- Exif.getTag exif "DateTime"
   compression               <- Exif.getTag exif "Compression"
   exposureTime              <- Exif.getTag exif "ExposureTime"
   fNumber                   <- Exif.getTag exif "FNumber"
   exposureProgram           <- Exif.getTag exif "ExposureProgram"
   isoSpeedRatings           <- Exif.getTag exif "ISOSpeedRatings"
   exifVersion               <- Exif.getTag exif "ExifVersion"
   dateTimeOriginal          <- Exif.getTag exif "DateTimeOriginal"
   dateTimeDigitized         <- Exif.getTag exif "DateTimeDigitized"
   exposureBiasValue         <- Exif.getTag exif "ExposureBiasValue"
   subjectDistance           <- Exif.getTag exif "SubjectDistance"
   meteringMode              <- Exif.getTag exif "MeteringMode"
   flash                     <- Exif.getTag exif "Flash"
   focalLength               <- Exif.getTag exif "FocalLength"
   subSecTimeOriginal        <- Exif.getTag exif "SubSecTimeOriginal"
   subSecTimeDigitized       <- Exif.getTag exif "SubSecTimeDigitized"
   focalPlaneXResolution     <- Exif.getTag exif "FocalPlaneXResolution"
   focalPlaneYResolution     <- Exif.getTag exif "FocalPlaneYResolution"
   focalPlaneResolutionUnit  <- Exif.getTag exif "FocalPlaneResolutionUnit"
   customRendered            <- Exif.getTag exif "CustomRendered"
   exposureMode              <- Exif.getTag exif "ExposureMode"
   whiteBalance              <- Exif.getTag exif "WhiteBalance"
   sceneCaptureType          <- Exif.getTag exif "SceneCaptureType"
   flashPixVersion           <- Exif.getTag exif "FlashPixVersion"
   colorSpace                <- Exif.getTag exif "ColorSpace"

   return Image
      { imageUserId                   = undefined
      , imageAccessibility            = undefined
      , imageMd5Hash                  = undefined
      , imageOrigName                 = undefined
      , imageMake                     = make
      , imageModel                    = model
      , imageXResolution              = safeRead xResolution
      , imageYResolution              = safeRead yResolution
      , imageResolutionUnit           = resolutionUnit
      , imageDateTime                 = safeReadT dateTime
      , imageCompression              = compression
      , imageExposureTime             = safeReadET exposureTime
      , imageFNumber                  = safeReadF fNumber
      , imageExposureProgram          = exposureProgram
      , imageISOSpeedRatings          = safeRead isoSpeedRatings
      , imageExifVersion              = exifVersion
      , imageDateTimeOriginal         = safeReadT dateTimeOriginal
      , imageDateTimeDigitized        = safeReadT dateTimeDigitized
      , imageExposureBiasValue        = safeRead exposureBiasValue
      , imageSubjectDistance          = safeRead subjectDistance
      , imageMeteringMode             = meteringMode
      , imageFlash                    = flash
      , imageFocalLength              = safeRead focalLength
      , imageSubSecTimeOriginal       = safeRead subSecTimeOriginal
      , imageSubSecTimeDigitized      = safeRead subSecTimeDigitized
      , imageFocalPlaneXResolution    = safeRead focalPlaneXResolution
      , imageFocalPlaneYResolution    = safeRead focalPlaneYResolution
      , imageFocalPlaneResolutionUnit = focalPlaneResolutionUnit
      , imageCustomRendered           = customRendered
      , imageExposureMode             = exposureMode
      , imageWhiteBalance             = whiteBalance
      , imageSceneCaptureType         = sceneCaptureType
      , imageFlashPixVersion          = flashPixVersion
      , imageColorSpace               = colorSpace
      }


data ImageException = MD5Exception
   deriving (Typeable)

instance Show ImageException where
   show MD5Exception = "MD5 checksum clashes or the image already exist on server."

instance Exception ImageException

-- | removes a file if it exists, otherwise does nothing
removeIfExist :: FilePath -> IO ()
removeIfExist fp = fileExist fp >>= flip when (removeLink fp)


-- | generates the derivative images
generateDerivatives :: FilePath -> String -> IO ()
generateDerivatives fn hash = do
   original <- GD.loadJpegFile fn
   size <- GD.imageSize original
   forM_ [ ( Thumbnail, thumbSize ) , ( Large, largeSize ) ] (\ (t, s) -> do
      modJpeg <- uncurry GD.resizeImage (retainAspectRatio s size) original
      GD.saveJpegFile jpegQuality (imageFilePath t hash) modJpeg)



-- | Saves the image from the Yesod file info to the hard drive and generates derivative
--   images.
--
-- Returns the pair of the image size and the Image, or SomeException if an exception was
-- raised. Image has all Exif related fields and the hash and origName fields filled in.
mkImage :: FileInfo -- ^ The Yesod file info from the uploaded file 
         -> IO (Either SomeException (Int, Image))
mkImage f = try (do
   temp <- sinkToTemp f
   (do
      hash <- md5Hash temp
      size <- liftM (fromIntegral . fileSize) $ getFileStatus temp
      let original = imageFilePath Original hash
      fileExist original >>= flip when (throwIO MD5Exception)
      rename temp original
      generateDerivatives original hash
      image <- getExif original
      return (size, image { imageMd5Hash = T.pack hash, imageOrigName = fileName f })
      ) `finally` (removeIfExist temp)
   )


-- | Deletes all forms of the image from the hard drive.
deleteImage :: String -- ^ The md5 checksum
            -> IO ()
deleteImage hash = mapM_ (removeIfExist . (`imageFilePath` hash)) [Original, Thumbnail, Large]

