-----------------------------------------------------------------------
-- | Different image file types.
--
-- This module provides a PathPiece instance where we can insert the
-- image type in the path of the image request in a type safe way.
-----------------------------------------------------------------------

module Lib.ImageType (ImageType (..)) where

import Prelude
import Text.ParserCombinators.ReadPrec
import Text.Read
import Data.Text
import Web.PathPieces

-----------------------------------------------------------------------
-- | data type representing different formats saved on the disk of the
--   same image 
-----------------------------------------------------------------------
data ImageType
   = Thumbnail -- ^ type for the thumbnail size image variant
   | Large     -- ^ type for the large size image variant
   | Original  -- ^ type for the original (not modified) image
   deriving Eq


instance Show ImageType where
   show Thumbnail = "thumbnail"
   show Large     = "large"
   show Original  = "original"


instance Read ImageType where
   readPrec = parens
       ( do Ident s <- lexP
            case s of
              "thumbnail" -> return Thumbnail
              "large"     -> return Large
              "original"  -> return Original
              _           -> pfail
       )


instance PathPiece ImageType where
   toPathPiece   i = pack $ show i
   fromPathPiece "thumbnail" = Just Thumbnail
   fromPathPiece "large"     = Just Large
   fromPathPiece "original"  = Just Original
   fromPathPiece _           = Nothing
