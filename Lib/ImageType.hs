{- |
Module      :  $Header$
Description :  Different image file types.
Copyright   :  (c) Paul Sonkoly
License     :  AllRightsReserved

Maintainer  :  sonkoly.pal@gmail.com
Stability   :  stable
Portability :  portable

This module provides a PathPiece instance where we can insert the
image type in the path of the image request in a type safe way.
-}


module Lib.ImageType (ImageType (..)) where

import Prelude
import Control.Applicative
import Data.Text
import Data.Maybe
import Web.PathPieces

-- | data type representing different formats saved on the disk of the
--   same image 
data ImageType
   = Thumbnail -- ^ type for the thumbnail size image variant
   | Large     -- ^ type for the large size image variant
   | Original  -- ^ type for the original (not modified) image
   deriving (Show, Read, Eq)


instance PathPiece ImageType where
   toPathPiece      = pack . show
   fromPathPiece pp = fst <$> (listToMaybe $ reads $ unpack pp)
