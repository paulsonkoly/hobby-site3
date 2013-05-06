{- |
Module      :  $Header$
Description :  Database field data type for resource accessibility
Copyright   :  (c) Paul Sonkoly
License     :  AllRightsReserved

Maintainer  :  sonkoly.pal@gmail.com
Stability   :  stable
Portability :  portable

The module provides a data type for resource accessibility. The field
is stored in the database for each resource, on request the authorisation
method can consider the accessibility value. Note that we don't control
accessibility here. Also note that the resource has to be read from the
database, even if the result will be access denied.
-}

module Lib.Accessibility
   ( Accessibility(..)
   , accessibilityWidget
   ) where

import Prelude
import Control.Applicative
import Data.Maybe
import Database.Persist
import Database.Persist.TH
import Data.Text

import Yesod.Core
import Yesod.Form.Fields
import Yesod.Form.Types

data Accessibility
   = Public  -- ^ the resource is accessible by everybody
   | Member  -- ^ the resource is accessible by logged in users
   | Owner   -- ^ the resource is accessible by it's owner and admin users
   deriving (Show, Read, Enum, Bounded, Eq)
derivePersistField "Accessibility"


instance PathPiece Accessibility where
   toPathPiece      = pack . show
   fromPathPiece pp = fst <$> (listToMaybe $ reads $ unpack pp)


-- | Stand alone select field ( no @Form@ ) for @Accessibility@
accessibilityWidget :: RenderMessage master FormMessage => GWidget sub master ()
accessibilityWidget = fieldView (selectField opts) "accessibility" "Accessibility" [] (Left "") True
   where
      opts :: GHandler sub master (OptionList Accessibility)
      opts = optionsEnum
