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
   ) where

import Prelude
import Database.Persist
import Database.Persist.TH


data Accessibility
   = Public  -- ^ the resource is accessible by everybody
   | Member  -- ^ the resource is accessible by logged in users
   | Owner   -- ^ the resource is accessible by it's owner and admin users
   deriving (Show, Read, Enum, Bounded, Eq)
derivePersistField "Accessibility"

