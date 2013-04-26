{- |
Module      :  $Header$
Description :  Additional ToMarkup instances
Copyright   :  (c) Paul Sonkoly
License     :  AllRightsReserved

Maintainer  :  sonkoly.pal@gmail.com
Stability   :  stable
Portability :  portable

Some data types Ie @UTCTime@ need to be rendered on the web site. This
module provides the required @ToMarkup@ instances.
-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lib.ToMarkup () where

import Prelude
import Text.Blaze.Html
import Data.Time.Clock
import Data.Time.Format
import Data.Int
import System.Locale


instance ToMarkup UTCTime where
   toMarkup = toMarkup . formatTime defaultTimeLocale "%d/%m %Y %T"


instance ToMarkup Int64 where
   toMarkup = toMarkup . show
