{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}

module Lib.FormValidation
   ( LengthCriteria(..)
   , lengthValidation
   , lengthInRange
   , goodPassword
   ) where

import Prelude
import Text.Shakespeare.Text
import Data.Text as T (Text, length, any)
import Data.Char
import Yesod.Form.Functions
import Yesod.Form.Types

instance ToText (Int) where
   toText = toText . show

data LengthCriteria = LessThan | AtLeast | Exactly | MoreThan | AtMost

instance ToText LengthCriteria where
   toText LessThan = "less than"
   toText AtLeast  = "at least"
   toText Exactly  = "exactly"
   toText MoreThan = "more than"
   toText AtMost   = "at most"

type CriteriaFunc a = a -> a -> Bool

toFunc :: (Ord a) => LengthCriteria -> CriteriaFunc a 
toFunc LessThan = (<)
toFunc AtLeast  = (>=)
toFunc Exactly  = (==)
toFunc MoreThan = (>)
toFunc AtMost   = (<=)


-- | Validates the lenght of a textual field
lengthValidation
   :: LengthCriteria -- ^ comparison function
   -> Int            -- ^ limit
   -> Field sub master Text
   -> Field sub master Text
lengthValidation f n = checkBool (\a -> toFunc f (T.length a) n) errorMsg
   where
      errorMsg :: Text
      errorMsg = [st|Field should contain #{f} #{n} characters|]


-- | Validates that the length of a textual field is in range
lengthInRange 
    :: Int -- ^ Lower bound
    -> Int -- ^ Upper bound
    -> Field sub master Text
    -> Field sub master Text 
lengthInRange n m = lengthValidation AtLeast n . lengthValidation AtMost m


-- | Validates that a textual field can be good password
--
-- The check doesn't look up any dictionaries or check the length. It simply ensures that
-- the password has at least one lower case, one upper case and one numeric character.
goodPassword :: Field sub master Text -> Field sub master Text
goodPassword = checkBool (T.any isLower)  errorNeedsLower
             . checkBool (T.any isNumber) errorNeedsNumber
             . checkBool (T.any isUpper)  errorNeedsUpper
   where
      errorNeedsLower  = "Needs lower case character" :: Text
      errorNeedsUpper  = "Needs upper case character" :: Text
      errorNeedsNumber = "Needs numeric character"    :: Text
