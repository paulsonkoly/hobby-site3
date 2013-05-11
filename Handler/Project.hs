{- |
Module      :  $Header$
Description :  Handlers for project related pages
Copyright   :  (c) Paul Sonkoly
License     :  AllRightsReserved

Maintainer  :  sonkoly.pal@gmail.com
Stability   :  stable
Portability :  portable
-}

module Handler.Project
   ( -- * Handlers
     getProjectsR
   , postProjectsR
   , getEditProjectR
   , postEditProjectR
   , postDeleteProjectR
   )
   where

import Import
import Data.Text hiding (map, toLower)
import Data.Char


-- | Project list & new project form for logged in users
getProjectsR :: Handler RepHtml
getProjectsR = do
   projects <- runDB $ selectList [] []
   let
      escapeName = map ((\c -> if isLower c then c else '_') . toLower)
      getId f = toHtml . pack . f . escapeName . unpack . projectName
      prid = getId id 
      hprid = getId ('#' :)
   defaultLayout  $ setTitle "Projects" >> $(widgetFile "projects")


-- | Create new project
postProjectsR :: Handler RepHtml
postProjectsR = undefined


-- | Edit project form
getEditProjectR :: ProjectId -> Handler RepHtml
getEditProjectR = undefined


-- | Save project edit
postEditProjectR :: ProjectId -> Handler RepHtml
postEditProjectR = undefined


-- | Delete a project
postDeleteProjectR :: ProjectId -> Handler RepHtml
postDeleteProjectR = undefined
