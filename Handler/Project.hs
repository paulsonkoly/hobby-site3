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


-- | Project list & new project form for logged in users
getProjectsR :: Handler RepHtml
getProjectsR = undefined


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
