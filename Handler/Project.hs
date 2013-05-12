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
   , getManageProjectsR
   , postProjectsR
   , getEditProjectR
   , postEditProjectR
   , postDeleteProjectR
   )
   where

import Import
import Yesod.Form.Nic
import Data.Text hiding (map, toLower)
import Data.Char


-- | Project list
getProjectsR :: Handler RepHtml
getProjectsR = do
   projects <- runDB $ selectList [] []
   let
      escapeName = map ((\c -> if isLower c then c else '_') . toLower)
      getId f = toHtml . pack . f . escapeName . unpack . projectName
      prid = getId id
      hprid = getId ('#' :)
   defaultLayout $ setTitle "Projects" >> $(widgetFile "projects")


-- | Adds sensible size to textareas in a widget
--
-- Without this the nicHtml looks awful
sensibleTextarea :: Widget
sensibleTextarea = toWidget [lucius| textarea { width: 100%; height: 200px; } |]


-- | Form for a project
projectForm :: (RenderMessage master FormMessage, YesodNic master)
   => Maybe Project   -- ^ gallery for default values
   -> Html -> MForm sub master (FormResult Project, GWidget sub master ())
projectForm mProject = renderBootstrap $ Project
   <$> areq textField "Name"              (projectName <$> mProject)
   <*> areq textField "Short description" (projectShortDescription <$> mProject)
   <*> aopt nicHtmlField "Description"    (mProject >>= Just . projectDescription)
   <*> areq textField "Clone path"        (projectClonePath <$> mProject)


getManageProjectsR' :: Maybe (Widget, Enctype) -> Handler RepHtml
getManageProjectsR' mForm = do
   projects <- runDB $ selectList [] []
   (projectWidget, enctype) <- maybe (generateFormPost $ projectForm Nothing) return mForm
   let destination = ProjectsR
   defaultLayout $ do
      setTitle "Manage projects"
      sensibleTextarea
      $(widgetFile "manageProjects")


-- | Project manager page
getManageProjectsR :: Handler RepHtml
getManageProjectsR = getManageProjectsR' Nothing


-- | Create new project
postProjectsR :: Handler RepHtml
postProjectsR = do
   ((res, projectWidget), enctype) <- runFormPost $ projectForm Nothing
   case res of
      FormSuccess project -> do
         _ <- runDB $ insert project
         setMessage $ toHtml $ projectName project <> " created"
         redirect ManageProjectsR
      _ -> getManageProjectsR' $ Just (projectWidget, enctype) 


getEditProjectR' :: Maybe (Widget, Enctype) -> ProjectId -> Handler RepHtml
getEditProjectR' mForm projectId = do
   project <- runDB $ get404 projectId
   (projectWidget, enctype) <- maybe (generateFormPost $ projectForm $ Just project) return mForm
   let destination = EditProjectR projectId
   defaultLayout $ do
      setTitle $ toHtml $ "Edit " <> projectName project
      sensibleTextarea
      $(widgetFile "editProject")


-- | Edit project form
getEditProjectR :: ProjectId -> Handler RepHtml
getEditProjectR = getEditProjectR' Nothing


-- | Save project edit
postEditProjectR :: ProjectId -> Handler RepHtml
postEditProjectR projectId = do
   ((res, projectWidget), enctype) <- runFormPost $ projectForm Nothing
   case res of 
      FormSuccess project -> do
         runDB $ update projectId
            [ ProjectName             =. projectName project
            , ProjectShortDescription =. projectShortDescription project
            , ProjectDescription      =. projectDescription project
            , ProjectClonePath        =. projectClonePath project
            ]
         setMessage $ toHtml (projectName project <> " has successfully been updated.")
         redirect ManageProjectsR
      _ -> getEditProjectR' (Just (projectWidget, enctype)) projectId


-- | Delete a project
postDeleteProjectR :: ProjectId -> Handler RepHtml
postDeleteProjectR projectId = do
   project <- runDB $ get404 projectId
   setMessage $ toHtml $ projectName project <> " deleted"
   runDB $ delete projectId
   redirect ManageProjectsR
