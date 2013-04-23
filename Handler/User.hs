{- |
Module      :  $Header$
Description :  User related handlers
Copyright   :  (c) Paul Sonkoly
License     :  AllRightsReserved

Maintainer  :  sonkoly.pal@gmail.com
Stability   :  stable
Portability :  portable
-}


module Handler.User
   ( -- * Handlers
     getUsersR
   , postUsersR
   , getEditUserR
   , postEditUserR
   , postDeleteUserR
   )
where

import Import
import Yesod.Auth
import Yesod.Auth.HashDB (setPassword)
import Yesod.Form.Validations


-- | data representing what can be edited on a User by an admin
data AdminEditableUser passwdType = AdminEditableUser
   { name     :: Text
   , password :: passwdType
   , admin    :: Bool
   }


-- | data representing what can be edited on a User by itself
data SelfEditableUser = SelfEditableUser
   { name'     :: Text
   , password' :: Maybe Text
   }


type NewUserByAdmin = AdminEditableUser Text
type OldUserByAdmin = AdminEditableUser (Maybe Text)
type OldUserBySelf  = SelfEditableUser


validNameField, validPasswordField
   :: RenderMessage master FormMessage
   => Field sub master Text
validNameField     = lengthInRange 5 10 textField
validPasswordField = lengthAtLeast 8 $ goodPassword passwordField 


-- | the form of a new user, no default values, password required
newUserAdminForm :: Form NewUserByAdmin
newUserAdminForm = renderBootstrap $ AdminEditableUser
   <$> areq validNameField     "Username" Nothing
   <*> areq validPasswordField "Password" Nothing
   <*> areq boolField          "Is admin" Nothing


-- | default values from User, password is optional
oldUserAdminForm
   :: User   -- ^ the default values on the form
   -> Form OldUserByAdmin
oldUserAdminForm user = renderBootstrap $ AdminEditableUser
   <$> areq validNameField     "Username" (Just $ userName user)
   <*> aopt validPasswordField "Password" Nothing
   <*> areq boolField          "Is admin" (Just $ userAdmin user)


-- | default values from User, password is optional, cannot toggle adminness
oldUserSelfForm
   :: User
   -> Form OldUserBySelf
oldUserSelfForm user = renderBootstrap $ SelfEditableUser
   <$> areq validNameField     "Username" (Just $ userName user)
   <*> aopt validPasswordField "Password" Nothing


usersLayout :: [Entity User] -> (Widget, Enctype) -> GHandler App App RepHtml
usersLayout users (userWidget, enctype) =
    defaultLayout $ setTitle "Users" >> $(widgetFile "users") 


-- | users admin page
getUsersR :: Handler RepHtml
getUsersR = do
   users <- runDB $ selectList [] [ Desc UserName ]
   generateFormPost newUserAdminForm >>= usersLayout users


-- | user creation post request
postUsersR :: Handler RepHtml
postUsersR = do
   ((res, userWidget), enctype) <- runFormPost newUserAdminForm
   case res of
      FormSuccess editable -> do
         user <- setPassword (password editable) User
            { userName  = name editable
            , userAdmin = admin editable
            , userHash  = ""
            , userSalt  = ""
            }
         _ <- runDB $ insert user
         setMessage $ toHtml $ userName user <> " created"
         redirect UsersR
      _ -> do
         users <- runDB $ selectList [] [ Desc UserName ]
         usersLayout users (userWidget, enctype)


editLayout :: User -> (Widget, Enctype) -> GHandler App App RepHtml
editLayout user (userWidget, enctype) = defaultLayout $ do
   setTitle $ toHtml $ "Edit " <> userName user
   $(widgetFile "editUser")


-- | user edit form
getEditUserR :: UserId -> Handler RepHtml
getEditUserR userId = do
   user <- runDB $ get404 userId
   Just (Entity _ currentUser) <- maybeAuth
   if userAdmin currentUser
      then generateFormPost (oldUserAdminForm user) >>= editLayout user
      else generateFormPost (oldUserSelfForm user)  >>= editLayout user


postEditUserR :: UserId -> Handler RepHtml
postEditUserR userId = 
   let
      onSuccess user mPasswd name'' admin'' whereTo = do
         user' <- case mPasswd of
            Just passwd -> setPassword passwd user
            Nothing     -> return user
         runDB $ update userId
            [ UserName  =. name''
            , UserAdmin =. admin''
            , UserHash  =. userHash user'
            , UserSalt  =. userSalt user'
            ]
         setMessage $ toHtml $ name'' <> " updated"
         redirect whereTo
   in do
      user <- runDB $ get404 userId
      Just (Entity _ currentUser) <- maybeAuth
      if userAdmin currentUser 
         then do
            ((res, userWidget), enctype) <- runFormPost $ oldUserAdminForm user
            case res of
                  FormSuccess editable ->
                     onSuccess user (password editable) (name editable) (admin editable) UsersR
                  _ -> editLayout user (userWidget, enctype)
         else do
            ((res, userWidget), enctype) <- runFormPost $ oldUserSelfForm user
            case res of
                  FormSuccess editable ->
                     onSuccess user (password' editable) (name' editable) (userAdmin user) GalleriesR
                  _ -> editLayout user (userWidget, enctype)


-- | user deletion post request
postDeleteUserR :: UserId -> Handler RepHtml
postDeleteUserR userId = do
   user <- runDB $ get404 userId
   setMessage $ toHtml $ userName user <> " deleted"
   runDB $ delete userId
   redirect UsersR
