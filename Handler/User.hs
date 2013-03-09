module Handler.User
   ( getUsersR
   , postUsersR
   , getUserR
   , getEditUserR
   , postDeleteUserR
   )
where

import Import
import Control.Monad (liftM)
import Yesod.Auth.HashDB (setPassword)
import Yesod.Form.Validations


entryForm :: Maybe User -> Form User
entryForm muser =
   let
      validNameField     = lengthInRange 5 10 textField
      validPasswordField = lengthAtLeast 8 $ goodPassword passwordField 
   in
      renderDivs $ User
        <$> areq validNameField     "Username" (liftM userName muser)
        <*> areq validPasswordField "Password" Nothing
        <*> pure "salt"
        <*> areq boolField          "Is admin" (liftM userAdmin muser)



getUsersR :: Handler RepHtml
getUsersR = do
    users <- runDB $ selectList [] [Desc UserName]
    Just currentUser <- currentUserM
    (userWidget, enctype) <- generateFormPost $ entryForm Nothing
    defaultLayout $(widgetFile "users") 


postUsersR :: Handler RepHtml
postUsersR = do
    ((res, userWidget), enctype) <- runFormPost $ entryForm Nothing
    case res of 
        FormSuccess user -> do
            user' <- setPassword (userHash user) user
            userId <- runDB $ insert user'
            setMessage $ toHtml $ userName user <> " created"
            redirect $ UserR userId 
        _ -> defaultLayout $ do
            setTitle "Please correct your entry form"
            $(widgetFile "userAddError") 


getUserR :: UserId -> Handler RepHtml
getUserR userId = do 
   user <- runDB $ get404 userId
   defaultLayout $ do
      setTitle $ toHtml $ userName user
      $(widgetFile "user")


getEditUserR :: UserId -> Handler RepHtml
getEditUserR userId = do
   user <- runDB $ get404 userId
   (userWidget, enctype) <- generateFormPost $ entryForm $ Just user
   defaultLayout $(widgetFile "editUser")


postDeleteUserR :: UserId -> Handler RepHtml
postDeleteUserR userId = do
   user <- runDB $ get404 userId
   setMessage $ toHtml $ userName user <> " deleted"
   runDB $ delete userId
   redirect UsersR
