module Foundation where

import Prelude
import Yesod
import Yesod.Static
import Yesod.Auth
import Yesod.Auth.HashDB (authHashDB, getAuthIdHashDB)
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Yesod.Form.Nic (YesodNic)
import Network.HTTP.Conduit (Manager)
import qualified Settings
import Settings.Development (development)
import qualified Database.Persist.Store
import Settings.StaticFiles
import Database.Persist.GenericSql
import Settings (widgetFile, Extra (..))
import Model
import Text.Jasmine (minifym)
import Web.ClientSession (getKey)
import Text.Hamlet (hamletFile)
import System.Log.FastLogger (Logger)

import Lib.ImageType
import Lib.Accessibility

import Control.Monad

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { settings :: AppConfig DefaultEnv Extra
    , getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Database.Persist.Store.PersistConfigPool Settings.PersistConfig -- ^ Database connection pool.
    , httpManager :: Manager
    , persistConfig :: Settings.PersistConfig
    , appLogger :: Logger
    }

-- Set up i18n messages. See the message folder.
mkMessage "App" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/handler
--
-- This function does three things:
--
-- * Creates the route datatype AppRoute. Every valid URL in your
--   application can be represented as a value of this type.
-- * Creates the associated type:
--       type instance Route App = AppRoute
-- * Creates the value resourcesApp which contains information on the
--   resources declared below. This is used in Handler.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- App. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the AppRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
mkYesodData "App" $(parseRoutesFile "config/routes")

instance YesodNic App

type Form x = Html -> MForm App App (FormResult x, Widget)

-- | the currently logged in user, if any
maybeUser :: GHandler s App (Maybe (Entity User))
maybeUser = do
   maid <- maybeAuthId
   case maid of
      Just aid -> liftM (fmap (Entity aid)) $ runDB (get aid)
      Nothing  -> return Nothing


-- | Authourization for pages that require logged in user wrapped in GHandler
isLoggedIn :: GHandler s App AuthResult
isLoggedIn = let
   toAuthorization (Just _) = Authorized
   toAuthorization Nothing  = AuthenticationRequired
   in liftM toAuthorization maybeUser


-- | Authorization for pages that require admin wrapped in GHandler
isAdmin :: GHandler s App AuthResult
isAdmin = let
   toAuthorization (Just (Entity _ user)) = if userAdmin user
      then Authorized
      else Unauthorized "You don't have the authorization to access the resource"  
   toAuthorization Nothing = AuthenticationRequired
   in liftM toAuthorization maybeUser


type Ownership t = (Maybe UserId, Entity t)


-- | Type class for resource ownership.
--
-- mininal complete definition : getOwner, canRead
class (Owned t) where
   getOwner :: Entity t -> UserId
   -- | define read access rules for a resource
   canRead :: Ownership t -> GHandler s App AuthResult

   -- | returns the Ownership in the request that needs to be verified
   getOwnership ::
      ( PersistEntityBackend t ~ PersistMonadBackend (SqlPersist (GHandler s App))
      , PersistEntity t
      ) => Key t -> GHandler s App (Ownership t)
   getOwnership tid = do
      muserId <- maybeAuthId
      t       <- runDB $ get404 tid
      return (muserId, Entity tid t)

   -- | returns Authorized for resource owners (and admins)
   isOwner :: Ownership t -> GHandler s App AuthResult
   isOwner (muserId, e) = if muserId == Just (getOwner e) then return Authorized else isAdmin


instance Owned User where -- users own themselves, and can only read themselves
   getOwner = entityKey
   canRead = isOwner


instance Owned Image where
   getOwner = imageUserId . entityVal
   canRead (muserId, Entity _ image) =
      case imageAccessibility image of
         Public -> return Authorized
         Member -> isLoggedIn
         Owner  -> if muserId == Just (imageUserId image)
            then return Authorized
            else isAdmin


instance Owned Gallery where
   getOwner = galleryUserId . entityVal
   canRead _ = return Authorized


-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    approot = ApprootMaster $ appRoot . settings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = do
        key <- getKey "config/client_session_key.aes"
        let timeout = 120 * 60 -- 120 minutes
        (getCachedDate, _closeDateCache) <- clientSessionDateCacher timeout
        return . Just $ clientSessionBackend2 key getCachedDate

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            $(widgetFile "normalize")
            addStylesheet $ StaticR css_bootstrap_css
            $(widgetFile "default-layout")
        hamletToRepHtml $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    isAuthorized HomeR       False = return Authorized
    isAuthorized (AuthR _)   _     = return Authorized
    isAuthorized (StaticR _) False = return Authorized
    isAuthorized FaviconR    False = return Authorized
    isAuthorized RobotsR     False = return Authorized

    -- route name, then a boolean indicating if it's a write request
    isAuthorized UsersR                      _ = isAdmin
    isAuthorized (UserR uid)                 _ = getOwnership uid >>= canRead
    isAuthorized (EditUserR uid)             _ = getOwnership uid >>= isOwner
    isAuthorized (DeleteUserR _)             _ = isAdmin
                                             
    isAuthorized ImagesR                     _ = return Authorized
    isAuthorized (ImageR imageId)            _ = getOwnership imageId >>= canRead
    isAuthorized CreateImageR                _ = isLoggedIn 
    isAuthorized (EditImageR imageId)        _ = getOwnership imageId >>= isOwner
    isAuthorized (DeleteImageR imageId)      _ = getOwnership imageId >>= isOwner
    -- the protection here is that the user can't guess the md5 from the route
    -- thus if we receive a request with good md5 we let it through, otherwise
    -- 404. This way we don't have to look up the database.
    isAuthorized (ImageFileR _ _)            _ = return Authorized
                                            
    isAuthorized GalleriesR                  _ = return Authorized
    isAuthorized GalleryTreeR                _ = isLoggedIn
    isAuthorized NewGalleryR                 _ = isLoggedIn
    -- would make sense to limit this to owner
    isAuthorized (NewChildGalleryR _)        _ = isLoggedIn
    isAuthorized (EditGalleryR galleryId)    _ = getOwnership galleryId >>= isOwner
    isAuthorized (MoveGalleryR galleryId _)  _ = getOwnership galleryId >>= isOwner
    isAuthorized (MoveTopGalleryR galleryId) _ = getOwnership galleryId >>= isOwner
    isAuthorized (DeleteGalleryR galleryId)  _ = getOwnership galleryId >>= isOwner

    -- default deny 
    isAuthorized _ _ = return
       $ Unauthorized "This resource is not accessable because we are hitting default deny"

    maximumContentLength _ (Just CreateImageR) = 30 * 1024 * 1024 -- 30 Mb
    maximumContentLength _ _ =  512 * 1024                        -- 0.5 megabyte

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent =
        addStaticContentExternal minifym genFileName Settings.staticDir (StaticR . flip StaticRoute [])
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs
            | development = "autogen-" ++ base64md5 lbs
            | otherwise   = base64md5 lbs

    -- Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfBody

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog _ _source level =
        development || level == LevelWarn || level == LevelError

    getLogger = return . appLogger

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlPersist
    runDB f = do
        master <- getYesod
        Database.Persist.Store.runPool
            (persistConfig master)
            f
            (connPool master)

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest _ = HomeR

    getAuthId     = getAuthIdHashDB AuthR (Just . UniqueUser)
    authPlugins _ = [authHashDB (Just . UniqueUser)]

    authHttpManager = httpManager

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- | Get the 'Extra' value, used to hold data from the settings.yml file.
getExtra :: Handler Extra
getExtra = fmap (appExtra . settings) getYesod

-- Note: previous versions of the scaffolding included a deliver function to
-- send emails. Unfortunately, there are too many different options for us to
-- give a reasonable default. Instead, the information is available on the
-- wiki:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
