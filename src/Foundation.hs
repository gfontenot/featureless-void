module Foundation where

import Import.NoFoundation
import Database.Persist.Sql
    ( ConnectionPool
    , runSqlPool
    )
import Text.Hamlet (hamletFile)
import Text.Jasmine (minifym)

import Yesod.Auth.Message (AuthMessage(..))
import Yesod.Auth.HashDB (authHashDBWithForm)
import Yesod.RssFeed (rssLink)

import Yesod.Default.Util (addStaticContentExternal)
import Yesod.Core.Types (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE

data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

csrfTokenJs :: a -> Html
csrfTokenJs = $(hamletFile "templates/csrf-token.hamlet")

oneWeek :: Int
oneWeek = 60 * 24 * 7

whenSSL :: AppSettings -> (a -> a) -> (a -> a)
whenSSL settings f = if appForceSSL settings then f else id

instance Yesod App where
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing -> getApprootText guessApproot app req
            Just root -> root

    makeSessionBackend app = whenSSL (appSettings app) sslOnlySessions $
        Just <$> envClientSessionBackend oneWeek "SESSION_KEY"

    yesodMiddleware handler = do
        settings <- appSettings <$> getYesod

        defaultCsrfMiddleware
            $ defaultYesodMiddleware
            $ whenSSL settings ( sslOnlyMiddleware oneWeek )
            $ handler

    defaultLayout widget = do
        mmsg <- getMessage

        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR css_reset_css
            addStylesheet $ StaticR css_screen_css
            addStylesheet $ StaticR css_microblog_css
            addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/jquery/2.1.4/jquery.js"
            addScript     $ StaticR js_app_js
            rssLink         FeedR "micro.gordonfontenot.com/feed"

            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- Routes not requiring authentication.
    isAuthorized NewScreamR _ = isAuthenticated
    isAuthorized _ _ = return Authorized

    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    shouldLog App{..} _source = (appSettings `allowsLevel`)

    makeLogger = return . appLogger

    -- Set the maximum content length for image uploads to 200 megs
    maximumContentLength _ (Just NewScreamR) = Just $ 200 * 1024 * 1024
    maximumContentLength _ _ = Just $ 2 * 1024 * 1024 -- 2 megabytes by default

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = UserId

    loginDest _ = NewScreamR
    logoutDest _ = HomeR
    authPlugins _ = [authHashDBWithForm loginForm (Just . UniqueUser)]

    authenticate creds = runDB $ do
        u <- getBy $ UniqueUser $ credsIdent creds
        return $ case u of
            Just (Entity uid _) -> Authenticated uid
            Nothing -> UserError Email

    authHttpManager = getHttpManager

loginForm :: Route App -> Widget
loginForm action = do
    request <- getRequest
    let mtok = reqToken request
    $(whamletFile "templates/login.hamlet")

isAuthenticated :: Handler AuthResult
isAuthenticated = do
    muid <- maybeAuthId
    return $ case muid of
        Nothing -> AuthenticationRequired
        Just _ -> Authorized

instance YesodAuthPersist App

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance HasHttpManager App where
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger
