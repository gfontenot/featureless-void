{-# Language CPP #-}
module Settings where

import ClassyPrelude.Yesod
import qualified Control.Exception as Exception
import Data.Aeson
    ( Result (..)
    , fromJSON
    , withObject
    , (.!=)
    , (.:?)
    )
import Data.FileEmbed (embedFile)
import Data.Yaml (decodeEither')
import Database.Persist.Postgresql (PostgresConf)
import Web.Heroku.Persist.Postgresql (fromDatabaseUrl)
import Language.Haskell.TH.Syntax
    ( Exp
    , Name
    , Q
    )
import Network.Wai.Handler.Warp (HostPreference)
import Yesod.Default.Config2
    ( applyEnvValue
    , configSettingsYml
    )

import Yesod.Default.Util
#if DEVELOPMENT
    (widgetFileReload)
#else
    (widgetFileNoReload)
#endif

import qualified Helper.Twitter.Types as Twitter

data AppSettings = AppSettings
    { appStaticDir              :: String
    , appDatabaseConf           :: PostgresConf
    , appRoot                   :: Maybe Text
    , appHost                   :: HostPreference
    , appPort                   :: Int
    , appForceSSL               :: Bool
    , appIpFromHeader           :: Bool
    , appLogLevel               :: LogLevel
    , appMutableStatic          :: Bool
    , appSkipCombining          :: Bool
    , appS3Bucket               :: Text
    , appTwitterCredentials     :: Twitter.Credentials
    }

instance FromJSON AppSettings where
    parseJSON = withObject "AppSettings" $ \o -> do
        let defaultDev =
#if DEVELOPMENT
                True
#else
                False
#endif
        appStaticDir              <- o .: "static-dir"
        appDatabaseConf           <- fromDatabaseUrl
            <$> o .: "database-pool-size"
            <*> o .: "database-url"
        appRoot                   <- o .:? "approot"
        appHost                   <- fromString <$> o .: "host"
        appPort                   <- o .: "port"
        appForceSSL               <- o .: "force-ssl"
        appIpFromHeader           <- o .: "ip-from-header"
        appLogLevel               <- parseLogLevel <$> o .: "log-level"
        appMutableStatic          <- o .:? "mutable-static"   .!= defaultDev
        appSkipCombining          <- o .:? "skip-combining"   .!= defaultDev
        appS3Bucket               <- o .: "s3-bucket"
        appTwitterCredentials     <- o .: "twitter"

        return AppSettings {..}

      where
        parseLogLevel :: Text -> LogLevel
        parseLogLevel t = case toLower t of
            "debug" -> LevelDebug
            "info" -> LevelInfo
            "warn" -> LevelWarn
            "error" -> LevelError
            _ -> LevelOther t

allowsLevel :: AppSettings -> LogLevel -> Bool
allowsLevel AppSettings{..} = (>= appLogLevel)

-- | How static files should be combined.
combineSettings :: CombineSettings
combineSettings = def

-- The rest of this file contains settings which rarely need changing by a
-- user.

widgetFile :: String -> Q Exp
widgetFile =
#if DEVELOPMENT
    widgetFileReload
#else
     widgetFileNoReload
#endif
    def

-- | Raw bytes at compile time of @config/settings.yml@
configSettingsYmlBS :: ByteString
configSettingsYmlBS = $(embedFile configSettingsYml)

-- | @config/settings.yml@, parsed to a @Value@.
configSettingsYmlValue :: Value
configSettingsYmlValue = either Exception.throw id
                       $ decodeEither' configSettingsYmlBS

-- | A version of @AppSettings@ parsed at compile time from @config/settings.yml@.
compileTimeAppSettings :: AppSettings
compileTimeAppSettings =
    case fromJSON $ applyEnvValue False mempty configSettingsYmlValue of
        Error e -> error e
        Success settings -> settings

-- The following two functions can be used to combine multiple CSS or JS files
-- at compile time to decrease the number of http requests.
-- Sample usage (inside a Widget):
--
-- > $(combineStylesheets 'StaticR [style1_css, style2_css])

combineStylesheets :: Name -> [Route Static] -> Q Exp
combineStylesheets = combineStylesheets'
    (appSkipCombining compileTimeAppSettings)
    combineSettings

combineScripts :: Name -> [Route Static] -> Q Exp
combineScripts = combineScripts'
    (appSkipCombining compileTimeAppSettings)
    combineSettings
