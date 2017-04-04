module Helper.Twitter.Types
    ( Endpoint(..)
    , Credentials(..)
    , Tweet(..)
    ) where

import ClassyPrelude.Yesod
import Data.Aeson
    ( FromJSON
    , withObject
    )

data Tweet = Tweet
    { tweetId :: Text
    } deriving (Eq, Show)

instance FromJSON Tweet where
    parseJSON = withObject "tweet" $ \o -> do
        tweetId <- o .: "id_str"

        return Tweet {..}

data Endpoint = Endpoint
    { endpointDomain :: String
    , endpointPath :: String
    , endpointMethod :: Method
    , endpointBody :: SimpleQuery
    }

data Credentials = Credentials
    { twitterConsumerKey :: Text
    , twitterConsumerSecret :: Text
    , twitterAccessToken :: Text
    , twitterAccessTokenSecret :: Text
    } deriving (Eq, Show)

instance FromJSON Credentials where
    parseJSON = withObject "twitter" $ \o -> do
        twitterConsumerKey <- o .: "consumer-key"
        twitterConsumerSecret <- o .: "consumer-secret"
        twitterAccessToken <- o .: "access-token"
        twitterAccessTokenSecret <- o .: "access-token-secret"

        return Credentials {..}

