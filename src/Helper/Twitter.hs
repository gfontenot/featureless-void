module Helper.Twitter
    ( postTweet
    , uploadImage
    ) where

import Network.HTTP.Simple (httpJSON)
import Data.Conduit.Binary (sinkLbs)

import Import
import Helper.Twitter.Base
import Helper.Twitter.Endpoints
import Helper.Twitter.Types

postTweet :: Text -> [Media] -> Handler Tweet
postTweet status images = do
    let endpoint = updateStatusEndpoint status images
    req <- makeRequest endpoint
    responseBody <$> httpJSON req

uploadImage :: FileInfo -> Handler Media
uploadImage info = do
    bytes <- fileSource info $$ sinkLbs
    let endpoint = uploadMediaEndpoint $ toStrict bytes
    req <- makeRequest endpoint
    responseBody <$> httpJSON req
