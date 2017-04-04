module Helper.Twitter
    ( postTweet
    , uploadImages
    ) where

import Network.HTTP.Simple (httpJSON)
import Data.Conduit.Binary (sinkLbs)

import Import
import Helper.Twitter.Base
import Helper.Twitter.Endpoints
import Helper.Twitter.Types

postTweet :: Text -> [Media] -> Handler Tweet
postTweet status images = do
    let endpoint = updateStatusEndpoint status $ map mediaId images
    req <- makeRequest endpoint
    responseBody <$> httpJSON req

uploadImages :: FileInfo -> Handler Media
uploadImages info = do
    bytes <- fileSource info $$ sinkLbs
    let endpoint = uploadMediaEndpoint $ toStrict bytes
    req <- makeRequest endpoint
    responseBody <$> httpJSON req
