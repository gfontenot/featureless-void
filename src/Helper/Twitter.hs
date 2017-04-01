module Helper.Twitter
    ( postTweet
    ) where

import Network.HTTP.Simple (httpJSON)

import Import
import Helper.Twitter.Base
import Helper.Twitter.Endpoints
import Helper.Twitter.Types

postTweet :: Text -> Handler Tweet
postTweet status = do
    let endpoint = updateStatusEndpoint status
    req <- makeRequest endpoint
    responseBody <$> httpJSON req
