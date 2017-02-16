module Task.TweetImport
    ( importTweets
    ) where

import Import

importTweets :: Text -> Handler ()
importTweets = putStrLn
