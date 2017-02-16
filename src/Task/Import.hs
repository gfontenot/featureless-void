module Task.Import
    ( importTweets
    ) where

import Import

importTweets :: Text -> Handler ()
importTweets = putStrLn
