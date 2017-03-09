module Helper.Time
    ( rssTimestamp
    , timestamp
    ) where

import Import

timestamp :: UTCTime -> String
timestamp = formatTime defaultTimeLocale timestampFormat

rssTimestamp :: UTCTime -> String
rssTimestamp = formatTime defaultTimeLocale rssTimestampFormat

timestampFormat :: String
timestampFormat = "%B %d, %Y - %l:%M %p"

rssTimestampFormat :: String
rssTimestampFormat = "%a, %d %b %Y %T %z"
