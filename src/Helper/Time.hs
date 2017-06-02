module Helper.Time
    ( rfc3339Timestamp
    , rssTimestamp
    , timestamp
    ) where

import Import

timestamp :: UTCTime -> String
timestamp = formatTime defaultTimeLocale timestampFormat

rssTimestamp :: UTCTime -> String
rssTimestamp = formatTime defaultTimeLocale rssTimestampFormat

rfc3339Timestamp :: UTCTime -> Text
rfc3339Timestamp = pack . formatTime defaultTimeLocale rfc3339TimestampFormat

timestampFormat :: String
timestampFormat = "%B %d, %Y - %l:%M %p"

rssTimestampFormat :: String
rssTimestampFormat = "%a, %d %b %Y %T %z"

rfc3339TimestampFormat :: String
rfc3339TimestampFormat = "%Y-%m-%dT%H:%M:%S%z"
