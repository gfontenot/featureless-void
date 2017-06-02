module Helper.Time
    ( rfc3339Timestamp
    , displayTimestamp
    ) where

import Import

displayTimestamp :: UTCTime -> String
displayTimestamp = formatTime defaultTimeLocale displayTimestampFormat

rfc3339Timestamp :: UTCTime -> Text
rfc3339Timestamp = pack . formatTime defaultTimeLocale rfc3339TimestampFormat

displayTimestampFormat :: String
displayTimestampFormat = "%B %d, %Y - %l:%M %p"

rfc3339TimestampFormat :: String
rfc3339TimestampFormat = "%Y-%m-%dT%H:%M:%S%z"
