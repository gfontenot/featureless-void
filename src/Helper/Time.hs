module Helper.Time
    ( timestamp
    ) where

import Import

timestampFormat :: String
timestampFormat = "%B %d, %Y - %l:%M %p"

timestamp :: UTCTime -> String
timestamp = formatTime defaultTimeLocale timestampFormat
