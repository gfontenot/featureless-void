module Helper.Twitter.Endpoints
    ( updateStatusEndpoint
    ) where

import Import
import Helper.Twitter.Types

import qualified Data.ByteString.Char8 as BS

updateStatusEndpoint :: Text -> Endpoint
updateStatusEndpoint t = Endpoint
    { endpointPath = "statuses/update.json"
    , endpointMethod = "POST"
    , endpointBody = [(BS.pack "status", encodeUtf8 t)]
    }

