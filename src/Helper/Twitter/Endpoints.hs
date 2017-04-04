module Helper.Twitter.Endpoints
    ( updateStatusEndpoint
    , uploadMediaEndpoint
    ) where

import Import
import Helper.Twitter.Types

import qualified Data.ByteString.Char8 as BS

updateStatusEndpoint :: Text -> [Text] -> Endpoint
updateStatusEndpoint status images = Endpoint
    { endpointDomain = "api"
    , endpointPath = "statuses/update.json"
    , endpointType = POSTRequest
    , endpointBody = [
        (BS.pack "status", encodeUtf8 status),
        (BS.pack "media_ids", encodeUtf8 $ intercalate "," images)
      ]
    }

uploadMediaEndpoint :: ByteString -> Endpoint
uploadMediaEndpoint fileData = Endpoint
    { endpointDomain = "upload"
    , endpointPath = "media/upload.json"
    , endpointType = MultipartRequest
    , endpointBody = [(BS.pack "media", fileData)]
    }
