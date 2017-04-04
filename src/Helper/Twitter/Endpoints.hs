module Helper.Twitter.Endpoints
    ( updateStatusEndpoint
    , uploadMediaEndpoint
    ) where

import Import
import Helper.Twitter.Types

updateStatusEndpoint :: Text -> [Media] -> Endpoint
updateStatusEndpoint status images = Endpoint
    { endpointDomain = "api"
    , endpointPath = "statuses/update.json"
    , endpointBody = PostBody
        [ ("status", status)
        , ("media_ids",  joinIds images)
        ]
    }
  where
    joinIds :: [Media] -> Text
    joinIds = (intercalate ",") . (map mediaId)

uploadMediaEndpoint :: ByteString -> Endpoint
uploadMediaEndpoint fileData = Endpoint
    { endpointDomain = "upload"
    , endpointPath = "media/upload.json"
    , endpointBody = MultipartBody [("media", fileData)]
    }
