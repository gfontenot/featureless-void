module Handler.Micropub.Auth.Types
    ( ApiQueryParams(..)
    , parseApiQueryParams
    ) where

import Import.NoFoundation

data ApiQueryParams = ApiQueryParams
    { paramMe :: Text
    , paramRedirectUri :: Text
    , paramClientId :: Text
    , paramState :: Text
    , paramScope :: Text
    , paramResponseType :: Text
    } deriving (Show)

parseApiQueryParams :: [(Text, Text)] -> Maybe ApiQueryParams
parseApiQueryParams ps = ApiQueryParams
    <$> lookup "me" ps
    <*> lookup "redirect_uri" ps
    <*> lookup "client_id" ps
    <*> lookup "state" ps
    <*> lookup "scope" ps
    <*> lookup "response_type" ps

