module Helper.Twitter.Base
    ( makeRequest
    ) where

import Web.Authenticate.OAuth (signOAuth)
import Network.HTTP.Simple (setRequestSecure)
import Network.HTTP.Client.MultipartFormData (formDataBody, partBS, Part)

import Import
import Helper.Twitter.Auth
import Helper.Twitter.Types

makeRequest :: Endpoint -> Handler Request
makeRequest e = parseRequest (url (endpointDomain e) (endpointPath e))
    >>= addParams e
    >>= forceSecure
    >>= signed

url :: String -> String -> String
url domain path = "https://" <> domain <> ".twitter.com/1.1" </> path

signed :: Request -> Handler Request
signed req = do
    oauth <- twitterOAuth
    creds <- twitterCredentials
    signOAuth oauth creds req

forceSecure :: Request -> Handler Request
forceSecure = return . setRequestSecure True

addParams :: Endpoint -> Request -> Handler Request
addParams (Endpoint _ _ POSTRequest b) = return . urlEncodedBody b
addParams (Endpoint _ _ MultipartRequest b) = formDataBody (formBody b)

formBody :: SimpleQuery -> [Part]
formBody b = map queryToPart b
  where
    queryToPart :: SimpleQueryItem -> Part
    queryToPart (k, v) = partBS (decodeUtf8 k) v
