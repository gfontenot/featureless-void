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
    >>= addBody (endpointBody e)
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

addBody :: Body -> Request -> Handler Request
addBody (PostBody b) = return . encodedBody b
addBody (MultipartBody b) = formDataBody (formBody b)

encodedBody :: [(Text, Text)] -> Request -> Request
encodedBody = urlEncodedBody . map toQueryItem
  where
    toQueryItem :: (Text, Text) -> SimpleQueryItem
    toQueryItem (k, v) = (encodeUtf8 k, encodeUtf8 v)

formBody :: [(Text, ByteString)] -> [Part]
formBody = map (uncurry partBS)
