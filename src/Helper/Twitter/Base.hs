module Helper.Twitter.Base
    ( makeRequest
    ) where

import Web.Authenticate.OAuth (signOAuth)
import Network.HTTP.Simple (setRequestSecure)

import Import
import Helper.Twitter.Auth
import Helper.Twitter.Types

makeRequest :: Endpoint -> Handler Request
makeRequest e = do
    req <- parseRequest $ url (endpointDomain e) (endpointPath e)
    signed $ secure $ addParams e $ req { method = endpointMethod e }
  where
    url :: String -> String -> String
    url domain path = "https://" <> domain <> ".twitter.com/1.1" </> path

    signed :: Request -> Handler Request
    signed req = do
        oauth <- twitterOAuth
        creds <- twitterCredentials
        signOAuth oauth creds req

    secure :: Request -> Request
    secure = setRequestSecure True

    addParams :: Endpoint -> Request -> Request
    addParams (Endpoint _ _ m b)
      | m == "POST" = urlEncodedBody b
      | otherwise   = \r -> r { queryString = renderSimpleQuery False b }
