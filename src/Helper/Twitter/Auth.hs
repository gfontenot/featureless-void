module Helper.Twitter.Auth
    ( twitterCredentials
    , twitterOAuth
    ) where

import Import
import Helper.Twitter.Types

import Web.Authenticate.OAuth

twitterOAuth :: Handler OAuth
twitterOAuth = do
    app <- getYesod

    return $
        def { oauthServerName = "twitter"
        , oauthRequestUri = "https://api.twitter.com/oauth/request_token"
        , oauthAccessTokenUri = "https://api.twitter.com/oauth/access_token"
        , oauthAuthorizeUri = "https://api.twitter.com/oauth/authorize"
        , oauthConsumerKey = consumerKey $ appSettings app
        , oauthConsumerSecret = consumerSecret $ appSettings app
        , oauthSignatureMethod = HMACSHA1
        , oauthCallback = Nothing
        }

twitterCredentials :: Handler Credential
twitterCredentials = do
    app <- getYesod

    return $
        newCredential
            (accessToken $ appSettings app)
            (accessTokenSecret $ appSettings app)

consumerKey :: AppSettings -> ByteString
consumerKey = extractSetting twitterConsumerKey

consumerSecret :: AppSettings -> ByteString
consumerSecret = extractSetting twitterConsumerSecret

accessToken :: AppSettings -> ByteString
accessToken = extractSetting twitterAccessToken

accessTokenSecret :: AppSettings -> ByteString
accessTokenSecret = extractSetting twitterAccessTokenSecret

extractSetting :: (Credentials -> Text) -> AppSettings -> ByteString
extractSetting f = encodeUtf8 . f . appTwitterCredentials
