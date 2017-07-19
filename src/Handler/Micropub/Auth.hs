module Handler.Micropub.Auth
    ( getMicropubAuthR
    , postMicropubAuthR
    ) where

import Yesod.Auth.HashDB (validateUser)
import Handler.Micropub.Auth.Types

import Import

getMicropubAuthR :: Handler Html
getMicropubAuthR = do
    params <- (parseApiQueryParams . reqGetParams) <$> getRequest
    renderLoginForm params

postMicropubAuthR :: Handler Html
postMicropubAuthR = do
    ((res, _), _) <- runFormPost loginResult
    case res of
      FormSuccess auth -> do
          authenticated <- validateUser (UniqueUser $ authUsername auth) $ authPassword auth
          case authenticated of
            True -> do
                setMessage "Authenticated!!!"
                renderLoginForm Nothing
            False -> do
                setMessage "Invalid login"
                renderLoginForm Nothing
      FormFailure es -> do
          setMessage $ toHtml $ "Failure!\n" ++ (unlines es)
          renderLoginForm Nothing
      FormMissing -> do
          setMessage "???"
          renderLoginForm Nothing

renderLoginForm :: Maybe ApiQueryParams -> Handler Html
renderLoginForm = defaultLayout . loginForm MicropubAuthR

data UserAuth = UserAuth
    { authUsername :: Text
    , authPassword :: Text
    }

-- parseResult :: Text -> Text -> Text -> Text -> Text -> Text -> Text -> Text -> (UserAuth, ApiQueryParams)
-- parseResult uname pwd me redirectUri client state scope responseCode =
--     ( UserAuth uname pwd
--     , ApiQueryParams me redirectUri client state scope responseCode
--     )


-- loginResult :: Form (UserAuth, ApiQueryParams)
-- loginResult = renderDivs $ parseResult
--     <$> areq textField "username" Nothing
--     <*> areq textField "password" Nothing
--     <*> areq textField "api-me" Nothing
--     <*> areq textField "api-redirect-uri" Nothing
--     <*> areq textField "api-client-id" Nothing
--     <*> areq textField "api-state" Nothing
--     <*> areq textField "api-scope" Nothing
--     <*> areq textField "api-response-type" Nothing
--
loginResult :: Form UserAuth
loginResult = renderDivs $
    UserAuth
        <$> areq textField "username" Nothing
        <*> areq textField "password" Nothing

-- loginResult :: Form (UserAuth, ApiQueryParams)
-- loginResult = renderDivs $ (,)
--     <$> ( UserAuth
--         <$> areq textField "username" Nothing
--         <*> areq textField "password" Nothing
--         )
--     <*> ( ApiQueryParams
--         <$> areq textField "api-me" Nothing
--         <*> areq textField "api-redirect-uri" Nothing
--         <*> areq textField "api-client-id" Nothing
--         <*> areq textField "api-state" Nothing
--         <*> areq textField "api-scope" Nothing
--         <*> areq textField "api-response-type" Nothing
--         )
