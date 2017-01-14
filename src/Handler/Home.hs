module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = do
    screams <- runDB $ selectList [] [Desc ScreamId]
    defaultLayout $ do
        $(widgetFile "home/index")
