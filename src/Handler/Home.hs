module Handler.Home where

import Import
import Helper

getHomeR :: Handler Html
getHomeR = do
    screams <- runDB $ selectList [] [Desc ScreamCreatedAt, Desc ScreamId]
    defaultLayout $ do
        $(widgetFile "home/index")
