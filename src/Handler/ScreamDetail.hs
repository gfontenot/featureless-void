module Handler.ScreamDetail
    ( getScreamDetailR
    ) where

import Import

getScreamDetailR :: ScreamId -> Handler Html
getScreamDetailR sid = do
    scream <- runDB $ get404 sid
    defaultLayout $ do
        setTitle "Post from"
        $(widgetFile "screams/show")
