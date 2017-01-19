module Handler.ScreamDetail
    ( getScreamDetailR
    , singleScream
    ) where

import Import
import Helper

getScreamDetailR :: ScreamId -> Handler Html
getScreamDetailR sid = do
    scream <- runDB $ get404 sid
    defaultLayout $ do
        setTitle "Post from"
        singleScream sid scream

singleScream :: ScreamId -> Scream -> Widget
singleScream sid scream = do
    $(widgetFile "screams/show")
