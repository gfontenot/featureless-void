module Handler.ScreamDetail
    ( getScreamDetailR
    , singleScream
    ) where

import Import
import Helper
import Query

getScreamDetailR :: ScreamId -> Handler Html
getScreamDetailR sid = do
    scream <- runDB $ fetch404 sid
    images <- runDB $ fetchImagesForScream scream
    defaultLayout $ do
        setTitle "Post from"
        singleScream (scream, images)

singleScream :: (Entity Scream, [Entity Image]) -> Widget
singleScream (Entity sid scream, images) = do
    $(widgetFile "screams/show")
