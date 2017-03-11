module Handler.ScreamDetail
    ( getScreamDetailR
    , singleScream
    ) where

import Text.Hamlet (hamletFile)

import Import
import Helper
import Query
import Markdown (strippedText)

getScreamDetailR :: ScreamId -> Handler Html
getScreamDetailR sid = do
    scream <- runDB $ fetch404 sid
    images <- runDB $ fetchImagesForScream scream
    defaultLayout $ do
        openGraphHead (scream, images)
        singleScream (scream, images)

singleScream :: (Entity Scream, [Entity Image]) -> Widget
singleScream (Entity sid scream, images) = do
    $(widgetFile "screams/show")

openGraphHead :: (Entity Scream, [Entity Image]) -> Widget
openGraphHead (Entity sid scream, images) =
        toWidgetHead $(hamletFile "templates/open-graph/scream.hamlet")
