module Handler.Feed
    ( getFeedR
    ) where

import Text.Hamlet (hamletFile)

import Import
import Query
import Helper

getFeedR :: Handler Html
getFeedR = do
    screams' <- runDB $ recentScreams
    images <- runDB $ fetchImagesForScreams screams'
    feedFromScreams $ map (joinOneToMany screamImage images) screams'

feedFromScreams :: [(Entity Scream, [Entity Image])] -> Handler Html
feedFromScreams screams = feedLayout $(widgetFile "feed/main")

feedLayout :: Widget -> Handler Html
feedLayout widget = do
    pc <- widgetToPageContent widget
    withUrlRenderer $(hamletFile "templates/feed/wrapper.hamlet")

markupDescription :: Scream -> [Entity Image] -> Widget
markupDescription scream images =
    $(widgetFile "feed/description")
