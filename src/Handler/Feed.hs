module Handler.Feed
    ( getFeedR
    ) where

import Yesod.RssFeed (RepRss(..))
import Text.Hamlet (hamletFile)

import Import
import Query
import Helper

getFeedR :: Handler RepRss
getFeedR = do
    screams' <- runDB $ recentScreams
    images <- runDB $ fetchImagesForScreams screams'
    generateFeed $ map (joinOneToMany screamImage images) screams'

generateFeed :: [(Entity Scream, [Entity Image])] -> Handler RepRss
generateFeed screams = toRss <$> feedLayout $(widgetFile "feed/main")

feedLayout :: Widget -> Handler Html
feedLayout widget = do
    pc <- widgetToPageContent widget
    withUrlRenderer $(hamletFile "templates/feed/wrapper.hamlet")

markupDescription :: Scream -> [Entity Image] -> Widget
markupDescription scream images =
    $(widgetFile "feed/description")

toRss :: ToContent a => a -> RepRss
toRss = RepRss . toContent
