module Handler.Feed
    ( getXmlFeedR
    ) where

import Yesod.RssFeed (RepRss(..))
import Text.Hamlet (hamletFile)

import Import hiding (feedTitle, feedDescription)
import Query
import Helper
import Markdown (strippedText)

getXmlFeedR :: Handler RepRss
getXmlFeedR = feedItems >>= generateFeed
  where
    generateFeed :: [(Entity Scream, [Entity Image])] -> Handler RepRss
    generateFeed screams = toRss <$> feedLayout $(widgetFile "feed/main")

    markupDescription :: Scream -> [Entity Image] -> Widget
    markupDescription scream images =
        $(widgetFile "feed/description")

    feedLayout :: Widget -> Handler Html
    feedLayout widget = do
        pc <- widgetToPageContent widget
        withUrlRenderer $(hamletFile "templates/feed/wrapper.hamlet")

    toRss :: ToContent a => a -> RepRss
    toRss = RepRss . toContent

feedItems :: Handler [(Entity Scream, [Entity Image])]
feedItems = do
    screams <- runDB $ recentScreams
    images <- runDB $ fetchImagesForScreams screams
    return $ map (joinOneToMany screamImage images) screams

feedTitle :: Text
feedTitle = "micro.gordonfontenot.com"

feedDescription :: Text
feedDescription = "Gordon Fontenot's microblog"
