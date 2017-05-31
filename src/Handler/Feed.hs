module Handler.Feed
    ( getXmlFeedR
    ) where

import Yesod.RssFeed (RepRss(..))
import Text.Hamlet (hamletFile)

import Import hiding (feedTitle, feedDescription)
import Query
import Helper
import Markdown (Markdown, strippedText)

data FeedItem = FeedItem
    { feedItemScream :: Entity Scream
    , feedItemImages :: [Entity Image]
    }

createItem :: (Entity Scream, [Entity Image]) -> FeedItem
createItem = uncurry FeedItem

itemId :: FeedItem -> ScreamId
itemId = entityKey . feedItemScream

itemBody :: FeedItem -> Markdown
itemBody = screamBody . entityVal . feedItemScream

itemCreatedAt :: FeedItem -> UTCTime
itemCreatedAt = screamCreatedAt . entityVal . feedItemScream

itemImages :: FeedItem -> [Image]
itemImages = (map entityVal) . feedItemImages

getXmlFeedR :: Handler RepRss
getXmlFeedR = feedItems >>= generateFeed
  where
    generateFeed :: [FeedItem] -> Handler RepRss
    generateFeed items = toRss <$> feedLayout $(widgetFile "feed/main")

    markupDescription :: FeedItem -> Widget
    markupDescription item =
        $(widgetFile "feed/description")

    feedLayout :: Widget -> Handler Html
    feedLayout widget = do
        pc <- widgetToPageContent widget
        withUrlRenderer $(hamletFile "templates/feed/wrapper.hamlet")

    toRss :: ToContent a => a -> RepRss
    toRss = RepRss . toContent

feedItems :: Handler [FeedItem]
feedItems = do
    screams <- runDB $ recentScreams
    images <- runDB $ fetchImagesForScreams screams
    return $ map (createItem . (joinOneToMany screamImage images)) screams

feedTitle :: Text
feedTitle = "micro.gordonfontenot.com"

feedDescription :: Text
feedDescription = "Gordon Fontenot's microblog"
