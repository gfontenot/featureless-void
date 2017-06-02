module Handler.Feed
    ( getFeedR
    ) where

import Yesod.RssFeed (RepRss(..))
import Text.Hamlet (hamletFile)

import Import hiding (Feed, feedTitle, feedDescription)
import Handler.Feed.Types
import Query
import Helper
import Markdown (strippedText)

getFeedR :: Handler TypedContent
getFeedR = do
    render <- getUrlRender
    items <- fetchItems
    feed <- generateFeed render items

    selectRep $ do
        provideRep $
            toRss <$> feedLayout feed $(widgetFile "feed/xml/items")

        provideJson feed
  where
    feedLayout :: Feed -> Widget -> Handler Html
    feedLayout feed widget = do
        pc <- widgetToPageContent widget
        withUrlRenderer $(hamletFile "templates/feed/xml/wrapper.hamlet")

    toRss :: ToContent a => a -> RepRss
    toRss = RepRss . toContent

generateFeed :: (Route App -> Text) -> [PopulatedScream] -> Handler Feed
generateFeed render screams = do
    items <- mapM (generateFeedItem render) screams

    return Feed
        { feedTitle = "micro.gordonfontenot.com"
        , feedDescription = "Gordon Fontenot's microblog"
        , feedHomeUrl = render HomeR
        , feedFeedUrl = render FeedR
        , feedItems = items
        }

generateFeedItem :: (Route App -> Text) -> PopulatedScream -> Handler FeedItem
generateFeedItem render item = do
    content <- widgetText (markupDescription item)

    return FeedItem
        { feedItemId = populatedScreamId item
        , feedItemPublishedAt = rfc3339Timestamp (populatedScreamCreatedAt item)
        , feedItemUrl = render (ScreamDetailR $ populatedScreamId item)
        , feedItemTextContent = strippedText (populatedScreamBody item)
        , feedItemHtmlContent = content
        , feedItemAuthor = def
        }

markupDescription :: PopulatedScream -> Widget
markupDescription item = $(widgetFile "feed/scream-content")

fetchItems :: Handler [PopulatedScream]
fetchItems = do
    screams <- runDB $ recentScreams
    images <- runDB $ fetchImagesForScreams screams
    return $ map (joinOneToMany screamImage images) screams
