module Handler.Feed
    ( getXmlFeedR
    , getJsonFeedR
    ) where

import Yesod.RssFeed (RepRss(..))
import Text.Hamlet (hamletFile)

import Import hiding (feedTitle, feedDescription)
import Handler.Feed.Types
import Query
import Helper
import Markdown (strippedText)

getXmlFeedR :: Handler RepRss
getXmlFeedR = feedItems >>= generateFeed
  where
    generateFeed :: [FeedItem] -> Handler RepRss
    generateFeed items = toRss <$> feedLayout $(widgetFile "feed/main")

    feedLayout :: Widget -> Handler Html
    feedLayout widget = do
        pc <- widgetToPageContent widget
        withUrlRenderer $(hamletFile "templates/feed/wrapper.hamlet")

    toRss :: ToContent a => a -> RepRss
    toRss = RepRss . toContent

getJsonFeedR :: Handler Value
getJsonFeedR = feedItems >>= generateFeed
  where
    generateFeed :: [FeedItem] -> Handler Value
    generateFeed items = do
        render <- getUrlRender
        items' <- mapM (itemJSON render) items
        return $ object
            [ "version" .= ("https://jsonfeed.org/version/1" :: Text)
            , "title" .= feedTitle
            , "description" .= feedDescription
            , "home_page_url" .= render HomeR
            , "feed_url" .= render JsonFeedR
            , "items" .= items'
            ]

    itemJSON :: (Route App -> Text) -> FeedItem -> Handler Value
    itemJSON render item = do
        content <- widgetText (markupDescription item)
        return $ object
            [ "id" .= itemId item
            , "date_published" .= rfc3339Timestamp (itemCreatedAt item)
            , "url" .= render (ScreamDetailR $ itemId item)
            , "content_text" .= strippedText (itemBody item)
            , "content_html" .= content
            , "author" .= author
            ]

    author :: Value
    author = object
        [ "name" .= ("Gordon Fontenot" :: Text)
        , "url" .= ("http://gordonfontenot.com" :: Text)
        ]

markupDescription :: FeedItem -> Widget
markupDescription item = $(widgetFile "feed/description")

feedItems :: Handler [FeedItem]
feedItems = do
    screams <- runDB $ recentScreams
    images <- runDB $ fetchImagesForScreams screams
    return $ map (createItem . (joinOneToMany screamImage images)) screams

feedTitle :: Text
feedTitle = "micro.gordonfontenot.com"

feedDescription :: Text
feedDescription = "Gordon Fontenot's microblog"
