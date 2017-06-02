module Handler.Feed.Types
    ( Feed(..)
    , FeedItem(..)
    ) where

import Import hiding (Feed, feedTitle, feedDescription)

data Feed = Feed
    { feedTitle :: Text
    , feedDescription :: Text
    , feedHomeUrl :: Text
    , feedFeedUrl :: Text
    , feedItems :: [FeedItem]
    }

data FeedItem = FeedItem
    { feedItemId :: ScreamId
    , feedItemPublishedAt :: Text
    , feedItemUrl :: Text
    , feedItemTextContent :: Text
    , feedItemHtmlContent :: Text
    , feedItemAuthor :: FeedItemAuthor
    }

data FeedItemAuthor = FeedItemAuthor
    { feedItemAuthorName :: Text
    , feedItemAuthorUrl :: Text
    }

instance ToJSON Feed where
    toJSON feed = object
        [ "version" .= ("https://jsonfeed.org/version/1" :: Text)
        , "title" .= feedTitle feed
        , "description" .= feedDescription feed
        , "home_page_url" .= feedHomeUrl feed
        , "feed_url" .= feedFeedUrl feed
        , "items" .= feedItems feed
        ]

instance ToJSON FeedItem where
    toJSON item = object
        [ "id" .= feedItemId item
        , "date_published" .= feedItemPublishedAt item
        , "url" .= feedItemUrl item
        , "content_text" .= feedItemTextContent item
        , "content_html" .= feedItemHtmlContent item
        , "author" .= feedItemAuthor item
        ]

instance ToJSON FeedItemAuthor where
    toJSON author = object
        [ "name" .= feedItemAuthorName author
        , "url" .= feedItemAuthorUrl author
        ]

instance Default FeedItemAuthor where
    def = FeedItemAuthor
        { feedItemAuthorName = "Gordon Fontenot"
        , feedItemAuthorUrl = "http://gordonfontenot.com"
        }
