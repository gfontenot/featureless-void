module Handler.Feed.Types
    ( FeedItem
    , createItem
    , itemId
    , itemBody
    , itemCreatedAt
    , itemImages
    ) where

import Import
import Markdown (Markdown)

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
