module Handler.Home where

import Yesod.Paginator
    ( PageWidget
    , PageWidgetConfig(..)
    , defaultPageWidgetConfig
    , simplePaginationWidget
    )

import Import
import Query

import Handler.ScreamDetail (singleScream)

getHomeR :: Handler Html
getHomeR = do
    (screams', pagination) <- runDB $ paginatedScreams 30 simpleWidget
    images <- runDB $ fetchImagesForScreams screams'
    let screams = joinOneToMany screamImage screams' images
    defaultLayout $ do
        $(widgetFile "home/index")

simpleWidget :: PageWidget App
simpleWidget = simplePaginationWidget $ defaultPageWidgetConfig
    { prevText     = "Newer"
    , nextText     = "Older"
    }
