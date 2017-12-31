module Handler.Home where

import Text.Hamlet (hamletFile)
import Yesod.Paginator
    ( PageWidget
    , PageWidgetConfig(..)
    , defaultPageWidgetConfig
    , simplePaginationWidget
    )

import Import
import Query

import Handler.Scream (showSingleScream)

getHomeR :: Handler Html
getHomeR = do
    (screams', pagination) <- runDB $ paginatedScreams 30 simpleWidget
    images <- runDB $ fetchImagesForScreams screams'
    let screams = map (joinOneToMany screamImage images) screams'
    defaultLayout $ do
        setTitle "micro.gordonfontenot.com"
        openGraphHead
        $(widgetFile "home/index")

simpleWidget :: PageWidget App
simpleWidget = simplePaginationWidget $ defaultPageWidgetConfig
    { prevText     = "Newer"
    , nextText     = "Older"
    }

openGraphHead :: Widget
openGraphHead =
        toWidgetHead $(hamletFile "templates/open-graph/home.hamlet")
