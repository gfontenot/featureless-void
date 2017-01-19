module Handler.Home where

import Import
import Helper
import Yesod.Paginator
import Yesod.Paginator.Simple

import Handler.ScreamDetail (singleScream)

getHomeR :: Handler Html
getHomeR = do
    (screams, pagination) <- fetchScreams
    defaultLayout $ do
        $(widgetFile "home/index")

fetchScreams :: Handler ([Entity Scream], Widget)
fetchScreams = runDB $
    selectPaginatedWith
        simplePaginationWidget                -- pagination widget
        30                                    -- number of items per page
        []                                    -- filters
        [Desc ScreamCreatedAt, Desc ScreamId] -- sort descriptors
