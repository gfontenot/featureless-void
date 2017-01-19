module Handler.Home where

import Import
import Helper
import Yesod.Paginator.Simple

getHomeR :: Handler Html
getHomeR = do
    (screams, pagination) <- fetchScreams
    defaultLayout $ do
        $(widgetFile "home/index")

fetchScreams :: Handler ([Entity Scream], Widget)
fetchScreams = runDB $
    selectSimplePaginated
        30                                    -- number of items per page
        []                                    -- filters
        [Desc ScreamCreatedAt, Desc ScreamId] -- sort descriptors
