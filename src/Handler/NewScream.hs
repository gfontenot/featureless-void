module Handler.NewScream
    ( getNewScreamR
    , postNewScreamR
    ) where

import Import
import Yesod.Markdown (Markdown, markdownField)

data ScreamFields = ScreamFields
    { bodyField :: Markdown
    }

fromScreamFields :: ScreamFields -> Handler Scream
fromScreamFields f = do
    now <- liftIO getCurrentTime
    return Scream
        { screamBody = bodyField f
        , screamCreatedAt = now
        }

screamForm :: Form ScreamFields
screamForm = renderDivs $
    ScreamFields
        <$> areq markdownField "Body" Nothing

getNewScreamR :: Handler Html
getNewScreamR = do
    (form, enctype) <- generateFormPost screamForm
    defaultLayout $ do
        setTitle "New Scream"
        $(widgetFile "screams/new")

postNewScreamR :: Handler Html
postNewScreamR = do
    ((res, form), enctype) <- runFormPost screamForm
    case res of
      FormSuccess fields -> do
          scream <- fromScreamFields fields
          _screamId <- runDB $ insert scream
          redirect HomeR
      _ -> defaultLayout $(widgetFile "screams/new")
