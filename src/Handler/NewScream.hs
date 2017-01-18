module Handler.NewScream
    ( getNewScreamR
    , postNewScreamR
    ) where

import Import
import Yesod.Markdown (Markdown, markdownField)

getNewScreamR :: Handler Html
getNewScreamR = do
    (form, enctype) <- generateFormPost screamForm
    renderNewScream form enctype

postNewScreamR :: Handler Html
postNewScreamR = do
    ((res, form), enctype) <- runFormPost screamForm
    case res of
      FormSuccess fields -> do
          scream <- fromScreamFields fields
          _screamId <- runDB $ insert scream
          redirect HomeR
      _ -> renderNewScream form enctype

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
        <$> areq markdownField ("Body" { fsId = Just "scream-body" }) Nothing

renderNewScream :: Widget -> Enctype -> Handler Html
renderNewScream form enctype = defaultLayout $ do
    setTitle "New Scream"
    addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/jquery/2.1.4/jquery.js"
    addScript $ StaticR js_markdown_min_js
    addScript $ StaticR js_char_count_js
    $(widgetFile "screams/new")
