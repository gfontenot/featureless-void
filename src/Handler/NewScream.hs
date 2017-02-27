module Handler.NewScream
    ( getNewScreamR
    , postNewScreamR
    ) where

import Import
import Yesod.Markdown (Markdown, markdownField)
import qualified Helper.S3 as S3

getNewScreamR :: Handler Html
getNewScreamR = do
    (form, enctype) <- generateFormPost screamForm
    renderNewScream form enctype

postNewScreamR :: Handler Html
postNewScreamR = do
    ((res, form), enctype) <- runFormPost screamForm
    case res of
      FormSuccess fields -> do
          scream <- parseScream fields
          sid <- runDB $ insert scream
          images <- parseImages fields sid
          void $ runDB $ insertMany images
          redirect HomeR
      _ -> renderNewScream form enctype

data ScreamFields = ScreamFields
    { bodyField :: Markdown
    , imageField :: Maybe FileInfo
    }

parseImages :: ScreamFields -> ScreamId -> Handler [Image]
parseImages f sid = do
    images <- S3.uploadImages $ maybeToList $ imageField f
    return $ fmap createImage images
  where
      createImage (fileInfo, url) = Image
          { imageScreamId = sid
          , imageFileName = fileName fileInfo
          , imageUrl = url
          }

parseScream :: ScreamFields -> Handler Scream
parseScream f = do
    now <- liftIO getCurrentTime
    return Scream
        { screamBody = bodyField f
        , screamCreatedAt = now
        }

screamForm :: Form ScreamFields
screamForm = renderDivs $
    ScreamFields
        <$> areq markdownField ("Body" { fsId = Just "scream-body" }) Nothing
        <*> fileAFormOpt "Image"

renderNewScream :: Widget -> Enctype -> Handler Html
renderNewScream form enctype = defaultLayout $ do
    setTitle "New Scream"
    addScript $ StaticR js_markdown_min_js
    addScript $ StaticR js_char_count_js
    $(widgetFile "screams/new")
