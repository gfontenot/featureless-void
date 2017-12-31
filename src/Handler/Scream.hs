module Handler.Scream
    ( getEditScreamR
    , getNewScreamR
    , getShowScreamR
    , postEditScreamR
    , postNewScreamR
    , showSingleScream
    ) where

import Text.Hamlet (hamletFile)

import Import
import Helper
import Query
import Markdown (Markdown, markdownField, plainText)

import qualified Helper.S3 as S3
import qualified Helper.Twitter as Twitter
import Helper.Twitter.Types (Tweet(..))

data EditScreamFields = EditScreamFields
    { editBodyField :: Markdown
    }

data NewScreamFields = NewScreamFields
    { newBodyField :: Markdown
    , newImageField :: Maybe FileInfo
    , newTwitterCrosspostField :: Bool
    }

-- Routes

getEditScreamR :: ScreamId -> Handler Html
getEditScreamR sid = do
    (Entity _ scream) <- runDB $ fetch404 sid
    (form, encType) <- generateFormPost $ editScreamForm scream
    renderScreamForm "Edit Scream" (EditScreamR sid) form encType

getNewScreamR :: Handler Html
getNewScreamR = do
    (form, enctype) <- generateFormPost newScreamForm
    renderScreamForm "New Scream" NewScreamR form enctype

getShowScreamR :: ScreamId -> Handler Html
getShowScreamR sid = do
    scream <- runDB $ fetch404 sid
    images <- runDB $ fetchImagesForScream scream
    authResult <- isAuthenticated
    defaultLayout $ do
        setTitle $ screamTitle scream
        openGraphHead (scream, images)
        showSingleScream authResult (scream, images)
  where
    screamTitle = toHtml . plainText . screamBody . entityVal

postEditScreamR :: ScreamId -> Handler Html
postEditScreamR sid = do
    (Entity _ scream) <- runDB $ fetch404 sid
    ((res, form), enctype) <- runFormPost $ editScreamForm scream
    case res of
      FormSuccess fields -> do
          let editedBody = editBodyField fields
          void $ runDB $ update sid [ScreamBody =. editedBody]
          redirect $ ShowScreamR sid
      _ -> renderScreamForm "EditScream" (EditScreamR sid) form enctype

postNewScreamR :: Handler Html
postNewScreamR = do
    ((res, form), enctype) <- runFormPost newScreamForm
    case res of
      FormSuccess fields -> do
          tid <- twitterCrosspostIfNecessary fields
          scream <- parseScream fields tid
          sid <- runDB $ insert scream
          images <- parseImages fields sid
          void $ runDB $ insertMany images
          redirect HomeR
      _ -> renderScreamForm "New Scream" NewScreamR form enctype

-- Rendering

showSingleScream :: AuthResult -> (Entity Scream, [Entity Image]) -> Widget
showSingleScream authResult (Entity sid scream, images) = do
    let authenticated = authResult == Authorized
    $(widgetFile "screams/show")

-- Helpers

openGraphHead :: (Entity Scream, [Entity Image]) -> Widget
openGraphHead (Entity sid scream, images) =
        toWidgetHead $(hamletFile "templates/open-graph/scream.hamlet")

twitterCrosspostIfNecessary :: NewScreamFields -> Handler (Maybe Text)
twitterCrosspostIfNecessary fields
    | newTwitterCrosspostField fields = do
        images <- mapM Twitter.uploadImage $ maybeToList $ newImageField fields
        let status = plainText . newBodyField $ fields
        tweet <- Twitter.postTweet status images
        return $ Just $ tweetId tweet
    | otherwise = return Nothing

parseImages :: NewScreamFields -> ScreamId -> Handler [Image]
parseImages f sid = do
    images <- S3.uploadItems $ map createUpload $ maybeToList $ newImageField f
    return $ fmap createImage images
  where
    createImage (desc, url) = Image sid (S3.uploadFileName desc) url

parseScream :: NewScreamFields -> Maybe Text -> Handler Scream
parseScream f tid = do
    now <- liftIO getCurrentTime
    return Scream
        { screamBody = newBodyField f
        , screamCreatedAt = now
        , screamTweetId = tid
        }

editScreamForm :: Scream -> Form EditScreamFields
editScreamForm scream = renderDivs $
    EditScreamFields
        <$> screamBodyField (Just $ screamBody scream)

newScreamForm :: Form NewScreamFields
newScreamForm = renderDivs $
    NewScreamFields
        <$> screamBodyField Nothing
        <*> fileAFormOpt "Image"
        <*> areq checkBoxField "Crosspost to Twitter" (Just True)

screamBodyField :: Maybe Markdown -> AForm Handler Markdown
screamBodyField = areq markdownField ("Body" { fsId = Just "scream-body" })

renderScreamForm :: Html -> Route App -> Widget -> Enctype -> Handler Html
renderScreamForm title action form enctype = defaultLayout $ do
    setTitle title
    addScript $ StaticR js_markdown_min_js
    addScript $ StaticR js_twitter_text_min_js
    addScript $ StaticR js_char_count_js
    $(widgetFile "screams/form")

createUpload :: FileInfo -> S3.Upload
createUpload info = S3.Upload
    { uploadSource = fileSource info
    , uploadContentType = fileContentType info
    , uploadFileName = fileName info
    }
