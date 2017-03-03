module Task.TweetImport
    ( importTweet
    ) where

import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Yesod.Markdown
import System.Directory (listDirectory)
import System.FilePath.Posix (takeFileName)
import Network.Mime (defaultMimeLookup)

import Import
import qualified Helper.S3 as S3

data Tweet = Tweet
    { tweetCreatedAt :: UTCTime
    , tweetBody :: Text
    , tweetId :: Text
    } deriving (Show)

instance FromJSON Tweet where
    parseJSON = withObject "tweet" $ \o -> do
        tweetId <- o .: "id"
        tweetBody <- o .: "text"
        tweetCreatedAt <- o .: "created_at"
        return Tweet{..}

importTweet :: FilePath -> Handler ()
importTweet p = do
    scream <- liftIO $ decodeScream p
    sid <- runDB $ insert scream
    images <- uploadImages p sid
    void $ runDB $ insertMany images

uploadImages :: FilePath -> ScreamId -> Handler [Image]
uploadImages p sid = do
    descriptions <- liftIO $ imagesFrom p
    images <- S3.uploadItems descriptions
    return $ fmap createImage images
  where
    createImage (desc, url) = Image sid (S3.uploadFileName desc) url

imagesFrom :: FilePath -> IO [S3.UploadDescription]
imagesFrom p = do
    let photosPath = p </> "photos"
    imagePaths <- fmap (photosPath </>) <$> listDirectory photosPath
    return $ fmap createDescription imagePaths

createDescription :: FilePath -> S3.UploadDescription
createDescription p = S3.UploadDescription
    { uploadSource = sourceFile p
    , uploadContentType = mimeType p
    , uploadFileName = pack $ takeFileName p
    }

decodeScream :: FilePath -> IO Scream
decodeScream p = do
    let entryPath = p </> "entry.json"
    bs <- B.readFile entryPath
    case eitherDecode bs of
        Left e -> throwIO $ userError e
        Right t -> return $ toScream t

mimeType :: FilePath -> Text
mimeType = decodeUtf8 . defaultMimeLookup . pack

toScream :: Tweet -> Scream
toScream t = Scream (Markdown $ tweetBody t) (tweetCreatedAt t) (Just $ tweetId t)
