module Task.TweetImport
    ( importTweet
    ) where

import qualified Data.ByteString.Lazy as B
import Data.Aeson
import System.Directory (listDirectory, doesDirectoryExist)
import System.FilePath.Posix (takeFileName)
import Network.Mime (defaultMimeLookup)

import Import
import qualified Helper.S3 as S3
import Markdown

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
    uploads <- liftIO $ imagesFrom p
    images <- S3.uploadItems uploads
    return $ fmap createImage images
  where
    createImage (desc, url) = Image sid (S3.uploadFileName desc) url

imagesFrom :: FilePath -> IO [S3.Upload]
imagesFrom p = do
    let photosPath = p </> "photos"
    exists <- doesDirectoryExist photosPath
    case exists of
      True -> do
          imagePaths <- fmap (photosPath </>) <$> listDirectory photosPath
          return $ fmap createUpload imagePaths
      False -> return []

createUpload :: FilePath -> S3.Upload
createUpload p = S3.Upload
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
