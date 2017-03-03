module Helper.S3
    ( uploadItem
    , uploadItems
    , UploadDescription(..)
    ) where

import Import

import Control.Lens (set)
import Network.AWS
    ( AWS
    , runAWS
    , newEnv
    , Credentials(..)
    )
import Network.AWS.S3
    ( BucketName(..)
    , ObjectKey(..)
    , CompleteMultipartUploadResponse
    , cmuContentType
    )
import Network.AWS.S3.StreamingUpload
    ( streamUpload
    , createMultipartUpload
    )
import Data.UUID
    ( toString
    )
import Data.UUID.V4
    ( nextRandom
    )
import System.FilePath.Posix
    ( takeExtension
    )

data UploadDescription = UploadDescription
    { uploadSource :: Source AWS ByteString
    , uploadContentType :: Text
    , uploadFileName :: Text
    }

uploadItems :: [UploadDescription] -> Handler [(UploadDescription, Text)]
uploadItems = mapM uploadItem

uploadItem :: UploadDescription -> Handler (UploadDescription, Text)
uploadItem desc = do
    uuid <- liftIO $ toString <$> nextRandom
    let path = pack $ uuid <.> (fileExtension $ uploadFileName desc)
    void $ performUpload path desc
    url <- generateURL path
    return (desc, url)

performUpload :: Text -> UploadDescription -> Handler CompleteMultipartUploadResponse
performUpload path desc = runS3 path $ \b k -> do
    uploadSource desc $$ streamUpload $
        set cmuContentType (Just $ uploadContentType desc) $
            createMultipartUpload b k

generateURL :: Text -> Handler Text
generateURL path = do
    bucketName <- getBucketName
    return
        $ "http://"
        <> bucketName
        <> ".s3.amazonaws.com/"
        <> path

getBucketName :: Handler Text
getBucketName = do
    app <- getYesod
    return $ appS3Bucket $ appSettings app

runS3 :: Text -> (BucketName -> ObjectKey -> AWS a) -> Handler a
runS3 identifier f = do
    e <- newEnv Discover
    bucketName <- getBucketName

    let b = BucketName bucketName
        k = ObjectKey identifier

    runResourceT $ runAWS e $ f b k

fileExtension :: Text -> String
fileExtension = takeExtension . unpack
