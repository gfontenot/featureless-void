module Helper.S3
    ( uploadItem
    , uploadItems
    , Upload(..)
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

data Upload = Upload
    { uploadSource :: Source AWS ByteString
    , uploadContentType :: Text
    , uploadFileName :: Text
    }

uploadItems :: [Upload] -> Handler [(Upload, Text)]
uploadItems = mapM uploadItem

uploadItem :: Upload -> Handler (Upload, Text)
uploadItem upload = do
    uuid <- liftIO $ toString <$> nextRandom
    let path = pack $ uuid <.> (fileExtension $ uploadFileName upload)
    void $ performUpload path upload
    url <- generateURL path
    return (upload, url)

performUpload :: Text -> Upload -> Handler CompleteMultipartUploadResponse
performUpload path upload = runS3 path $ \b k -> do
    uploadSource upload $$ streamUpload $
        set cmuContentType (Just $ uploadContentType upload) $
            createMultipartUpload b k

generateURL :: Text -> Handler Text
generateURL path = do
    bucketName <- getBucketName
    return
        $ "https://s3.amazonaws.com/"
        <> bucketName
        <> "/"
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
