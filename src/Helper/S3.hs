module Helper.S3
    ( uploadImage
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

uploadImage :: FileInfo -> Handler Text
uploadImage info = do
    uuid <- liftIO $ toString <$> nextRandom
    let path = pack $ uuid <.> fileExtension info
    void $ performUpload path info
    generateURL path

performUpload :: Text -> FileInfo -> Handler CompleteMultipartUploadResponse
performUpload path info = runS3 path $ \b k -> do
    fileSource info $$ streamUpload $
        set cmuContentType (Just $ fileContentType info) $
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

fileExtension :: FileInfo -> String
fileExtension = takeExtension . unpack . fileName
