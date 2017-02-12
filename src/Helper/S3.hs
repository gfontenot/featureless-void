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

uploadImage :: FileInfo -> Handler Text
uploadImage info = do
    void $ performUpload info
    generateURL info

performUpload :: FileInfo -> Handler CompleteMultipartUploadResponse
performUpload info = runS3 (fileName info) $ \b k -> do
    fileSource info $$ streamUpload $
        set cmuContentType (Just $ fileContentType info) $
            createMultipartUpload b k

generateURL :: FileInfo -> Handler Text
generateURL info = do
    bucketName <- getBucketName
    return $ "http://" <> bucketName <> ".s3.amazonaws.com/" <> fileName info

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
