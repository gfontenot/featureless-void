module Helper.S3
    ( uploadImage
    ) where

import Import

uploadImage :: Maybe FileInfo -> Handler (Maybe Text)
uploadImage Nothing = return Nothing
uploadImage (Just info) = return . Just $ fileName info
