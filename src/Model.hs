{-# LANGUAGE FlexibleInstances #-}

module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi
import Yesod.Auth.HashDB (HashDBUser(..))
import Markdown

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

instance HashDBUser User where
    userPasswordHash = userPassword
    setPasswordHash h u = u { userPassword = Just h }

type PopulatedScream = (Entity Scream, [Entity Image])

populatedScreamId :: PopulatedScream -> ScreamId
populatedScreamId = entityKey . fst

populatedScreamBody :: PopulatedScream -> Markdown
populatedScreamBody = screamBody . entityVal . fst

populatedScreamCreatedAt :: PopulatedScream -> UTCTime
populatedScreamCreatedAt = screamCreatedAt . entityVal . fst

populatedScreamImages :: PopulatedScream -> [Image]
populatedScreamImages = (map entityVal) . snd
