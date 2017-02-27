module Query
    ( fetchImagesForScream
    , fetchImagesForScreams
    , paginatedScreams
    , fetch404
    , joinOneToMany
    , screamImage
    ) where

import Yesod.Paginator
import Database.Persist.Sql
    ( fromSqlKey
    )
import Import hiding
    ( for
    , belongsTo
    )

screamImage :: Image -> Key Scream
screamImage = imageScreamId

paginatedScreams :: Int
                 -> PageWidget App
                 -> ReaderT SqlBackend
                    Handler
                    ([Entity Scream], Widget)
paginatedScreams c widget =
    selectPaginatedWith
    widget                                -- pagination widget
    c                                     -- number of items per page
    []                                    -- filters
    [Desc ScreamCreatedAt, Desc ScreamId] -- sort descriptors

fetch404 :: ( PersistEntityBackend record ~ BaseBackend backend
            , PersistEntity record
            , PersistStoreWrite backend
            , MonadIO m
            ) => Key record -> ReaderT backend m (Entity record)
fetch404 oid = do
    obj <- get404 oid
    return $ Entity oid obj

fetchImagesForScreams :: ( BaseBackend backend ~ SqlBackend
                         , MonadIO m
                         , PersistQueryRead backend
                         ) => [Entity Scream] -> ReaderT backend m [Entity Image]
fetchImagesForScreams screams =
    selectList
    [ImageScreamId <-. map entityKey screams]
    []

fetchImagesForScream :: ( BaseBackend backend ~ SqlBackend
                        , MonadIO m
                        , PersistQueryRead backend
                        ) => Entity Scream -> ReaderT backend m [Entity Image]
fetchImagesForScream scream =
    selectList
    [ImageScreamId ==. entityKey scream]
    []

joinOneToMany :: (ToBackendKey SqlBackend a)
              => (b -> Key a)
              -> [Entity b]
              -> Entity a
              -> (Entity a, [Entity b])
joinOneToMany f bs a = (a, filter isRelation bs)
  where
    isRelation b = (fromSqlKey $ relationKey b) == (fromSqlKey $ entityKey a)
    relationKey = f . entityVal
