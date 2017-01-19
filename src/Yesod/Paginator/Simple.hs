module Yesod.Paginator.Simple
    ( simplePaginationWidget
    ) where

import Import
import Yesod.Paginator
import qualified Data.Text as T

simplePaginationWidget :: PageWidget App
simplePaginationWidget page per tot = do
    -- total number of pages
    let pages = (\(n, r) -> n + (min r 1)) $ tot `divMod` per
    curParams <- handlerToWidget $ liftM reqGetParams getRequest

    [whamlet|$newline never
        <ul class="pagination">
            $forall link <- buildLinks page pages
                ^{showLink curParams link}
    |]

    where
        buildLinks :: Int -> Int -> [PageLink]
        buildLinks pg pgs = concat
            [ PageLink (pg - 1) "Newer" <$ (guard $ pg /= 1)
            , PageLink (pg + 1) "Older" <$ (guard $ pg /= pgs)
            ]

data PageLink = PageLink Int Text

showLink :: [(Text, Text)] -> PageLink -> Widget
showLink params (PageLink pg txt) = do
    let param = ("p", showT pg)

    [whamlet|$newline never
        <li>
            <a href="#{updateGetParam params param}">#{txt}
    |]

    where
        updateGetParam :: [(Text,Text)] -> (Text,Text) -> Text
        updateGetParam getParams (p, n) = (T.cons '?') . T.intercalate "&"
                                        . map (\(k,v) -> k `T.append` "=" `T.append` v)
                                        . (++ [(p, n)]) . filter ((/= p) . fst) $ getParams

showT :: (Show a) => a -> Text
showT = T.pack . show
