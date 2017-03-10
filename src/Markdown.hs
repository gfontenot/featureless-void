module Markdown where

import ClassyPrelude.Yesod
import qualified Yesod.Markdown as Y
import Database.Persist.Sql (PersistFieldSql(..))
import Text.Blaze (ToMarkup (toMarkup))
import Text.Blaze.Renderer.String (renderMarkup)
import Text.Pandoc
import Text.XML.HXT.Core
    ( (//>)
    , (>>.)
    , (>>>)
    , arr
    , getText
    , hread
    , runLA
    )

newtype Markdown = Markdown Y.Markdown
    deriving (Eq, Ord, Show, Read, PersistField, IsString, Monoid)

instance PersistFieldSql Markdown where
    sqlType _ = SqlString

instance ToMarkup Markdown where
    -- | Sanitized by default
    toMarkup = handleError . markdownToHtml

strippedText :: ToMarkup a => a -> Text
strippedText = pack . runLA
    (   arr toHtml
    >>> arr renderMarkup
    >>> hread
    //> getText
    >>. concat
    )

markdown :: Text -> Markdown
markdown = Markdown . Y.Markdown

unMarkdown :: Markdown -> Text
unMarkdown (Markdown m) = Y.unMarkdown m

markdownField :: Monad m => RenderMessage (HandlerSite m) FormMessage => Field m Markdown
markdownField = Field
    { fieldParse = parseHelper $ Right . markdown . filter (/= '\r')
    , fieldView  = \theId name attrs val _isReq -> toWidget
        [hamlet|$newline never
<textarea id="#{theId}" name="#{name}" *{attrs}>#{either id unMarkdown val}
|]
    , fieldEnctype = UrlEncoded
    }

markdownToHtml :: Markdown -> Either PandocError Html
markdownToHtml (Markdown m) = fmap (Y.writePandoc Y.yesodDefaultWriterOptions)
               . Y.parseMarkdown readerOptions $ m

readerOptions :: ReaderOptions
readerOptions = Y.yesodDefaultReaderOptions
    { readerExtensions = githubMarkdownExtensions
    }
