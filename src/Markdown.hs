module Markdown where

import ClassyPrelude.Yesod
import qualified Yesod.Markdown as Y
import Database.Persist.Sql (PersistFieldSql(..))
import Text.Blaze (ToMarkup (toMarkup))
import Text.Pandoc

newtype Markdown = Markdown Y.Markdown
    deriving (Eq, Ord, Show, Read, PersistField, IsString, Monoid)

instance PersistFieldSql Markdown where
    sqlType _ = SqlString

instance ToMarkup Markdown where
    -- | Sanitized by default
    toMarkup = handleError . markdownToHtml

toPandoc :: Markdown -> Pandoc
toPandoc m = either mempty id $ parse m
  where
    parse :: Markdown -> Either PandocError Pandoc
    parse = readMarkdown readerOptions . unpack . unMarkdown

plainText :: Markdown -> Text
plainText = pack
            . writePlain plainWriterOptions
            . topDown removeInlineStyles
            . toPandoc

removeInlineStyles :: [Inline] -> [Inline]
removeInlineStyles ((Emph lst) : xs) = removeInlineStyles $ lst ++ xs
removeInlineStyles ((Strong lst) : xs) = removeInlineStyles $ lst ++ xs
removeInlineStyles (x : xs) = x : removeInlineStyles xs
removeInlineStyles [] = []

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

plainWriterOptions :: WriterOptions
plainWriterOptions = def
    { writerExtensions = plainExtensions
    }

readerOptions :: ReaderOptions
readerOptions = Y.yesodDefaultReaderOptions
    { readerExtensions = githubMarkdownExtensions
    }
