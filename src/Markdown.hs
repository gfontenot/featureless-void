module Markdown
    ( Markdown(..)
    , markdownField
    , plainText
    ) where

import ClassyPrelude.Yesod
import Database.Persist.Sql (PersistFieldSql(..))
import Text.Blaze (ToMarkup (toMarkup))
import Text.HTML.SanitizeXSS (sanitizeBalance)
import Text.Pandoc
    ( Pandoc(..)
    , PandocError(..)
    , ReaderOptions(..)
    , WriterOptions(..)
    , WrapOption(..)
    , Inline(..)
    , readMarkdown
    , writeHtmlString
    , writePlain
    , githubMarkdownExtensions
    , plainExtensions
    , topDown
    )

newtype Markdown = Markdown { unMarkdown :: Text }
    deriving (Eq, Ord, Show, Read, PersistField, IsString, Monoid)

instance PersistFieldSql Markdown where
    sqlType _ = SqlString

instance ToMarkup Markdown where
    -- | Sanitized by default
    toMarkup = preEscapedToMarkup
               . sanitizeBalance
               . pack
               . writeHtmlString htmlWriterOptions
               . toPandoc

plainText :: Markdown -> Text
plainText = pack
            . writePlain plainWriterOptions
            . topDown removeInlineStyles
            . toPandoc

markdownField :: Monad m => RenderMessage (HandlerSite m) FormMessage => Field m Markdown
markdownField = Field
    { fieldParse = parseHelper $ Right . Markdown . filter (/= '\r')
    , fieldView  = \theId name attrs val _isReq -> toWidget
        [hamlet|$newline never
<textarea id="#{theId}" name="#{name}" *{attrs}>#{either id unMarkdown val}
|]
    , fieldEnctype = UrlEncoded
    }

removeInlineStyles :: [Inline] -> [Inline]
removeInlineStyles ((Emph lst) : xs) = removeInlineStyles $ lst ++ xs
removeInlineStyles ((Strong lst) : xs) = removeInlineStyles $ lst ++ xs
removeInlineStyles (x : xs) = x : removeInlineStyles xs
removeInlineStyles [] = []

toPandoc :: Markdown -> Pandoc
toPandoc m = either mempty id $ parse m
  where
    parse :: Markdown -> Either PandocError Pandoc
    parse = readMarkdown readerOptions . unpack . unMarkdown

plainWriterOptions :: WriterOptions
plainWriterOptions = def
    { writerExtensions = plainExtensions
    }

htmlWriterOptions :: WriterOptions
htmlWriterOptions =  def
    { writerHtml5     = True
    , writerWrapText  = WrapNone
    , writerHighlight = True
    }

readerOptions :: ReaderOptions
readerOptions = def
    { readerExtensions = githubMarkdownExtensions
    , readerSmart = True
    , readerParseRaw = True
    }
