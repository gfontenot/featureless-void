module MarkdownSpec (spec) where

import           Markdown
import           TestImport

sampleWrappingScream :: Markdown
sampleWrappingScream = Markdown "Action Bronson hanging out on the side of the stage eating chicken wings during Meyhem Lauren’s set is everything I could have hoped for"

sampleMultilineScream :: Markdown
sampleMultilineScream = Markdown "this is a test\n\nwith multilines"

sampleHtmlScream :: Markdown
sampleHtmlScream = Markdown "this is a scream [with a link](http://example.com)"

spec :: Spec
spec =
    describe "plainText" $
        it "extracts the plain text version of markdown" $ do
            plainText sampleWrappingScream `shouldBe` unMarkdown sampleWrappingScream
            plainText sampleMultilineScream `shouldBe` unMarkdown sampleMultilineScream
            plainText sampleHtmlScream `shouldBe` "this is a scream with a link"
