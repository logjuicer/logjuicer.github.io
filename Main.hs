{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid (mappend)
import Hakyll
import Text.Pandoc
import Text.Pandoc.Walk

-- | Update section with links and adjust level to be lower than the title
addSectionLinks :: Pandoc -> Pandoc
addSectionLinks = walk f
  where
    f (Header n attr@(idAttr, _, _) inlines)
      | n == 2 =
        let link = Link ("", ["anchor"], []) [Str "§"] ("#" <> idAttr, "")
         in Header (n + 1) attr ([link, Space] <> inlines)
    f x = x

customCompiler :: Compiler (Item String)
customCompiler =
  pandocCompilerWithTransform defaultHakyllReaderOptions defaultHakyllWriterOptions addSectionLinks

pageCtx :: String -> Context String
pageCtx usage =
    dateField "date" "%B %e, %Y" `mappend`
    constField "usage" usage `mappend`
    defaultContext

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll do
  match "templates/*" $ compile templateCompiler
  match "generated/*.txt" do
    compile getResourceBody
  match "css/*" $ do
    route idRoute
    compile compressCssCompiler
  match "generated/doc.md" do
    route $ constRoute "index.html"
    compile $
      customCompiler
        >>= loadAndApplyTemplate "templates/default.html" (pageCtx "--help")
        >>= relativizeUrls

  -- create ["index.html"] do
  --   route idRoute
  --   compile do
  --     doc <- load "generated/doc.md"
  --     customCompiler
  --       >>= loadAndApplyTemplate "templates/default.html" (pageCtx itemBody doc)
  --       >>= relativizeUrls
