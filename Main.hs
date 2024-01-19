{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text (pack)
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

addUsage :: String -> Pandoc -> Pandoc
addUsage usage (Pandoc meta blocks) = Pandoc meta (concatMap f blocks)
  where
    f x@(Header _ ("configure", _, _) _) = [CodeBlock ("", ["shellsession"], []) (pack usage), x]
    f x = [x]

customCompiler :: String -> Compiler (Item String)
customCompiler usage =
  pandocCompilerWithTransform defaultHakyllReaderOptions defaultHakyllWriterOptions (addSectionLinks . addUsage usage)

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
    compile do
      usage <- itemBody <$> load "generated/cli-help.txt"
      customCompiler usage
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

  -- create ["index.html"] do
  --   route idRoute
  --   compile do
  --     doc <- load "generated/doc.md"
  --     customCompiler
  --       >>= loadAndApplyTemplate "templates/default.html" (pageCtx itemBody doc)
  --       >>= relativizeUrls
