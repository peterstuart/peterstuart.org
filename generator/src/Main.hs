{-# LANGUAGE OverloadedStrings #-}

import Data.List (isSuffixOf)
import Hakyll
import System.FilePath
  ( takeBaseName,
    takeDirectory,
    (</>),
  )
import Text.Pandoc.SideNote (usingSideNotes)

config :: Configuration
config =
  defaultConfiguration
    { destinationDirectory = "../dist",
      ignoreFile = const False,
      previewHost = "127.0.0.1",
      previewPort = 8000,
      providerDirectory = "../src",
      storeDirectory = "../hakyll-cache",
      tmpDirectory = "../hakyll-cache/tmp"
    }

main :: IO ()
main = hakyllWith config $ do
  match "images/*" $ do
    route idRoute
    compile copyFileCompiler

  match "fonts/**" $ do
    route idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  match "images/**" $ do
    route idRoute
    compile copyFileCompiler

  match "posts/*" $ do
    route cleanRoute
    compile $
      tuftePandocCompiler
        >>= loadAndApplyTemplate "templates/post.html" postCtx
        >>= saveSnapshot "content"
        >>= applyNavTemplate postCtx
        >>= applyDefaultTemplate postCtx
        >>= cleanUrls

  match (fromList ["error.markdown"]) $ do
    route $ setExtension "html"
    compile $
      pandocCompiler
        >>= applyNavTemplate defaultContext
        >>= applyDefaultTemplate defaultContext
        >>= cleanUrls

  create ["index.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let ctx = listField "posts" postCtx (return posts) <> defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" ctx
        >>= applyDefaultTemplate ctx
        >>= cleanUrls

  create ["rss"] $ do
    route idRoute
    compile $ do
      let feedCtx = postCtx <> bodyField "description"
      posts <- loadAllSnapshots "posts/*" "content" >>= recentFirst
      renderRss feedConfiguration feedCtx posts

  create ["sitemap.xml"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      singlePages <- loadAll (fromList [])
      let pages = posts <> singlePages
          sitemapCtx =
            constField "root" root <> listField "pages" postCtx (return pages)
      makeItem ("" :: String)
        >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

  match "templates/*" $ compile templateBodyCompiler

root :: String
root = "https://www.peterstuart.org"

postCtx :: Context String
postCtx =
  constField "root" root
    <> dateField "w3cDateTime" "%Y-%m-%d"
    <> dateField "date" "%B %e, %Y"
    <> defaultContext

applyDefaultTemplate :: Context a -> Item a -> Compiler (Item String)
applyDefaultTemplate = loadAndApplyTemplate "templates/default.html"

applyNavTemplate :: Context a -> Item a -> Compiler (Item String)
applyNavTemplate = loadAndApplyTemplate "templates/nav.html"

cleanUrls :: Item String -> Compiler (Item String)
cleanUrls item = relativizeUrls item >>= cleanIndexUrls >>= cleanIndexHtmls

-- The following clean* functions are from https://www.rohanjain.in/hakyll-clean-urls/

cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute
  where
    createIndexRoute ident = takeDirectory p </> takeBaseName p </> "index.html"
      where
        p = toFilePath ident

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanIndex)

cleanIndexHtmls :: Item String -> Compiler (Item String)
cleanIndexHtmls = return . fmap (replaceAll indexPattern replacement)
  where
    indexPattern = "/index.html"
    replacement = const "/"

cleanIndex :: String -> String
cleanIndex url
  | idx `isSuffixOf` url = take (length url - length idx) url
  | otherwise = url
  where
    idx = "index.html"

tuftePandocCompiler :: Compiler (Item String)
tuftePandocCompiler =
  pandocCompilerWithTransform
    defaultHakyllReaderOptions
    defaultHakyllWriterOptions
    usingSideNotes

feedConfiguration :: FeedConfiguration
feedConfiguration =
  FeedConfiguration
    { feedTitle = "peterstuart.org",
      feedDescription = "Articles from peterstuart.org",
      feedAuthorName = "Peter Stuart",
      feedAuthorEmail = "peter@peterstuart.org",
      feedRoot = "http://www.peterstuart.org"
    }
