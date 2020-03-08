{-# LANGUAGE OverloadedStrings #-}

import           Data.List                      ( isSuffixOf )
import           Data.Monoid                    ( mappend )
import           Hakyll
import           System.FilePath                ( (</>)
                                                , takeDirectory
                                                , takeBaseName
                                                )
import           Text.Pandoc.SideNote           ( usingSideNotes )

main :: IO ()
main = hakyll $ do
  match "images/*" $ do
    route idRoute
    compile copyFileCompiler

  match "fonts/*/*/*" $ do
    route idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  match "posts/*" $ do
    route cleanRoute
    compile
      $   tuftePandocCompiler
      >>= loadAndApplyTemplate "templates/post.html" postCtx
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= relativizeUrls
      >>= cleanIndexUrls
      >>= cleanIndexHtmls

  create ["index.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let ctx =
            listField "posts" postCtx (return posts) `mappend` defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls
        >>= cleanIndexUrls
        >>= cleanIndexHtmls

  create ["rss"] $ do
    route idRoute
    compile $ do
      let feedCtx = postCtx `mappend` bodyField "description"
      posts <- loadAllSnapshots "posts/*" "content" >>= recentFirst
      renderRss feedConfiguration feedCtx posts

  match "templates/*" $ compile templateBodyCompiler

postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" `mappend` defaultContext

-- The following clean* functions are from https://www.rohanjain.in/hakyll-clean-urls/

cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute
 where
  createIndexRoute ident = takeDirectory p </> takeBaseName p </> "index.html"
    where p = toFilePath ident

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanIndex)

cleanIndexHtmls :: Item String -> Compiler (Item String)
cleanIndexHtmls = return . fmap (replaceAll pat replacement)
 where
  pat         = "/index.html"
  replacement = const "/"

cleanIndex :: String -> String
cleanIndex url | idx `isSuffixOf` url = take (length url - length idx) url
               | otherwise            = url
  where idx = "index.html"

tuftePandocCompiler :: Compiler (Item String)
tuftePandocCompiler = pandocCompilerWithTransform defaultHakyllReaderOptions
                                                  defaultHakyllWriterOptions
                                                  usingSideNotes

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
  { feedTitle       = "peterstuart.org"
  , feedDescription = "Articles from peterstuart.org"
  , feedAuthorName  = "Peter Stuart"
  , feedAuthorEmail = "peter@peterstuart.org"
  , feedRoot        = "http://www.peterstuart.org"
  }
