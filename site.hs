{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative ((<$>))
import           Data.Char
import           Data.Monoid         ((<>), mappend)
import           Hakyll

blogRoot = "http://blog.samplecount.com"

myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
  { feedTitle       = "Samplecount Blog"
  , feedDescription = "This is the feed for Samplecount's company blog - Posts about sound and programming"
  , feedAuthorName  = "Samplecount"
  , feedAuthorEmail = "info@samplecount.com"
  , feedRoot        = blogRoot
  }

matchPosts template tag =
  match "posts/*" $ version tag $ do
    route $ setExtension "html"
    -- let localCtx = postCtx `mappend` field "postid" (\_ -> concatMap (show.ord) . show <$> getUnderlying)
    compile $ pandocCompiler
      >>= loadAndApplyTemplate template postCtx
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= relativizeUrls

postCtx :: Context String
postCtx =
       dateField "date" "%B %e, %Y"
    <> dateField "postid" "%Y%m%d%H%M%S"
    <> defaultContext

postList :: Compiler [Item String]
postList = recentFirst <$> loadAllSnapshots "posts/*" "content"

postItemList :: Compiler String
postItemList = do
  posts   <- recentFirst <$> loadAll "posts/*"
  itemTpl <- loadBody "templates/post-item.html"
  list    <- applyTemplateList itemTpl postCtx posts
  return list

blogConfig :: Configuration
blogConfig = defaultConfiguration {
    deployCommand = "rsync -av --delete _site/ hearhear.me:html/blog.samplecount.com/"
  }

main :: IO ()
main = hakyllWith blogConfig $ do
  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  match "images/*" $ do
    route idRoute
    compile copyFileCompiler

  match "js/**" $ do
    route idRoute
    compile copyFileCompiler

  -- match (fromList ["about.rst", "contact.markdown"]) $ do
  --     route   $ setExtension "html"
  --     compile $ pandocCompiler
  --         >>= loadAndApplyTemplate "templates/default.html" defaultContext
  --         >>= relativizeUrls

  match "posts/*" $ do
    route $ setExtension "html"
    -- let localCtx = postCtx `mappend` field "postid" (\_ -> concatMap (show.ord) . show <$> getUnderlying)
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/post.html" postCtx
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= relativizeUrls

  create ["archive/index.html"] $ do
    route idRoute
    compile $ do
      let archiveCtx =
               field "post-items" (const $ postItemList)
            <> constField "title" "Archives"
            <> defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls

  create ["atom.xml"] $ do
    route idRoute
    compile $ do
      let feedCtx = postCtx `mappend` bodyField "description"
      posts <- take 10 <$> postList
      renderAtom myFeedConfiguration feedCtx posts

  match "index.html" $ do
      route idRoute
      compile $ do
          -- let indexCtx = field "posts" $ \_ -> postList (take 3 . recentFirst)
          let indexCtx = field "posts" $ const $ concatMap itemBody . take 10 <$> postList
          getResourceBody
              >>= applyAsTemplate indexCtx
              >>= loadAndApplyTemplate "templates/default.html" postCtx
              >>= relativizeUrls

  match "templates/*" $ compile templateCompiler
