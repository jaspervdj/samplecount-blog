{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative ((<$>))
import           Data.Char
import           Data.Monoid         ((<>), mappend)
import           Hakyll

myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
  { feedTitle       = "Samplecount Blog"
  , feedDescription = "Posts about sound, programming and sample counts."
  , feedAuthorName  = "Samplecount"
  , feedAuthorEmail = "info@samplecount.com"
  , feedRoot        = "http://blog.samplecount.com/blog"
  }

postCtx :: Context String
postCtx =
       dateField "date" "%B %e, %Y"
    <> dateField "postId" "%Y%m%d"
    <> urlField  "postUrl"
    <> defaultContext

postList :: Compiler String
postList = do
  posts <- recentFirst <$> loadAllSnapshots "posts/*" "post"
  templ <- loadBody "templates/post.html"
  list  <- applyTemplateList templ postCtx posts
  return list

postItemList :: Compiler String
postItemList = do
  posts   <- recentFirst <$> loadAll ("posts/*" .&&. hasNoVersion)
  itemTpl <- loadBody "templates/post-item.html"
  list    <- applyTemplateList itemTpl postCtx posts
  return list

blogConfig :: Configuration
blogConfig = defaultConfiguration {
    deployCommand = "rsync -av --delete _site/ hearhear.me:html/blog.samplecount.com/"
  }

-- blogRoute = customRoute $ ("blog/"++) . toFilePath
blogRoute = idRoute

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

  match "posts/*" $ do
    route $ blogRoute `composeRoutes` setExtension "html"
    compile $ pandocCompiler
      >>= saveSnapshot "post"
      >>= loadAndApplyTemplate "templates/post-page.html" postCtx
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= relativizeUrls

  create ["archive/index.html"] $ do
    route blogRoute
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
    route blogRoute
    compile $ do
      let feedCtx = postCtx `mappend` bodyField "description"
      posts <- loadAll ("posts/*" .&&. hasVersion "post")
      renderAtom myFeedConfiguration feedCtx posts

  match "index.html" $ do
      route blogRoute
      compile $ do
          -- let indexCtx = field "posts" $ \_ -> postList (take 3 . recentFirst)
          let indexCtx = field "posts" (const $ postList) <> defaultContext
          getResourceBody
              >>= applyAsTemplate indexCtx
              >>= loadAndApplyTemplate "templates/default.html" indexCtx
              >>= relativizeUrls

  -- -- Redirect "/" to "/blog"
  -- create [".htaccess"] $ do
  --   route idRoute
  --   compile $ makeItem "" >>= loadAndApplyTemplate "templates/htaccess" defaultContext

  match "templates/*" $ compile templateCompiler
