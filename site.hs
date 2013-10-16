{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend, mconcat)
import           Hakyll
import           Data.Maybe(fromMaybe)
import qualified Data.Map as M
import           Text.Pandoc(WriterOptions, HTMLMathMethod(..), writerHTMLMathMethod)

main ::
  IO ()
main =
  hakyllWith configuration $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    -- Build tags
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    match (fromList ["contact.markdown", "404.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" (postCtx tags)
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompilerWith defaultHakyllReaderOptions pandocOptions
            >>= saveSnapshot "content"
            -- >>= return . fmap demoteHeaders
            >>= loadAndApplyTemplate "templates/post.html"    (postCtx tags)
            >>= loadAndApplyTemplate "templates/disqus.html"  (postCtx tags)
            >>= loadAndApplyTemplate "templates/default.html" (postCtx tags)
            >>= relativizeUrls

    create ["posts.html"] $ do
        route idRoute
        compile $ do
            list <- postList tags "posts/*" recentFirst
            let archiveCtx =
                    constField "posts" list                    `mappend`
                    constField "title" "Posts"                 `mappend`
                    postCtx tags

            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html"   archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    -- Post tags
    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged " ++ tag

        -- Copied from posts, need to refactor
        route idRoute
        compile $ do
            list <- postList tags pattern recentFirst
            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html"
                        (constField "title" title   `mappend`
                            constField "posts" list `mappend`
                            constField "tag" tag `mappend`
                            field "tags" (\_ -> renderTagList tags) `mappend`
                            defaultCtx)
                >>= loadAndApplyTemplate "templates/default.html" defaultCtx
                >>= relativizeUrls
        version "atom" $ do
            route $ setExtension "xml"
            compile $ loadAllSnapshots pattern "content"
                >>= fmap (take 10) . recentFirst
                >>= renderAtom (feedConfiguration title) feedCtx

    match "index.html" $ do
        route idRoute
        compile $ do
            list <- postList tags "posts/*" $ fmap (take 5) . recentFirst
            let indexCtx = constField "posts" list          `mappend`
                    field "tags" (\_ -> renderTagList tags) `mappend`
                    defaultCtx

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" defaultCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

    -- Render RSS feed
    create ["atom.xml"] $ do
        route idRoute
        compile $
            loadAllSnapshots "posts/*" "content"
                >>= recentFirst
                >>= renderAtom (feedConfiguration "All posts") feedCtx
    
colourField ::
  String
  -> String
  -> Context a
colourField name defaultC = field name $ \i -> do
  metadata <- getMetadata (itemIdentifier i)
  return . fromMaybe defaultC . M.lookup "colour" $ metadata

defaultColour ::
  String
defaultColour =
  "light cyan"

postCtx ::
  Tags
  -> Context String
postCtx tags =
  mconcat
    [ 
      tagsField  "tags"  tags
    , dateField  "date"  "%B %e, %Y"
    , defaultCtx
    ]

defaultCtx ::
  Context String
defaultCtx =
   colourField "colour" defaultColour `mappend` defaultContext

postList ::
  Tags
  -> Pattern
  -> ([Item String] -> Compiler [Item String])
  -> Compiler String
postList tags pattern sortFilter = do
  posts   <- sortFilter =<< loadAll pattern
  itemTpl <- loadBody "templates/post-item.html"
  applyTemplateList itemTpl (postCtx tags) posts

feedCtx ::
  Context String
feedCtx =
  mconcat
    [ 
      bodyField "description"
    , defaultContext
    ]

feedConfiguration ::
  String
  -> FeedConfiguration
feedConfiguration title =
  FeedConfiguration
    { 
      feedTitle       = "Ride 2014 Planning — " ++ title
    , feedDescription = "Planning for the Trans-Australia Ride of 2014"
    , feedAuthorName  = "Tony Morris"
    , feedAuthorEmail = "ride@tmorris.net"
    , feedRoot        = "http://ride.tmorris.net"
    }

pandocOptions ::
  WriterOptions
pandocOptions =
  defaultHakyllWriterOptions {
    writerHTMLMathMethod = MathJax ""
  }

configuration ::
  Configuration
configuration =
  defaultConfiguration { deployCommand = "cp -r _site/* ../" }
