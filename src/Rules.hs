{-# LANGUAGE OverloadedStrings #-}
module Rules where

import           Context
import qualified Data.ByteString.Lazy.Char8 as C
import           Hakyll
import           Hakyll.Favicon
import           Hakyll.Web.Sass            (sassCompiler)
import           Text.Jasmine

--------------------------------------------------------------------------------

-- todo: optimize svg, images etc...
imageRules :: Rules ()
imageRules = match "images/*" $ do
    route   idRoute
    compile copyFileCompiler

-- | You should only have one .ico file in images
favIconRule :: Rules ()
favIconRule = faviconsRules "images/TheCrimsonKingLogo.svg"

--------------------------------------------------------------------------------

jsRules :: Rules ()
jsRules = match "assets/js/*" $ do
    route   idRoute
    compile compressJsCompiler

-- | Create a JavaScript compiler that minifies the content
compressJsCompiler :: Compiler (Item String)
compressJsCompiler = do
  let minifyJS = C.unpack . minify . C.pack . itemBody
  s <- getResourceString
  return $ itemSetBody (minifyJS s) s

--------------------------------------------------------------------------------

cssRules :: Rules ()
cssRules = match "css/*.css" $ do
    route   idRoute
    compile compressCssCompiler

scssRules :: Rules ()
scssRules = match "css/*.scss" $ do
        route $ setExtension "css"
        let compressCssItem = fmap compressCss
        compile (compressCssItem <$> sassCompiler)

--------------------------------------------------------------------------------

-- | Helper functions to apply a bunch of templates and then relativize the urls
-- used by almost every rule.
loadAndApplyTemplates :: Item String   --- ^ the base item
                      -> Context String -- ^ context to be applied to all templates
                      -> [Identifier] -- ^ template identifiers
                      -> Compiler (Item String)
loadAndApplyTemplates item ctx ids = do
    final <- foldl (>>=) (return item) . fmap (\id -> loadAndApplyTemplate id ctx ) $ ids
    relativizeUrls final

--------------------------------------------------------------------------------

contentWritingRules :: Pattern -> Tags -> Rules ()
contentWritingRules pattern tags = match pattern $ do
    route $ setExtension "html"
    compile $ pandocCompiler
        >>= saveSnapshot "contentSnap"
        >>= loadAndApplyTemplate "templates/contentWriting.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls
            where
                ctx = contentWithTeaserCtx tags

blogRules :: Tags -> Rules ()
blogRules = contentWritingRules "content/blog/*"

writingRules :: Tags -> Rules ()
writingRules = contentWritingRules "content/writing/**"

--------------------------------------------------------------------------------

-- | basic rule to load all templates for other rules to use
templateRules :: Rules ()
templateRules = match "templates/*" $ compile templateCompiler

-- | This rule copy static files that doesn't need to be generated
staticCopyRule :: Rules ()
staticCopyRule = match (fromList ["robots.txt"]) $ do
    route idRoute
    compile $ getResourceBody >>= relativizeUrls

--------------------------------------------------------------------------------

aboutRules :: Rules ()
aboutRules = matchPandocRule "about.markdown" baseCtx
    [ "templates/about.html"
    , "templates/default.html"
    ]

pageNotFoundRules :: Rules ()
pageNotFoundRules = matchPandocRule "404.html" baseCtx
    [ "templates/contentWriting.html"
    , "templates/default.html"
    ]


matchPandocRule :: Pattern -- ^ pattern of what is to be created
          -> Context String -- ^ context for the templates
          -> [Identifier] -- ^ list of template identifiers
          -> Rules ()
matchPandocRule pattern ctx templates = match pattern $ do
    route $ setExtension "html"
    compile $ do
        baseItem <- pandocCompiler
        loadAndApplyTemplates baseItem ctx templates

--------------------------------------------------------------------------------

createRules :: Identifier -> Compiler (Item String) -> Rules ()
createRules id co = create [id] $ do
    route idRoute
    compile co

indexRules :: Rules ()
indexRules = createRules "index.html" $ do
        baseItem <- makeItem ""
        loadAndApplyTemplates baseItem indexCtx
           [ "templates/default.html"
           ]

--------------------------------------------------------------------------------

indexTextRules :: Identifier -- where to save the file
               -> Pattern -- ^ where to load content
               -> (Tags -> [Item String] -> Context String) -- create context
               -> Tags -> Rules ()
indexTextRules id pattern getCtx tags = createRules id $ do
        textContent <- recentFirst =<< loadAll pattern
        let ctx = getCtx tags textContent
        item <- makeItem ""
        loadAndApplyTemplates item ctx
            [ "templates/postList.html"
            , "templates/indexText.html"
            , "templates/default.html"
            ]

indexBlogRules :: Tags -> Rules ()
indexBlogRules =
    indexTextRules "indexBlog.html" "content/blog/*" indexBlogCtx


indexWritingRules :: Tags -> Rules ()
indexWritingRules =
     indexTextRules "indexWriting.html" "content/writing/**" indexWritingCtx

--------------------------------------------------------------------------------

-- | This is done so that indexProjectRules will be able to read the files later
projectRules :: Rules ()
projectRules = match "content/projects/*" $ do
    compile $ pandocCompiler
        >>= saveSnapshot "contentSnap"
        >>= relativizeUrls

indexProjectsRules :: Tags -> Rules ()
indexProjectsRules tags = createRules "indexProjects.html" $ do
        projects <- recentFirst =<< loadAll "content/projects/*"
        let ctx = indexProjectsCtx tags projects
        item <- makeItem ""
        loadAndApplyTemplates item ctx
            [ "templates/indexProjects.html"
            , "templates/default.html"
            ]

--------------------------------------------------------------------------------

-- This doesn't do anything but instantiate all the files in art/*
-- so that artPortfolio have access to them later
artRules :: Rules ()
artRules = match "content/art/*" $
    compile (getResourceBody >>= saveSnapshot "contentSnap")

-- art portfolio
artPortfolio :: Tags -> Rules ()
artPortfolio tags = do
    artPortfolio <- buildPaginateWith
        (\ids -> sortRecentFirst ids >>= return . paginateEvery 5)
        "content/art/*"
        (\n -> if n == 1
            then "indexArt.html"
            else fromCapture "content/art/*.html" (show n))
    paginateRules artPortfolio $ \pageNum pattern -> do
        route idRoute
        compile $ do
            art <- recentFirst =<< loadAll pattern  -- Should be just one
            let paginateCtx = paginateContext artPortfolio pageNum
            let ctx         = indexArtCtx tags paginateCtx art
            item <- makeItem ""
            loadAndApplyTemplates item ctx
                [ "templates/indexArt.html"
                , "templates/default.html"
                ]

--------------------------------------------------------------------------------

tagPagesRules :: Tags -> Rules ()
tagPagesRules tags = tagsRules tags $ \tag pattern -> do
    let title = "\"" ++ tag ++ "\""
    route idRoute
    compile $ do
        content <- recentFirst =<< loadAll pattern
        let ctx = constField "title" title
                <> tagPageCtx tags content
        item <- makeItem ""
        loadAndApplyTemplates item ctx
            [ "templates/tagPage.html"
            , "templates/default.html"
            ]


--------------------------------------------------------------------------------

atomRule :: Rules ()
atomRule = createRules "atom.xml" $ feed renderAtom

-- Not currently in use
rssRule :: Rules ()
rssRule = createRules "rss.xml" $ feed renderRss

feed :: (FeedConfiguration -> Context String -> [Item String] -> Compiler (Item String))
     -> Compiler (Item String)
feed feedRender =
    loadAllSnapshots "content/**" "contentSnap"
        >>= fmap (take 10) . recentFirst
        >>= feedRender (feedConfiguration "All content") feedCtx

feedConfiguration :: String -> FeedConfiguration
feedConfiguration title = FeedConfiguration
    { feedTitle       = "frankhampusweslien - " ++ title
    , feedDescription = "Personal website of Frank Hampus Weslien"
    , feedAuthorName  = "Frank Hampus Weslien"
    , feedAuthorEmail = "frankhampusweslien@gmail.com"
    , feedRoot        = "http://frankhampusweslien.com"
    }

feedCtx :: Context String
feedCtx = mconcat
    [ bodyField "description"
    , baseCtx
    ]

--------------------------------------------------------------------------------

-- | index.html is not included as it has it is defined directly in the sitemap
-- with a higher priority
singlePages :: [Identifier]
singlePages =
    [ "about.html"
    , "indexArt"
    , "indexBlog"
    , "indexWriting"
    , "indexProjects"
    ]

-- |
-- The urls are not absolut to amke the site easier to crawl
sitemapRule :: Rules ()
sitemapRule = createRules "sitemap.xml" $ do
    content <- recentFirst =<< loadAll "content/**"
    pages <- loadAll $ fromList singlePages
    let ctx =  sitemapCtx $ content <> pages
    makeItem ""
        >>= loadAndApplyTemplate "templates/sitemap.xml" ctx
