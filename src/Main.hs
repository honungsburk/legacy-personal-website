{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Hakyll
import           Rules

--------------------------------------------------------------------------------

-- | the ordering doens't matter, Hakyll figures out the dependencies by itself.
main :: IO ()
main = hakyll $ do
    tags <- buildTags "content/**" (fromCapture "tags/*.html")
    cssRules
    imageRules
    scssRules
    -- content
    blogRules tags
    writingRules tags
    projectRules
    artRules
    -- index pages
    indexRules
    indexBlogRules tags
    indexWritingRules tags
    indexProjectsRules tags
    artPortfolio tags
    -- tags
    tagPagesRules tags
    templateRules
    -- site information
    staticCopyRule
    sitemapRule
    atomRule
    favIconRule
    -- misc
    aboutRules
    pageNotFoundRules
