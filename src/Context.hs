module Context where

import           Hakyll
import           Hakyll.Favicon
import           Text.Blaze     (preEscapedToMarkup)

--------------------------------------------------------------------------------

siteCtx ::  Context String
siteCtx = mconcat
    [ constField "email" "frankhampusweslien@gmail.com"
    , constField "githubUser" "honungsburk"
    , constField "gitlabUser" "HampusWeslien"
    , constField "siteTitle" "FrankHampusWeslien"
    , constField "root" "http://frankhampusweslien.com"
    , constField "description" "FrankHampusWeslien: code and poetry. No more, no less."
    , constField "stylesheet" "/css/basicStyling.css"
    , faviconsField
    ]

baseCtx :: Context String
baseCtx = siteCtx <> defaultContext

dateCtx :: Context String
dateCtx = dateField "date" "%B %e, %Y"

--------------------------------------------------------------------------------

contentCtx :: Tags -> Context String
contentCtx tags = mconcat
    [ dateCtx
    , baseCtx
    , customTagField "tags" tags
    ]

contentWithTeaserCtx :: Tags -> Context String
contentWithTeaserCtx tags = mconcat
    [ teaserField "teaser" "contentSnap"
    , contentCtx tags
    ]

contentsField :: Context String -- ^ context for individual content
              -> [Item String]
              -> Context String
contentsField ctx content =
    listField "contents" ctx (return content)

--------------------------------------------------------------------------------

tagPageCtx :: Tags
           -> [Item String] -- ^ content sharing the same tag
           -> Context String
tagPageCtx tags content = mconcat
    [ contentsField (contentCtx tags) content
    , baseCtx
    ]

projectCtx :: Tags -> Context String
projectCtx tags =
               customTagField "tags" tags
            <> teaserField "teaser" "contentSnap"
            <> defaultContext
            <> siteCtx

--------------------------------------------------------------------------------

indexCtx :: Context String
indexCtx = constField "title" "Home" <> baseCtx

indexContentCtx :: String -- ^ identifier for the context
                -> Context String -- ^ the context for each content
                -> [Item String] -- ^ Content
                ->  Context String
indexContentCtx id ctx content = mconcat
    [ trueField id
    , contentsField ctx content
    , indexCtx
    ]

indexBlogCtx :: Tags
             -> [Item String] -- ^ Blog items
             -> Context String
indexBlogCtx tags = indexContentCtx "indexBlog" $ contentWithTeaserCtx tags

indexWritingCtx :: Tags
                -> [Item String] -- ^ Writing items
                -> Context String
indexWritingCtx tags = indexContentCtx "indexWriting" $ contentWithTeaserCtx tags

indexProjectsCtx :: Tags
              -> [Item String] -- ^ Project items
              -> Context String
indexProjectsCtx tags = indexContentCtx "indexProjects" $ contentCtx tags

indexArtCtx :: Tags
            -> Context String -- ^ Paginate context
            -> [Item String] -- ^ Art items
            -> Context String
indexArtCtx tags paginateCtx art = mconcat
    [ constField "title" "Art"
    , paginateCtx
    , indexContentCtx "indexArt" (artCtx tags <> paginateCtx) art
    ]

artCtx :: Tags -> Context String
artCtx tags = mconcat
    [ dateCtx
    , customTagField "tags" tags
    , metadataField
    ]

--------------------------------------------------------------------------------


-- | let you write $if(name)$ in the templates
trueField :: String -- ^ name
          -> Context a
trueField name = boolField name $ const True

-- | Custom tagField generator
--
-- With link:
-- <a class="tag" href="/tags/haskell.html">haskell</a> ... (more tags)
--
-- No link:
-- <a class="tag">haskell</a> ... (more tags)
--
-- Appends '/' to the begining of the link to make it absolut.
-- This will let you have tage pages not in the root that can
-- refrence tags.
customTagField :: String -> Tags -> Context a
customTagField = tagsFieldWith getTags renderTag concatTags
    where
        renderTag tag (Just link) = Just . preEscapedToMarkup $
                "<a class=\"tag\" href=\"/"++ link ++"\">" ++ tag ++ "</a>"
        renderTag tag _ = Just . preEscapedToMarkup $
                "<a class=\"tag\">" ++ tag ++ "</a>"
        concatTags = preEscapedToMarkup

--------------------------------------------------------------------------------

sitemapCtx :: [Item String] -> Context String
sitemapCtx items = mconcat
    [ contentsField (baseCtx <> dateCtx) items
    , baseCtx
    ]

--------------------------------------------------------------------------------

-- | Helper function that lets one turn a list of scripts (or links)
-- to a list of items
scriptsToItem :: [String] -- ^ scripts
              -> [Item String]
scriptsToItem = fmap (\s -> Item (fromFilePath s) s)

-- | Returns a listField of scripts that you use in templates
--
-- Ex:
-- $for(scripts)$
--  $script$
-- $endfor$
--
jsCtx :: [Item String] -- ^ Scripts
      -> Context String
jsCtx scripts = listField "scripts" scriptField $ return scripts
        where
            scriptField = field "script" $ return . itemBody
