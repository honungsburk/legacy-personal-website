---
title:  "Hakyll Reviewed"
date:   2019-04-21 19:51:11 +0100
author: "Frank Hampus Weslien"
tags: 
  - haskell
  - web
  - hakyll
---

During Easter I finally found some time to finish my website: fix the styling, html templates, and all the rest.
But during my "involuntary" break of little more over a year I found myself scratching me head looking through my Jekyll project.
*Oh my...* I thought to myself.
And seeing as I've been toying around with Haskell I promptly decided to rebuild the entire thing using Hakyll instead.
Brilliant.

<!--more-->

Now, Hakyll takes a different approach to static site generation compared to something like Jekyll.
Instead of being a ready made tool it's a library that lets you build own generator.
Of course, that means that it is a bit more complex.
Trading convince for power.

Like Jekyll you have templates, CSS, and your posts or other content you wish to display.
Just like normal.
But you also have code to define the behavior of your static site generator.
You have a main function like so

``` haskell
main :: IO ()
main = hakyll $ do
    cssRules
    tagPagesRules
    ...
```

after which you define a set of rules.
Rules can be arbitrarily complex but are often very simple, and thanks to Haskell being so terse you often don't need many lines of code.
Here are two examples.

``` haskell

-- | A simple rule that copies all .css files in 
--   the css folder and compress them
cssRules :: Rules ()
cssRules = match "css/*.css" $ do
    route   idRoute
    compile compressCssCompiler

-- | And here an example of a more complex rule 
--   that generates tag pages.
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

tagPageCtx :: Tags
           -> [Item String] -- ^ content sharing the same tag
           -> Context String
tagPagesCtx = ...
```

That said, if you want a highly customized website you might have to write a bit more.
In fact this [website](http://frankhampusweslien.com) is 322 loc. 
Which, when I come to think about it, might be a few more lines then I would like.
Luckily however the code you write when defining your rules usually isn't to complex and having such direct access to the internals lends Hakyll to very easily be modified.
Adding an extension is trivial.
One example would be supporting Sass, seeing as Hakyll itself doesn't ship with it.
On Hackage we can find the convenient hakyll-sass library that defines functions to do what we want.

``` Haskell
import Hakyll.Web.Sass (sassCompiler)

scssRules :: Rules ()
scssRules = match "css/*.scss" $ do
        route $ setExtension "css"
        let compressCssItem = fmap compressCss
        compile (compressCssItem <$> sassCompiler)

```
And we are done.

However, for all its brilliance Hakyll do fall short in one area: documentation.
Simply put, there isn't that much of it. 
There is enough but sometimes you stumble upon a piece of undocumented code leaving you to scratch your head.
The function bellow was used to generate the html of the tags scattered around the website.

``` haskell
tagsFieldWith :: (Identifier -> Compiler [String])
              -> (String -> (Maybe FilePath) -> Maybe Html) 
              -> ([Html] -> Html) 
              -> String 
              -> Tags
              -> Context a
```

After staring at the types a bit, and finally having the compiler cease its complaining I came up with this.

``` haskell
tagsFieldWith :: (Identifier -> Compiler [String])            
              -- ^ Function to get the tags
              -> (String -> (Maybe FilePath) -> Maybe Html)   
              -- ^ Function to generate the html of one tag
              -> ([Html] -> Html)                             
              -- ^ Function to concatenate tags to a larger html
              -> String                                       
              -- ^ Name of the field 
              -> Tags
              -> Context a
```

So thankfully Haskell's expressive type system comes to the rescue.
But documentation is invaluable and you should expect to have some of your time wasted when it's missing. 

All in all I found Hakyll to be a delightful tool to build my website and if your a Haskell enthusiast I'd recommend it whole heartedly. 


