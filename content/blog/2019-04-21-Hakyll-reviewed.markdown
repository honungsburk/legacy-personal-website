---
title:  "Hakyll Reviwed"
date:   2019-04-21 19:51:11 +0100
author: "Frank Hampus Weslien"
tags: 
  - haskell
  - web
  - hakyll
---

During easter I finally found some time to improve upon my website.
Fix the styling, the html templates, and the rest.
But during this "involuntary" break of little more over a year I found myself scratching me head looking through my jekyll project.
*Oh my...* I thought.
And promptly decided to rebuild the entire thing using hakyll instead.
Brilliant, wouldn't you agree?

<!--more-->

## Overview

Now, Hakyll takes a different approach to static site generation comparing to Jekyll.
Jekyll is rather dogmatic, it has its way of doing things: all posts need to be in the folder \_post/ and ...

Hakyll on the otherhand lets you define your own static site generator through its dsl and there for, for better or for worse, lends it self to be customized alot easier. Trading convience for power.

## How fucking loadAll works...

you must compile it at an eariler stage or loadAll doesn't work ???
