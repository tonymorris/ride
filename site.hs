{-
Copyright (C) 2012 John Lenz <lenz@math.uic.edu>
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this list
of conditions and the following disclaimer.  Redistributions in binary form must
reproduce the above copyright notice, this list of conditions and the following
disclaimer in the documentation and/or other materials provided with the
distribution.  Neither the name of John Lenz nor the names of its contributors
may be used to endorse or promote products derived from this software without
specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Char
import Control.Monad (void)
import Control.Arrow ((>>>), arr, second)
import Hakyll
import Text.Pandoc.Shared
import System.FilePath

pandocOptions ::
  WriterOptions
pandocOptions =
  defaultWriterOptions {
    writerHTMLMathMethod = MathJax ""
  , writerLiterateHaskell = True
  }

checkMathOption ::
  Page String
  -> Page String
checkMathOption page =
  setField "mathjax" (case getFieldMaybe "math" page of
                        Nothing -> ""
                        Just _ -> "<script type=\"text/javascript\" src=\"http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML\" />") page

-- | Compile and output each individual post
post ::
  Rules
post =
  match "posts/*" $ do
    route (customRoute fileToDirectory)
    void . compile $ pageCompilerWith defaultHakyllParserState pandocOptions
       >>> arr (\p -> renderDateField "published" "%Y-%m-%dT%H:%M:%SZ" (getField "date" p) p)
       >>> arr (copyField "published" "updated")
       >>> arr (copyBodyToField "feedcontent")
       >>> renderTagsField "prettytags" (fromCapture "tags/*")
       >>> applyTemplateCompiler "templates/post.html"
       >>> arr checkMathOption
       >>> applyTemplateCompiler "templates/default.html"
       >>> relativizeUrlsCompiler

-- | Compile a list of pages to a tag page
makeTagPage ::
  String
  -> [Page String]
  -> Compiler () (Page String)
makeTagPage tag posts =
  constA posts
  >>> listToPageCompiler "templates/post-item.html"
  >>> arr (setField "title" ("Posts tagged &#8216;" ++ tag ++ "&#8217;"))
  >>> arr (setField "tag" tag)
  >>> applyTemplateCompiler "templates/tag.html"
  >>> arr (setField "mathjax" "")
  >>> applyTemplateCompiler "templates/default.html"
  >>> relativizeUrlsCompiler

-- | Compile a list of pages to a tag feed page
makeTagFeed ::
  String
  -> [Page String]
  -> Compiler () (Page String)
makeTagFeed tag posts = 
  constA posts
  >>> listToPageCompiler "templates/atom-post.xml"
  >>> arr (\p -> (p,posts))
  >>> setMaxUpdated
  >>> arr (setField "extratitle" (" - Posts tagged " ++ tag))
  >>> applyTemplateCompiler "templates/atom.xml"

-- | The tags rules
tags ::
  Rules
tags = do
  create "tags" $ requireAll "posts/*" (\_ ps -> readTags ps :: Tags String)
  match "tags/*" . route . customRoute $ tagsPath
  withTags "tags" (fromCapture "tags/*") makeTagPage
  match "tagfeeds/*" $ route $ setExtension ".xml"
  withTags "tags" (fromCapture "tagfeeds/*") makeTagFeed

-- | The feed rules
feed ::
  Rules
feed =
  do
  match "atom.xml" . route $ idRoute
  void . create "atom.xml" $
    requireAll_ "posts/*"
    >>> listToPageCompiler "templates/atom-post.xml"
    >>> requireAllA "posts/*" setMaxUpdated
    >>> arr (setField "extratitle" "")
    >>> applyTemplateCompiler "templates/atom.xml"

-- | The toplevel pages rules
topLevel ::
  Rules
topLevel = match "toplevel/*" $ do
    route $ gsubRoute "toplevel/" (const "") `composeRoutes` setExtension "html"
    void . compile $ pageCompiler'
      (
        addPostsToFields "templates/post-item.html"
        >>> requireA "tags" (setFieldA "tagcloud" renderTagCloud')
      )
      >>> arr (setField "mathjax" "")
      >>> applyTemplateCompiler "templates/default.html"
      >>> relativizeUrlsCompiler

main ::
  IO ()
main = hakyll $ do
  ["favicon.ico"] --> copy
  ["robots.txt"] --> copy
  ["images/**"] --> copy
  ["static/**"] --> copyStatic
  ["js/**"] --> copy
  ["css/*.css"] --> css
  ["templates/*"] --> template
  post
  feed
  tags
  topLevel
    where
      xs --> f = mapM_ (`match` f) xs
      copy = route idRoute >> compile copyFileCompiler
      copyStatic = route (gsubRoute "static/*" (const "")) >> compile copyFileCompiler
      css = route (setExtension "css") >> compile compressCssCompiler
      template = compile templateCompiler

-- | Compiler which adds the list of posts with a template applied to fields of the compiled page
addPostsToFields ::
  Identifier Template
  -> Compiler (Page String) (Page String)
addPostsToFields template =
  setFieldPageList recentFirst template "allPosts" "posts/*"
  >>> setFieldPageList (take 5 . recentFirst) template "recentPosts" "posts/*"
  >>> setFieldPageList chronological template "chronologicalPosts" "posts/*"

-- | Compiler which takes two inputs: a list of pages and a page.  The compiler goes through
-- the list of pages and finds the maximum updated date, and sets the "lastupdated" field in the second
-- input page to this maximum date.  Useful when combined with requireAllA
setMaxUpdated ::
  Compiler (Page String, [Page String]) (Page String)
setMaxUpdated =
  setFieldA "lastupdated" $ arr maxUpdated
    where
      maxUpdated ::
        [Page String]
        -> String
      maxUpdated posts =
        maximum $ map (getField "updated") posts

-- | Build a page by starting with an empty page, applying the template to each page from the input,
--   and finally combining all pages together in the body.  The metadata of the created page just
--   contains the default fields from 'addDefaultFields'.
listToPageCompiler ::
  Identifier Template
   -> Compiler [Page String] (Page String)
listToPageCompiler template =
  pageListCompiler recentFirst template
  >>> arr fromBody
  >>> addDefaultFields

-- | Register a compiler to produce a page for each (tag, list of pages).
--
-- This is a useful combination of 'metaCompile' and 'tagsMap'.
withTags ::
  Identifier (Tags String) -- ^ the mapping of tags to pages
  -> (String -> Identifier (Page String)) -- ^ build an identifier for each created tag page
  -> (String -> [Page String] -> Compiler () (Page String)) -- ^ a compiler for a specific tag page
  -> Rules
withTags t ident pageC =
  metaCompile $ require_ t
  >>> arr tagsMap
  >>> arr (map (\(tag,ps) -> (ident tag, pageC tag ps)))

-- | Renders a tag cloud
--   This needs a type signature for ambigious type variable
renderTagCloud' ::
  Compiler (Tags String) String
renderTagCloud' =
  renderTagCloud (fromCapture "tags/*") 100 120

-- | A page compiler which allows setting fields
pageCompiler' ::
  Compiler (Page String) (Page String)
  -> Compiler Resource (Page String)
pageCompiler' =
  pageCompilerWithFields defaultHakyllParserState defaultHakyllWriterOptions id

fileToDirectory ::
  Identifier a
  -> FilePath
fileToDirectory =
  flip combine "index.html" . dropExtension . uncurry (++) . second (drop 11 {- yyyy-mm-dd- -}) . splitAt 6 {- "posts/" -} . identifierPath

tagsPath ::
  Identifier a
  -> FilePath
tagsPath =
  flip combine "index.html" . ("category"++) . drop 4 {- "tags" -} . map toLower . identifierPath
