{-# LANGUAGE OverloadedStrings #-}

-- | This module exposes combinators for all of the basic HTML elements
-- as listed here: 
-- https://www.w3.org/TR/html-markup/elements-by-function.html
module Tags where

import Combinator

-- Hide the Prelude because several tag names conflict with
-- Prelude functions. In practice, this module should be
-- imported qualified to prevent name collisions.
import Prelude ()


-- Void elements. These can't contain child elements and thus
-- do not take any arguments.

area :: HtmlLeaf
area = newVoid "area"

base :: HtmlLeaf
base = newVoid "base"

br :: HtmlLeaf
br = newVoid "br"

col :: HtmlLeaf
col = newVoid "col"

hr :: HtmlLeaf
hr = newVoid "hr"

img :: HtmlLeaf
img = newVoid "img"

input :: HtmlLeaf
input = newVoid "input"

link :: HtmlLeaf
link = newVoid "link"

meta :: HtmlLeaf
meta = newVoid "meta"

param :: HtmlLeaf
param = newVoid "param"



-- Parent elements. Takes an HtmlM and embeds it as the inner HTML of 
-- the given element, making it easy to nest elements.

html :: HtmlParent
html = newElem "html"

head :: HtmlParent
head = newElem "head"

title :: HtmlParent
title = newElem "title"

style :: HtmlParent
style = newElem "style"

script :: HtmlParent
script = newElem "script"

noscript :: HtmlParent
noscript = newElem "noscript"

body :: HtmlParent
body = newElem "body"

section :: HtmlParent
section = newElem "section"

nav :: HtmlParent
nav = newElem "nav"

article :: HtmlParent
article = newElem "article"

aside :: HtmlParent
aside = newElem "aside"

h1 :: HtmlParent
h1 = newElem "h1"

h2 :: HtmlParent
h2 = newElem "h2"

h3 :: HtmlParent
h3 = newElem "h3"

h4 :: HtmlParent
h4 = newElem "h4"

h5 :: HtmlParent
h5 = newElem "h5"

h6 :: HtmlParent
h6 = newElem "h6"

hgroup :: HtmlParent
hgroup = newElem "hgroup"

header :: HtmlParent
header = newElem "header"

footer :: HtmlParent
footer = newElem "footer"

address :: HtmlParent
address = newElem "address"

p :: HtmlParent
p = newElem "p"

pre :: HtmlParent
pre = newElem "pre"

blockquote :: HtmlParent
blockquote = newElem "blockquote"

ol :: HtmlParent
ol = newElem "ol"

li :: HtmlParent
li = newElem "li"

dl :: HtmlParent
dl = newElem "dl"

dt :: HtmlParent
dt = newElem "dt"

dd :: HtmlParent
dd = newElem "dd"

figure :: HtmlParent
figure = newElem "figure"

figcaption :: HtmlParent
figcaption = newElem "figcaption"

div :: HtmlParent
div = newElem "div"

a :: HtmlParent
a = newElem "a"

em :: HtmlParent
em = newElem "em"

strong :: HtmlParent
strong = newElem "strong"

s :: HtmlParent
s = newElem "s"

cite :: HtmlParent
cite = newElem "cite"

q :: HtmlParent
q = newElem "q"

dfn :: HtmlParent
dfn = newElem "dfn"

abbr :: HtmlParent
abbr = newElem "abbr"

time :: HtmlParent
time = newElem "time"

code :: HtmlParent
code = newElem "code"









