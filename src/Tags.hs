module Tags where

import Combinator

-- https://www.w3.org/TR/html-markup/elements-by-function.html



html :: HtmlC -> HtmlC
html = customElem "html"

head :: HtmlC -> HtmlC
head = customElem "head"

title :: HtmlC -> HtmlC
title = customElem "title"







-- Void elements:

area :: HtmlC
area = customVoid "area"

base :: HtmlC
base = customVoid "base"

br :: HtmlC
br = customVoid "br"

col :: HtmlC
col = customVoid "col"

hr :: HtmlC
hr = customVoid "hr"

img :: HtmlC
img = customVoid "img"

input :: HtmlC
input = customVoid "input"

link :: HtmlC
link = customVoid "link"

meta :: HtmlC
meta = customVoid "meta"

param :: HtmlC
param = customVoid "param"