{-# LANGUAGE OverloadedStrings #-}

module Attributes where

import Html
import Combinator

import qualified Data.Text as T
import Prelude ()

-- | Define combinators for all attributes listed here:
-- https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes

accept :: T.Text -> Attribute
accept = newAttr "accept"

accept_charset :: T.Text -> Attribute
accept_charset = newAttr "accept-charset"

accesskey :: T.Text -> Attribute
accesskey = newAttr "accesskey"

action :: T.Text -> Attribute
action = newAttr "action"

align :: T.Text -> Attribute
align = newAttr "align"

alt :: T.Text -> Attribute
alt = newAttr "alt"

async :: T.Text -> Attribute
async = newAttr "async"

autocomplete :: T.Text -> Attribute
autocomplete = newAttr "autocomplete"

autofocus :: T.Text -> Attribute
autofocus = newAttr "autofocus"

autoplay :: T.Text -> Attribute
autoplay = newAttr "autoplay"

autosave :: T.Text -> Attribute
autosave = newAttr "autosave"

bgcolor :: T.Text -> Attribute
bgcolor = newAttr "bgcolor"

border :: T.Text -> Attribute
border = newAttr "border"

buffered :: T.Text -> Attribute
buffered = newAttr "buffered"

challenge :: T.Text -> Attribute
challenge = newAttr "challenge"

charset :: T.Text -> Attribute
charset = newAttr "charset"

checkedcite :: T.Text -> Attribute
checkedcite = newAttr "checkedcite"

class_ :: T.Text -> Attribute
class_ = newAttr "class"

code :: T.Text -> Attribute
code = newAttr "code"

codebase :: T.Text -> Attribute
codebase = newAttr "codebase"

color :: T.Text -> Attribute
color = newAttr "color"

cols :: T.Text -> Attribute
cols = newAttr "cols"

colspan :: T.Text -> Attribute
colspan = newAttr "colspan"

content :: T.Text -> Attribute
content = newAttr "content"

contenteditable :: T.Text -> Attribute
contenteditable = newAttr "contenteditable"

contextmenu :: T.Text -> Attribute
contextmenu = newAttr "contextmenu"

controls :: T.Text -> Attribute
controls = newAttr "controls"

coords :: T.Text -> Attribute
coords = newAttr "coords"

data_ :: T.Text -> Attribute
data_ = newAttr "data"

datetime :: T.Text -> Attribute
datetime = newAttr "datetime"

default_ :: T.Text -> Attribute
default_ = newAttr "default"

defer :: T.Text -> Attribute
defer = newAttr "defer"

dir :: T.Text -> Attribute
dir = newAttr "dir"

dirname :: T.Text -> Attribute
dirname = newAttr "dirname"

disabled :: T.Text -> Attribute
disabled = newAttr "disabled"

download :: T.Text -> Attribute
download = newAttr "download"

draggable :: T.Text -> Attribute
draggable = newAttr "draggable"

dropzone :: T.Text -> Attribute
dropzone = newAttr "dropzone"

enctype :: T.Text -> Attribute
enctype = newAttr "enctype"

for :: T.Text -> Attribute
for = newAttr "for"

form :: T.Text -> Attribute
form = newAttr "form"

formaction :: T.Text -> Attribute
formaction = newAttr "formaction"

headers :: T.Text -> Attribute
headers = newAttr "headers"

height :: T.Text -> Attribute
height = newAttr "height"

hidden :: T.Text -> Attribute
hidden = newAttr "hidden"

high :: T.Text -> Attribute
high = newAttr "high"

href :: T.Text -> Attribute
href = newAttr "href"

hreflang :: T.Text -> Attribute
hreflang = newAttr "hreflang"

http_equiv :: T.Text -> Attribute
http_equiv = newAttr "http-equiv"

icon :: T.Text -> Attribute
icon = newAttr "icon"

id :: T.Text -> Attribute
id = newAttr "id"

ismap :: T.Text -> Attribute
ismap = newAttr "ismap"

itemprop :: T.Text -> Attribute
itemprop = newAttr "itemprop"

keytype :: T.Text -> Attribute
keytype = newAttr "keytype"

kind :: T.Text -> Attribute
kind = newAttr "kind"

label :: T.Text -> Attribute
label = newAttr "label"

lang :: T.Text -> Attribute
lang = newAttr "lang"

language :: T.Text -> Attribute
language = newAttr "language"

list :: T.Text -> Attribute
list = newAttr "list"

loop :: T.Text -> Attribute
loop = newAttr "loop"

low :: T.Text -> Attribute
low = newAttr "low"

manifest :: T.Text -> Attribute
manifest = newAttr "manifest"

max :: T.Text -> Attribute
max = newAttr "max"

maxlength :: T.Text -> Attribute
maxlength = newAttr "maxlength"

media :: T.Text -> Attribute
media = newAttr "media"

method :: T.Text -> Attribute
method = newAttr "method"

min :: T.Text -> Attribute
min = newAttr "min"

multiple :: T.Text -> Attribute
multiple = newAttr "multiple"

muted :: T.Text -> Attribute
muted = newAttr "muted"

name :: T.Text -> Attribute
name = newAttr "name"

novalidate :: T.Text -> Attribute
novalidate = newAttr "novalidate"

open :: T.Text -> Attribute
open = newAttr "open"

optimum :: T.Text -> Attribute
optimum = newAttr "optimum"

pattern :: T.Text -> Attribute
pattern = newAttr "pattern"

ping :: T.Text -> Attribute
ping = newAttr "ping"

placeholder :: T.Text -> Attribute
placeholder = newAttr "placeholder"

poster :: T.Text -> Attribute
poster = newAttr "poster"

preload :: T.Text -> Attribute
preload = newAttr "preload"

radiogroup :: T.Text -> Attribute
radiogroup = newAttr "radiogroup"

readonly :: T.Text -> Attribute
readonly = newAttr "readonly"

rel :: T.Text -> Attribute
rel = newAttr "rel"

required :: T.Text -> Attribute
required = newAttr "required"

reversed :: T.Text -> Attribute
reversed = newAttr "reversed"

rows :: T.Text -> Attribute
rows = newAttr "rows"

rowspan :: T.Text -> Attribute
rowspan = newAttr "rowspan"

sandbox :: T.Text -> Attribute
sandbox = newAttr "sandbox"

scope :: T.Text -> Attribute
scope = newAttr "scope"

scoped :: T.Text -> Attribute
scoped = newAttr "scoped"

seamless :: T.Text -> Attribute
seamless = newAttr "seamless"

selected :: T.Text -> Attribute
selected = newAttr "selected"

shape :: T.Text -> Attribute
shape = newAttr "shape"

size :: T.Text -> Attribute
size = newAttr "size"

sizes :: T.Text -> Attribute
sizes = newAttr "sizes"

span :: T.Text -> Attribute
span = newAttr "span"

spellcheck :: T.Text -> Attribute
spellcheck = newAttr "spellcheck"

src :: T.Text -> Attribute
src = newAttr "src"

srcdoc :: T.Text -> Attribute
srcdoc = newAttr "srcdoc"

srclang :: T.Text -> Attribute
srclang = newAttr "srclang"

srcset :: T.Text -> Attribute
srcset = newAttr "srcset"

start :: T.Text -> Attribute
start = newAttr "start"

step :: T.Text -> Attribute
step = newAttr "step"

style :: T.Text -> Attribute
style = newAttr "style"

summary :: T.Text -> Attribute
summary = newAttr "summary"

tabindex :: T.Text -> Attribute
tabindex = newAttr "tabindex"

target :: T.Text -> Attribute
target = newAttr "target"

title :: T.Text -> Attribute
title = newAttr "title"

type_ :: T.Text -> Attribute
type_ = newAttr "type"

usemap :: T.Text -> Attribute
usemap = newAttr "usemap"

value :: T.Text -> Attribute
value = newAttr "value"

width :: T.Text -> Attribute
width = newAttr "width"

wrap :: T.Text -> Attribute
wrap = newAttr "wrap"


