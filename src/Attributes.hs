{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Attributes where

import Html

import qualified Data.Text as T
import Prelude ()

-- | Define combinators for all attributes listed here:
-- https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes

accept :: T.Text -> Attribute
accept = ("accept",)

accept_charset :: T.Text -> Attribute
accept_charset = ("accept-charset",)

accesskey :: T.Text -> Attribute
accesskey = ("accesskey",)

action :: T.Text -> Attribute
action = ("action",)

align :: T.Text -> Attribute
align = ("align",)

alt :: T.Text -> Attribute
alt = ("alt",)

async :: T.Text -> Attribute
async = ("async",)

autocomplete :: T.Text -> Attribute
autocomplete = ("autocomplete",)

autofocus :: T.Text -> Attribute
autofocus = ("autofocus",)

autoplay :: T.Text -> Attribute
autoplay = ("autoplay",)

autosave :: T.Text -> Attribute
autosave = ("autosave",)

bgcolor :: T.Text -> Attribute
bgcolor = ("bgcolor",)

border :: T.Text -> Attribute
border = ("border",)

buffered :: T.Text -> Attribute
buffered = ("buffered",)

challenge :: T.Text -> Attribute
challenge = ("challenge",)

charset :: T.Text -> Attribute
charset = ("charset",)

checkedcite :: T.Text -> Attribute
checkedcite = ("checkedcite",)

class_ :: T.Text -> Attribute
class_ = ("class",)

code :: T.Text -> Attribute
code = ("code",)

codebase :: T.Text -> Attribute
codebase = ("codebase",)

color :: T.Text -> Attribute
color = ("color",)

cols :: T.Text -> Attribute
cols = ("cols",)

colspan :: T.Text -> Attribute
colspan = ("colspan",)

content :: T.Text -> Attribute
content = ("content",)

contenteditable :: T.Text -> Attribute
contenteditable = ("contenteditable",)

contextmenu :: T.Text -> Attribute
contextmenu = ("contextmenu",)

controls :: T.Text -> Attribute
controls = ("controls",)

coords :: T.Text -> Attribute
coords = ("coords",)

data_ :: T.Text -> Attribute
data_ = ("data",)

datetime :: T.Text -> Attribute
datetime = ("datetime",)

default_ :: T.Text -> Attribute
default_ = ("default",)

defer :: T.Text -> Attribute
defer = ("defer",)

dir :: T.Text -> Attribute
dir = ("dir",)

dirname :: T.Text -> Attribute
dirname = ("dirname",)

disabled :: T.Text -> Attribute
disabled = ("disabled",)

download :: T.Text -> Attribute
download = ("download",)

draggable :: T.Text -> Attribute
draggable = ("draggable",)

dropzone :: T.Text -> Attribute
dropzone = ("dropzone",)

enctype :: T.Text -> Attribute
enctype = ("enctype",)

for :: T.Text -> Attribute
for = ("for",)

form :: T.Text -> Attribute
form = ("form",)

formaction :: T.Text -> Attribute
formaction = ("formaction",)

headers :: T.Text -> Attribute
headers = ("headers",)

height :: T.Text -> Attribute
height = ("height",)

hidden :: T.Text -> Attribute
hidden = ("hidden",)

high :: T.Text -> Attribute
high = ("high",)

href :: T.Text -> Attribute
href = ("href",)

hreflang :: T.Text -> Attribute
hreflang = ("hreflang",)

http_equiv :: T.Text -> Attribute
http_equiv = ("http-equiv",)

icon :: T.Text -> Attribute
icon = ("icon",)

id :: T.Text -> Attribute
id = ("id",)

ismap :: T.Text -> Attribute
ismap = ("ismap",)

itemprop :: T.Text -> Attribute
itemprop = ("itemprop",)

keytype :: T.Text -> Attribute
keytype = ("keytype",)

kind :: T.Text -> Attribute
kind = ("kind",)

label :: T.Text -> Attribute
label = ("label",)

lang :: T.Text -> Attribute
lang = ("lang",)

language :: T.Text -> Attribute
language = ("language",)

list :: T.Text -> Attribute
list = ("list",)

loop :: T.Text -> Attribute
loop = ("loop",)

low :: T.Text -> Attribute
low = ("low",)

manifest :: T.Text -> Attribute
manifest = ("manifest",)

max :: T.Text -> Attribute
max = ("max",)

maxlength :: T.Text -> Attribute
maxlength = ("maxlength",)

media :: T.Text -> Attribute
media = ("media",)

method :: T.Text -> Attribute
method = ("method",)

min :: T.Text -> Attribute
min = ("min",)

multiple :: T.Text -> Attribute
multiple = ("multiple",)

muted :: T.Text -> Attribute
muted = ("muted",)

name :: T.Text -> Attribute
name = ("name",)

novalidate :: T.Text -> Attribute
novalidate = ("novalidate",)

open :: T.Text -> Attribute
open = ("open",)

optimum :: T.Text -> Attribute
optimum = ("optimum",)

pattern :: T.Text -> Attribute
pattern = ("pattern",)

ping :: T.Text -> Attribute
ping = ("ping",)

placeholder :: T.Text -> Attribute
placeholder = ("placeholder",)

poster :: T.Text -> Attribute
poster = ("poster",)

preload :: T.Text -> Attribute
preload = ("preload",)

radiogroup :: T.Text -> Attribute
radiogroup = ("radiogroup",)

readonly :: T.Text -> Attribute
readonly = ("readonly",)

rel :: T.Text -> Attribute
rel = ("rel",)

required :: T.Text -> Attribute
required = ("required",)

reversed :: T.Text -> Attribute
reversed = ("reversed",)

rows :: T.Text -> Attribute
rows = ("rows",)

rowspan :: T.Text -> Attribute
rowspan = ("rowspan",)

sandbox :: T.Text -> Attribute
sandbox = ("sandbox",)

scope :: T.Text -> Attribute
scope = ("scope",)

scoped :: T.Text -> Attribute
scoped = ("scoped",)

seamless :: T.Text -> Attribute
seamless = ("seamless",)

selected :: T.Text -> Attribute
selected = ("selected",)

shape :: T.Text -> Attribute
shape = ("shape",)

size :: T.Text -> Attribute
size = ("size",)

sizes :: T.Text -> Attribute
sizes = ("sizes",)

span :: T.Text -> Attribute
span = ("span",)

spellcheck :: T.Text -> Attribute
spellcheck = ("spellcheck",)

src :: T.Text -> Attribute
src = ("src",)

srcdoc :: T.Text -> Attribute
srcdoc = ("srcdoc",)

srclang :: T.Text -> Attribute
srclang = ("srclang",)

srcset :: T.Text -> Attribute
srcset = ("srcset",)

start :: T.Text -> Attribute
start = ("start",)

step :: T.Text -> Attribute
step = ("step",)

style :: T.Text -> Attribute
style = ("style",)

summary :: T.Text -> Attribute
summary = ("summary",)

tabindex :: T.Text -> Attribute
tabindex = ("tabindex",)

target :: T.Text -> Attribute
target = ("target",)

title :: T.Text -> Attribute
title = ("title",)

type_ :: T.Text -> Attribute
type_ = ("type",)

usemap :: T.Text -> Attribute
usemap = ("usemap",)

value :: T.Text -> Attribute
value = ("value",)

width :: T.Text -> Attribute
width = ("width",)

wrap :: T.Text -> Attribute
wrap = ("wrap",)

