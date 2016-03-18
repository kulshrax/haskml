{-# LANGUAGE OverloadedStrings #-}

module Tags where

import Combinator

-- | Hide the Prelude because several tag names conflict with
-- Prelude functions. In practice, this module should be
-- imported qualified to prevent name collisions.
import Prelude ()


-- | Void elements. These can't contain child elements and thus
-- do not take any arguments. List of void elements taken from here:
-- https://www.w3.org/TR/html-markup/syntax.html#syntax-elements

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

keygen :: HtmlLeaf
keygen = newVoid "keygen"

link :: HtmlLeaf
link = newVoid "link"

meta :: HtmlLeaf
meta = newVoid "meta"

param :: HtmlLeaf
param = newVoid "param"

source :: HtmlLeaf
source = newVoid "source"

track :: HtmlLeaf
track = newVoid "track"

wbr :: HtmlLeaf
wbr = newVoid "wbr"


-- | Parent elements. Takes an HtmlM and embeds it as the inner HTML of 
-- the given element, making it easy to nest elements. Combinators are
-- included for all elements listed here:
-- https://www.w3.org/TR/html-markup/elements-by-function.html
-- (With the exception of the void elements, which are defined above.)

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

ul :: HtmlParent
ul = newElem "ul"

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

var :: HtmlParent
var = newElem "var"

samp :: HtmlParent
samp = newElem "samp"

kbd :: HtmlParent
kbd = newElem "kbd"

sub :: HtmlParent
sub = newElem "sub"

sup :: HtmlParent
sup = newElem "sup"

i :: HtmlParent
i = newElem "i"

b :: HtmlParent
b = newElem "b"

u :: HtmlParent
u = newElem "u"

mark :: HtmlParent
mark = newElem "mark"

ruby :: HtmlParent
ruby = newElem "ruby"

rt :: HtmlParent
rt = newElem "rt"

rp :: HtmlParent
rp = newElem "rp"

bdi :: HtmlParent
bdi = newElem "bdi"

bdo :: HtmlParent
bdo = newElem "bdo"

span :: HtmlParent
span = newElem "span"

ins :: HtmlParent
ins = newElem "ins"

del :: HtmlParent
del = newElem "del"

iframe :: HtmlParent
iframe = newElem "iframe"

embed :: HtmlParent
embed = newElem "embed"

object :: HtmlParent
object = newElem "object"

video :: HtmlParent
video = newElem "video"

audio :: HtmlParent
audio = newElem "audio"

canvas :: HtmlParent
canvas = newElem "canvas"

map :: HtmlParent
map = newElem "map"

table :: HtmlParent
table = newElem "table"

caption :: HtmlParent
caption = newElem "caption"

colgroup :: HtmlParent
colgroup = newElem "colgroup"

tbody :: HtmlParent
tbody = newElem "tbody"

thead :: HtmlParent
thead = newElem "thead"

tfoot :: HtmlParent
tfoot = newElem "tfoot"

tr :: HtmlParent
tr = newElem "tr"

td :: HtmlParent
td = newElem "td"

th :: HtmlParent
th = newElem "th"

form :: HtmlParent
form = newElem "form"

fieldset :: HtmlParent
fieldset = newElem "fieldset"

legend :: HtmlParent
legend = newElem "legend"

label :: HtmlParent
label = newElem "label"

button :: HtmlParent
button = newElem "button"

select :: HtmlParent
select = newElem "select"

datalist :: HtmlParent
datalist = newElem "datalist"

optgroup :: HtmlParent
optgroup = newElem "optgroup"

option :: HtmlParent
option = newElem "option"

textarea :: HtmlParent
textarea = newElem "textarea"

output :: HtmlParent
output = newElem "output"

progess :: HtmlParent
progess = newElem "progess"

meter :: HtmlParent
meter = newElem "meter"

details :: HtmlParent
details = newElem "details"

summary :: HtmlParent
summary = newElem "summary"

command :: HtmlParent
command = newElem "command"

menu :: HtmlParent
menu = newElem "menu"
