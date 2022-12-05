module Mation.Gen.Tags where

import Mation.Core.Html (Prop, Html, mkElement)


a :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
a props children = mkElement "a" props children

abbr :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
abbr props children = mkElement "abbr" props children

address :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
address props children = mkElement "address" props children

area :: forall m s. Array (Prop m s) -> Html m s
area props = mkElement "area" props []

article :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
article props children = mkElement "article" props children

aside :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
aside props children = mkElement "aside" props children

audio :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
audio props children = mkElement "audio" props children

b :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
b props children = mkElement "b" props children

base :: forall m s. Array (Prop m s) -> Html m s
base props = mkElement "base" props []

bdi :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
bdi props children = mkElement "bdi" props children

bdo :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
bdo props children = mkElement "bdo" props children

blockquote :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
blockquote props children = mkElement "blockquote" props children

body :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
body props children = mkElement "body" props children

br :: forall m s. Array (Prop m s) -> Html m s
br props = mkElement "br" props []

button :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
button props children = mkElement "button" props children

canvas :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
canvas props children = mkElement "canvas" props children

caption :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
caption props children = mkElement "caption" props children

cite :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
cite props children = mkElement "cite" props children

code :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
code props children = mkElement "code" props children

col :: forall m s. Array (Prop m s) -> Html m s
col props = mkElement "col" props []

colgroup :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
colgroup props children = mkElement "colgroup" props children

data_ :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
data_ props children = mkElement "data" props children

datalist :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
datalist props children = mkElement "datalist" props children

dd :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
dd props children = mkElement "dd" props children

del :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
del props children = mkElement "del" props children

details :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
details props children = mkElement "details" props children

dfn :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
dfn props children = mkElement "dfn" props children

dialog :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
dialog props children = mkElement "dialog" props children

div :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
div props children = mkElement "div" props children

dl :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
dl props children = mkElement "dl" props children

dt :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
dt props children = mkElement "dt" props children

em :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
em props children = mkElement "em" props children

embed :: forall m s. Array (Prop m s) -> Html m s
embed props = mkElement "embed" props []

fieldset :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
fieldset props children = mkElement "fieldset" props children

figcaption :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
figcaption props children = mkElement "figcaption" props children

figure :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
figure props children = mkElement "figure" props children

footer :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
footer props children = mkElement "footer" props children

form :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
form props children = mkElement "form" props children

h1 :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
h1 props children = mkElement "h1" props children

h2 :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
h2 props children = mkElement "h2" props children

h3 :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
h3 props children = mkElement "h3" props children

h4 :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
h4 props children = mkElement "h4" props children

h5 :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
h5 props children = mkElement "h5" props children

h6 :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
h6 props children = mkElement "h6" props children

head :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
head props children = mkElement "head" props children

header :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
header props children = mkElement "header" props children

hgroup :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
hgroup props children = mkElement "hgroup" props children

hr :: forall m s. Array (Prop m s) -> Html m s
hr props = mkElement "hr" props []

html :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
html props children = mkElement "html" props children

i :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
i props children = mkElement "i" props children

iframe :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
iframe props children = mkElement "iframe" props children

img :: forall m s. Array (Prop m s) -> Html m s
img props = mkElement "img" props []

input :: forall m s. Array (Prop m s) -> Html m s
input props = mkElement "input" props []

ins :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
ins props children = mkElement "ins" props children

kbd :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
kbd props children = mkElement "kbd" props children

label :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
label props children = mkElement "label" props children

legend :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
legend props children = mkElement "legend" props children

li :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
li props children = mkElement "li" props children

link :: forall m s. Array (Prop m s) -> Html m s
link props = mkElement "link" props []

main :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
main props children = mkElement "main" props children

map :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
map props children = mkElement "map" props children

mark :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
mark props children = mkElement "mark" props children

math :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
math props children = mkElement "math" props children

menu :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
menu props children = mkElement "menu" props children

meta :: forall m s. Array (Prop m s) -> Html m s
meta props = mkElement "meta" props []

meter :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
meter props children = mkElement "meter" props children

nav :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
nav props children = mkElement "nav" props children

noscript :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
noscript props children = mkElement "noscript" props children

object :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
object props children = mkElement "object" props children

ol :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
ol props children = mkElement "ol" props children

optgroup :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
optgroup props children = mkElement "optgroup" props children

option :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
option props children = mkElement "option" props children

output :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
output props children = mkElement "output" props children

p :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
p props children = mkElement "p" props children

param :: forall m s. Array (Prop m s) -> Html m s
param props = mkElement "param" props []

picture :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
picture props children = mkElement "picture" props children

pre :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
pre props children = mkElement "pre" props children

progress :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
progress props children = mkElement "progress" props children

q :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
q props children = mkElement "q" props children

rp :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
rp props children = mkElement "rp" props children

rt :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
rt props children = mkElement "rt" props children

ruby :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
ruby props children = mkElement "ruby" props children

s :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
s props children = mkElement "s" props children

samp :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
samp props children = mkElement "samp" props children

script :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
script props children = mkElement "script" props children

section :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
section props children = mkElement "section" props children

select :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
select props children = mkElement "select" props children

slot :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
slot props children = mkElement "slot" props children

small :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
small props children = mkElement "small" props children

source :: forall m s. Array (Prop m s) -> Html m s
source props = mkElement "source" props []

span :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
span props children = mkElement "span" props children

strong :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
strong props children = mkElement "strong" props children

style :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
style props children = mkElement "style" props children

sub :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
sub props children = mkElement "sub" props children

summary :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
summary props children = mkElement "summary" props children

sup :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
sup props children = mkElement "sup" props children

svg :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
svg props children = mkElement "svg" props children

table :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
table props children = mkElement "table" props children

tbody :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
tbody props children = mkElement "tbody" props children

td :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
td props children = mkElement "td" props children

template :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
template props children = mkElement "template" props children

textarea :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
textarea props children = mkElement "textarea" props children

tfoot :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
tfoot props children = mkElement "tfoot" props children

th :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
th props children = mkElement "th" props children

thead :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
thead props children = mkElement "thead" props children

time :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
time props children = mkElement "time" props children

title :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
title props children = mkElement "title" props children

tr :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
tr props children = mkElement "tr" props children

track :: forall m s. Array (Prop m s) -> Html m s
track props = mkElement "track" props []

u :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
u props children = mkElement "u" props children

ul :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
ul props children = mkElement "ul" props children

var :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
var props children = mkElement "var" props children

video :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s
video props children = mkElement "video" props children

wbr :: forall m s. Array (Prop m s) -> Html m s
wbr props = mkElement "wbr" props []
