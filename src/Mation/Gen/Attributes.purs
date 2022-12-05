module Mation.Gen.Attributes where

import Mation.Core.Html (Prop (..))


abbr :: forall m s. String -> Prop m s
abbr val = PPair "abbr" val

accept :: forall m s. String -> Prop m s
accept val = PPair "accept" val

acceptcharset :: forall m s. String -> Prop m s
acceptcharset val = PPair "accept-charset" val

accesskey :: forall m s. String -> Prop m s
accesskey val = PPair "accesskey" val

action :: forall m s. String -> Prop m s
action val = PPair "action" val

allow :: forall m s. String -> Prop m s
allow val = PPair "allow" val

allowfullscreen :: forall m s. Boolean -> Prop m s
allowfullscreen bool = if bool then PPair "allowfullscreen" "allowfullscreen" else PNoop

allowpaymentrequest :: forall m s. String -> Prop m s
allowpaymentrequest val = PPair "allowpaymentrequest" val

alt :: forall m s. String -> Prop m s
alt val = PPair "alt" val

as :: forall m s. String -> Prop m s
as val = PPair "as" val

async :: forall m s. Boolean -> Prop m s
async bool = if bool then PPair "async" "async" else PNoop

autocapitalize :: forall m s. String -> Prop m s
autocapitalize val = PPair "autocapitalize" val

autocomplete :: forall m s. String -> Prop m s
autocomplete val = PPair "autocomplete" val

autofocus :: forall m s. Boolean -> Prop m s
autofocus bool = if bool then PPair "autofocus" "autofocus" else PNoop

autoplay :: forall m s. Boolean -> Prop m s
autoplay bool = if bool then PPair "autoplay" "autoplay" else PNoop

charset :: forall m s. String -> Prop m s
charset val = PPair "charset" val

checked :: forall m s. Boolean -> Prop m s
checked bool = if bool then PPair "checked" "checked" else PNoop

cite :: forall m s. String -> Prop m s
cite val = PPair "cite" val

class_ :: forall m s. String -> Prop m s
class_ val = PPair "class" val

color :: forall m s. String -> Prop m s
color val = PPair "color" val

cols :: forall m s. String -> Prop m s
cols val = PPair "cols" val

colspan :: forall m s. String -> Prop m s
colspan val = PPair "colspan" val

content :: forall m s. String -> Prop m s
content val = PPair "content" val

contenteditable :: forall m s. String -> Prop m s
contenteditable val = PPair "contenteditable" val

controls :: forall m s. Boolean -> Prop m s
controls bool = if bool then PPair "controls" "controls" else PNoop

coords :: forall m s. String -> Prop m s
coords val = PPair "coords" val

crossorigin :: forall m s. String -> Prop m s
crossorigin val = PPair "crossorigin" val

data_ :: forall m s. String -> Prop m s
data_ val = PPair "data" val

datetime :: forall m s. String -> Prop m s
datetime val = PPair "datetime" val

decoding :: forall m s. String -> Prop m s
decoding val = PPair "decoding" val

default :: forall m s. Boolean -> Prop m s
default bool = if bool then PPair "default" "default" else PNoop

defer :: forall m s. Boolean -> Prop m s
defer bool = if bool then PPair "defer" "defer" else PNoop

dir :: forall m s. String -> Prop m s
dir val = PPair "dir" val

dirname :: forall m s. String -> Prop m s
dirname val = PPair "dirname" val

disabled :: forall m s. Boolean -> Prop m s
disabled bool = if bool then PPair "disabled" "disabled" else PNoop

download :: forall m s. String -> Prop m s
download val = PPair "download" val

draggable :: forall m s. String -> Prop m s
draggable val = PPair "draggable" val

enctype :: forall m s. String -> Prop m s
enctype val = PPair "enctype" val

enterkeyhint :: forall m s. String -> Prop m s
enterkeyhint val = PPair "enterkeyhint" val

for :: forall m s. String -> Prop m s
for val = PPair "for" val

form :: forall m s. String -> Prop m s
form val = PPair "form" val

formaction :: forall m s. String -> Prop m s
formaction val = PPair "formaction" val

formenctype :: forall m s. String -> Prop m s
formenctype val = PPair "formenctype" val

formmethod :: forall m s. String -> Prop m s
formmethod val = PPair "formmethod" val

formnovalidate :: forall m s. Boolean -> Prop m s
formnovalidate bool = if bool then PPair "formnovalidate" "formnovalidate" else PNoop

formtarget :: forall m s. String -> Prop m s
formtarget val = PPair "formtarget" val

headers :: forall m s. String -> Prop m s
headers val = PPair "headers" val

height :: forall m s. String -> Prop m s
height val = PPair "height" val

hidden :: forall m s. String -> Prop m s
hidden val = PPair "hidden" val

high :: forall m s. String -> Prop m s
high val = PPair "high" val

href :: forall m s. String -> Prop m s
href val = PPair "href" val

hreflang :: forall m s. String -> Prop m s
hreflang val = PPair "hreflang" val

httpequiv :: forall m s. String -> Prop m s
httpequiv val = PPair "http-equiv" val

id :: forall m s. String -> Prop m s
id val = PPair "id" val

imagesizes :: forall m s. String -> Prop m s
imagesizes val = PPair "imagesizes" val

imagesrcset :: forall m s. String -> Prop m s
imagesrcset val = PPair "imagesrcset" val

inputmode :: forall m s. String -> Prop m s
inputmode val = PPair "inputmode" val

integrity :: forall m s. String -> Prop m s
integrity val = PPair "integrity" val

is :: forall m s. String -> Prop m s
is val = PPair "is" val

ismap :: forall m s. Boolean -> Prop m s
ismap bool = if bool then PPair "ismap" "ismap" else PNoop

itemid :: forall m s. String -> Prop m s
itemid val = PPair "itemid" val

itemprop :: forall m s. String -> Prop m s
itemprop val = PPair "itemprop" val

itemref :: forall m s. String -> Prop m s
itemref val = PPair "itemref" val

itemscope :: forall m s. Boolean -> Prop m s
itemscope bool = if bool then PPair "itemscope" "itemscope" else PNoop

itemtype :: forall m s. String -> Prop m s
itemtype val = PPair "itemtype" val

kind :: forall m s. String -> Prop m s
kind val = PPair "kind" val

label :: forall m s. String -> Prop m s
label val = PPair "label" val

lang :: forall m s. String -> Prop m s
lang val = PPair "lang" val

list :: forall m s. String -> Prop m s
list val = PPair "list" val

loading :: forall m s. String -> Prop m s
loading val = PPair "loading" val

loop :: forall m s. Boolean -> Prop m s
loop bool = if bool then PPair "loop" "loop" else PNoop

low :: forall m s. String -> Prop m s
low val = PPair "low" val

manifest :: forall m s. String -> Prop m s
manifest val = PPair "manifest" val

max :: forall m s. String -> Prop m s
max val = PPair "max" val

maxlength :: forall m s. String -> Prop m s
maxlength val = PPair "maxlength" val

media :: forall m s. String -> Prop m s
media val = PPair "media" val

method :: forall m s. String -> Prop m s
method val = PPair "method" val

min :: forall m s. String -> Prop m s
min val = PPair "min" val

minlength :: forall m s. String -> Prop m s
minlength val = PPair "minlength" val

multiple :: forall m s. Boolean -> Prop m s
multiple bool = if bool then PPair "multiple" "multiple" else PNoop

muted :: forall m s. Boolean -> Prop m s
muted bool = if bool then PPair "muted" "muted" else PNoop

name :: forall m s. String -> Prop m s
name val = PPair "name" val

nomodule :: forall m s. Boolean -> Prop m s
nomodule bool = if bool then PPair "nomodule" "nomodule" else PNoop

nonce :: forall m s. String -> Prop m s
nonce val = PPair "nonce" val

novalidate :: forall m s. Boolean -> Prop m s
novalidate bool = if bool then PPair "novalidate" "novalidate" else PNoop

open :: forall m s. Boolean -> Prop m s
open bool = if bool then PPair "open" "open" else PNoop

optimum :: forall m s. String -> Prop m s
optimum val = PPair "optimum" val

pattern :: forall m s. String -> Prop m s
pattern val = PPair "pattern" val

ping :: forall m s. String -> Prop m s
ping val = PPair "ping" val

placeholder :: forall m s. String -> Prop m s
placeholder val = PPair "placeholder" val

playsinline :: forall m s. Boolean -> Prop m s
playsinline bool = if bool then PPair "playsinline" "playsinline" else PNoop

poster :: forall m s. String -> Prop m s
poster val = PPair "poster" val

preload :: forall m s. String -> Prop m s
preload val = PPair "preload" val

readonly :: forall m s. Boolean -> Prop m s
readonly bool = if bool then PPair "readonly" "readonly" else PNoop

referrerpolicy :: forall m s. String -> Prop m s
referrerpolicy val = PPair "referrerpolicy" val

rel :: forall m s. String -> Prop m s
rel val = PPair "rel" val

required :: forall m s. Boolean -> Prop m s
required bool = if bool then PPair "required" "required" else PNoop

reversed :: forall m s. Boolean -> Prop m s
reversed bool = if bool then PPair "reversed" "reversed" else PNoop

role :: forall m s. String -> Prop m s
role val = PPair "role" val

rows :: forall m s. String -> Prop m s
rows val = PPair "rows" val

rowspan :: forall m s. String -> Prop m s
rowspan val = PPair "rowspan" val

sandbox :: forall m s. String -> Prop m s
sandbox val = PPair "sandbox" val

scope :: forall m s. String -> Prop m s
scope val = PPair "scope" val

selected :: forall m s. Boolean -> Prop m s
selected bool = if bool then PPair "selected" "selected" else PNoop

shape :: forall m s. String -> Prop m s
shape val = PPair "shape" val

size :: forall m s. String -> Prop m s
size val = PPair "size" val

sizes :: forall m s. String -> Prop m s
sizes val = PPair "sizes" val

slot :: forall m s. String -> Prop m s
slot val = PPair "slot" val

span :: forall m s. String -> Prop m s
span val = PPair "span" val

spellcheck :: forall m s. String -> Prop m s
spellcheck val = PPair "spellcheck" val

src :: forall m s. String -> Prop m s
src val = PPair "src" val

srcdoc :: forall m s. String -> Prop m s
srcdoc val = PPair "srcdoc" val

srclang :: forall m s. String -> Prop m s
srclang val = PPair "srclang" val

srcset :: forall m s. String -> Prop m s
srcset val = PPair "srcset" val

start :: forall m s. String -> Prop m s
start val = PPair "start" val

step :: forall m s. String -> Prop m s
step val = PPair "step" val

style :: forall m s. String -> Prop m s
style val = PPair "style" val

tabindex :: forall m s. String -> Prop m s
tabindex val = PPair "tabindex" val

target :: forall m s. String -> Prop m s
target val = PPair "target" val

title :: forall m s. String -> Prop m s
title val = PPair "title" val

translate :: forall m s. String -> Prop m s
translate val = PPair "translate" val

type_ :: forall m s. String -> Prop m s
type_ val = PPair "type" val

usemap :: forall m s. String -> Prop m s
usemap val = PPair "usemap" val

value :: forall m s. String -> Prop m s
value val = PPair "value" val

width :: forall m s. String -> Prop m s
width val = PPair "width" val

wrap :: forall m s. String -> Prop m s
wrap val = PPair "wrap" val
