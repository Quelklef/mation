module Mation.Gen.Attributes where

import Prelude
import Mation.Core.Html (Prop, mkPair, mkNoop)


abbr :: forall m s. String -> Prop m s
abbr = mkPair "abbr"

accept :: forall m s. String -> Prop m s
accept = mkPair "accept"

acceptCharset :: forall m s. String -> Prop m s
acceptCharset = mkPair "accept-charset"

accesskey :: forall m s. String -> Prop m s
accesskey = mkPair "accesskey"

action :: forall m s. String -> Prop m s
action = mkPair "action"

allow :: forall m s. String -> Prop m s
allow = mkPair "allow"

allowfullscreen :: forall m s. Boolean -> Prop m s
allowfullscreen bool = if bool then mkPair "allowfullscreen" "allowfullscreen" else mkNoop

allowpaymentrequest :: forall m s. String -> Prop m s
allowpaymentrequest = mkPair "allowpaymentrequest"

alt :: forall m s. String -> Prop m s
alt = mkPair "alt"

as :: forall m s. String -> Prop m s
as = mkPair "as"

async :: forall m s. Boolean -> Prop m s
async bool = if bool then mkPair "async" "async" else mkNoop

autocapitalize :: forall m s. String -> Prop m s
autocapitalize = mkPair "autocapitalize"

autocomplete :: forall m s. String -> Prop m s
autocomplete = mkPair "autocomplete"

autofocus :: forall m s. Boolean -> Prop m s
autofocus bool = if bool then mkPair "autofocus" "autofocus" else mkNoop

autoplay :: forall m s. Boolean -> Prop m s
autoplay bool = if bool then mkPair "autoplay" "autoplay" else mkNoop

charset :: forall m s. String -> Prop m s
charset = mkPair "charset"

checked :: forall m s. Boolean -> Prop m s
checked bool = if bool then mkPair "checked" "checked" else mkNoop

cite :: forall m s. String -> Prop m s
cite = mkPair "cite"

class_ :: forall m s. String -> Prop m s
class_ = mkPair "class"

color :: forall m s. String -> Prop m s
color = mkPair "color"

cols :: forall m s. String -> Prop m s
cols = mkPair "cols"

colspan :: forall m s. String -> Prop m s
colspan = mkPair "colspan"

content :: forall m s. String -> Prop m s
content = mkPair "content"

contenteditable :: forall m s. String -> Prop m s
contenteditable = mkPair "contenteditable"

controls :: forall m s. Boolean -> Prop m s
controls bool = if bool then mkPair "controls" "controls" else mkNoop

coords :: forall m s. String -> Prop m s
coords = mkPair "coords"

crossorigin :: forall m s. String -> Prop m s
crossorigin = mkPair "crossorigin"

data_ :: forall m s. String -> Prop m s
data_ = mkPair "data"

datetime :: forall m s. String -> Prop m s
datetime = mkPair "datetime"

decoding :: forall m s. String -> Prop m s
decoding = mkPair "decoding"

default :: forall m s. Boolean -> Prop m s
default bool = if bool then mkPair "default" "default" else mkNoop

defer :: forall m s. Boolean -> Prop m s
defer bool = if bool then mkPair "defer" "defer" else mkNoop

dir :: forall m s. String -> Prop m s
dir = mkPair "dir"

dirname :: forall m s. String -> Prop m s
dirname = mkPair "dirname"

disabled :: forall m s. Boolean -> Prop m s
disabled bool = if bool then mkPair "disabled" "disabled" else mkNoop

download :: forall m s. String -> Prop m s
download = mkPair "download"

draggable :: forall m s. String -> Prop m s
draggable = mkPair "draggable"

enctype :: forall m s. String -> Prop m s
enctype = mkPair "enctype"

enterkeyhint :: forall m s. String -> Prop m s
enterkeyhint = mkPair "enterkeyhint"

for :: forall m s. String -> Prop m s
for = mkPair "for"

form :: forall m s. String -> Prop m s
form = mkPair "form"

formaction :: forall m s. String -> Prop m s
formaction = mkPair "formaction"

formenctype :: forall m s. String -> Prop m s
formenctype = mkPair "formenctype"

formmethod :: forall m s. String -> Prop m s
formmethod = mkPair "formmethod"

formnovalidate :: forall m s. Boolean -> Prop m s
formnovalidate bool = if bool then mkPair "formnovalidate" "formnovalidate" else mkNoop

formtarget :: forall m s. String -> Prop m s
formtarget = mkPair "formtarget"

headers :: forall m s. String -> Prop m s
headers = mkPair "headers"

height :: forall m s. String -> Prop m s
height = mkPair "height"

hidden :: forall m s. String -> Prop m s
hidden = mkPair "hidden"

high :: forall m s. String -> Prop m s
high = mkPair "high"

href :: forall m s. String -> Prop m s
href = mkPair "href"

hreflang :: forall m s. String -> Prop m s
hreflang = mkPair "hreflang"

httpEquiv :: forall m s. String -> Prop m s
httpEquiv = mkPair "http-equiv"

id :: forall m s. String -> Prop m s
id = mkPair "id"

imagesizes :: forall m s. String -> Prop m s
imagesizes = mkPair "imagesizes"

imagesrcset :: forall m s. String -> Prop m s
imagesrcset = mkPair "imagesrcset"

inputmode :: forall m s. String -> Prop m s
inputmode = mkPair "inputmode"

integrity :: forall m s. String -> Prop m s
integrity = mkPair "integrity"

is :: forall m s. String -> Prop m s
is = mkPair "is"

ismap :: forall m s. Boolean -> Prop m s
ismap bool = if bool then mkPair "ismap" "ismap" else mkNoop

itemid :: forall m s. String -> Prop m s
itemid = mkPair "itemid"

itemprop :: forall m s. String -> Prop m s
itemprop = mkPair "itemprop"

itemref :: forall m s. String -> Prop m s
itemref = mkPair "itemref"

itemscope :: forall m s. Boolean -> Prop m s
itemscope bool = if bool then mkPair "itemscope" "itemscope" else mkNoop

itemtype :: forall m s. String -> Prop m s
itemtype = mkPair "itemtype"

kind :: forall m s. String -> Prop m s
kind = mkPair "kind"

label :: forall m s. String -> Prop m s
label = mkPair "label"

lang :: forall m s. String -> Prop m s
lang = mkPair "lang"

list :: forall m s. String -> Prop m s
list = mkPair "list"

loading :: forall m s. String -> Prop m s
loading = mkPair "loading"

loop :: forall m s. Boolean -> Prop m s
loop bool = if bool then mkPair "loop" "loop" else mkNoop

low :: forall m s. String -> Prop m s
low = mkPair "low"

manifest :: forall m s. String -> Prop m s
manifest = mkPair "manifest"

max :: forall m s. String -> Prop m s
max = mkPair "max"

maxlength :: forall m s. String -> Prop m s
maxlength = mkPair "maxlength"

media :: forall m s. String -> Prop m s
media = mkPair "media"

method :: forall m s. String -> Prop m s
method = mkPair "method"

min :: forall m s. String -> Prop m s
min = mkPair "min"

minlength :: forall m s. String -> Prop m s
minlength = mkPair "minlength"

multiple :: forall m s. Boolean -> Prop m s
multiple bool = if bool then mkPair "multiple" "multiple" else mkNoop

muted :: forall m s. Boolean -> Prop m s
muted bool = if bool then mkPair "muted" "muted" else mkNoop

name :: forall m s. String -> Prop m s
name = mkPair "name"

nomodule :: forall m s. Boolean -> Prop m s
nomodule bool = if bool then mkPair "nomodule" "nomodule" else mkNoop

nonce :: forall m s. String -> Prop m s
nonce = mkPair "nonce"

novalidate :: forall m s. Boolean -> Prop m s
novalidate bool = if bool then mkPair "novalidate" "novalidate" else mkNoop

open :: forall m s. Boolean -> Prop m s
open bool = if bool then mkPair "open" "open" else mkNoop

optimum :: forall m s. String -> Prop m s
optimum = mkPair "optimum"

pattern :: forall m s. String -> Prop m s
pattern = mkPair "pattern"

ping :: forall m s. String -> Prop m s
ping = mkPair "ping"

placeholder :: forall m s. String -> Prop m s
placeholder = mkPair "placeholder"

playsinline :: forall m s. Boolean -> Prop m s
playsinline bool = if bool then mkPair "playsinline" "playsinline" else mkNoop

poster :: forall m s. String -> Prop m s
poster = mkPair "poster"

preload :: forall m s. String -> Prop m s
preload = mkPair "preload"

readonly :: forall m s. Boolean -> Prop m s
readonly bool = if bool then mkPair "readonly" "readonly" else mkNoop

referrerpolicy :: forall m s. String -> Prop m s
referrerpolicy = mkPair "referrerpolicy"

rel :: forall m s. String -> Prop m s
rel = mkPair "rel"

required :: forall m s. Boolean -> Prop m s
required bool = if bool then mkPair "required" "required" else mkNoop

reversed :: forall m s. Boolean -> Prop m s
reversed bool = if bool then mkPair "reversed" "reversed" else mkNoop

role :: forall m s. String -> Prop m s
role = mkPair "role"

rows :: forall m s. String -> Prop m s
rows = mkPair "rows"

rowspan :: forall m s. String -> Prop m s
rowspan = mkPair "rowspan"

sandbox :: forall m s. String -> Prop m s
sandbox = mkPair "sandbox"

scope :: forall m s. String -> Prop m s
scope = mkPair "scope"

selected :: forall m s. Boolean -> Prop m s
selected bool = if bool then mkPair "selected" "selected" else mkNoop

shape :: forall m s. String -> Prop m s
shape = mkPair "shape"

size :: forall m s. String -> Prop m s
size = mkPair "size"

sizes :: forall m s. String -> Prop m s
sizes = mkPair "sizes"

slot :: forall m s. String -> Prop m s
slot = mkPair "slot"

span :: forall m s. String -> Prop m s
span = mkPair "span"

spellcheck :: forall m s. String -> Prop m s
spellcheck = mkPair "spellcheck"

src :: forall m s. String -> Prop m s
src = mkPair "src"

srcdoc :: forall m s. String -> Prop m s
srcdoc = mkPair "srcdoc"

srclang :: forall m s. String -> Prop m s
srclang = mkPair "srclang"

srcset :: forall m s. String -> Prop m s
srcset = mkPair "srcset"

start :: forall m s. String -> Prop m s
start = mkPair "start"

step :: forall m s. String -> Prop m s
step = mkPair "step"

style :: forall m s. String -> Prop m s
style = mkPair "style"

tabindex :: forall m s. String -> Prop m s
tabindex = mkPair "tabindex"

target :: forall m s. String -> Prop m s
target = mkPair "target"

title :: forall m s. String -> Prop m s
title = mkPair "title"

translate :: forall m s. String -> Prop m s
translate = mkPair "translate"

type_ :: forall m s. String -> Prop m s
type_ = mkPair "type"

usemap :: forall m s. String -> Prop m s
usemap = mkPair "usemap"

value :: forall m s. String -> Prop m s
value = mkPair "value"

width :: forall m s. String -> Prop m s
width = mkPair "width"

wrap :: forall m s. String -> Prop m s
wrap = mkPair "wrap"

