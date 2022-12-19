-- | This module contains generated code relating to HTML attributes.
-- |
-- | Names are converted from their canonical `kebab-case` form into `camelCase`, which is idiomatic for Purescript. For example, `align-content` becomes `alignContent`. Purescript-reserved words are suffixed by an understore, so `type` becomes `type_`.

module Mation.Gen.Attributes where

import Mation.Core.Prop (Prop, mkPair, mkNoop)


-- | [HTML abbr attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/abbr). This is generated code.
abbr :: forall m s. String -> Prop m s
abbr = mkPair "abbr"

-- | [HTML accept attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/accept). This is generated code.
accept :: forall m s. String -> Prop m s
accept = mkPair "accept"

-- | [HTML accept-charset attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/accept-charset). This is generated code.
acceptCharset :: forall m s. String -> Prop m s
acceptCharset = mkPair "accept-charset"

-- | [HTML accesskey attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/accesskey). This is generated code.
accesskey :: forall m s. String -> Prop m s
accesskey = mkPair "accesskey"

-- | [HTML action attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/action). This is generated code.
action :: forall m s. String -> Prop m s
action = mkPair "action"

-- | [HTML allow attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/allow). This is generated code.
allow :: forall m s. String -> Prop m s
allow = mkPair "allow"

-- | [HTML allowfullscreen attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/allowfullscreen). This is generated code.
allowfullscreen :: forall m s. Boolean -> Prop m s
allowfullscreen bool = if bool then mkPair "allowfullscreen" "allowfullscreen" else mkNoop

-- | [HTML allowpaymentrequest attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/allowpaymentrequest). This is generated code.
allowpaymentrequest :: forall m s. String -> Prop m s
allowpaymentrequest = mkPair "allowpaymentrequest"

-- | [HTML alt attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/alt). This is generated code.
alt :: forall m s. String -> Prop m s
alt = mkPair "alt"

-- | [HTML as attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/as). This is generated code.
as :: forall m s. String -> Prop m s
as = mkPair "as"

-- | [HTML async attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/async). This is generated code.
async :: forall m s. Boolean -> Prop m s
async bool = if bool then mkPair "async" "async" else mkNoop

-- | [HTML autocapitalize attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/autocapitalize). This is generated code.
autocapitalize :: forall m s. String -> Prop m s
autocapitalize = mkPair "autocapitalize"

-- | [HTML autocomplete attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/autocomplete). This is generated code.
autocomplete :: forall m s. String -> Prop m s
autocomplete = mkPair "autocomplete"

-- | [HTML autofocus attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/autofocus). This is generated code.
autofocus :: forall m s. Boolean -> Prop m s
autofocus bool = if bool then mkPair "autofocus" "autofocus" else mkNoop

-- | [HTML autoplay attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/autoplay). This is generated code.
autoplay :: forall m s. Boolean -> Prop m s
autoplay bool = if bool then mkPair "autoplay" "autoplay" else mkNoop

-- | [HTML charset attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/charset). This is generated code.
charset :: forall m s. String -> Prop m s
charset = mkPair "charset"

-- | [HTML checked attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/checked). This is generated code.
checked :: forall m s. Boolean -> Prop m s
checked bool = if bool then mkPair "checked" "checked" else mkNoop

-- | [HTML cite attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/cite). This is generated code.
cite :: forall m s. String -> Prop m s
cite = mkPair "cite"

-- | [HTML class attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/class). This is generated code.
class_ :: forall m s. String -> Prop m s
class_ = mkPair "class"

-- | [HTML color attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/color). This is generated code.
color :: forall m s. String -> Prop m s
color = mkPair "color"

-- | [HTML cols attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/cols). This is generated code.
cols :: forall m s. String -> Prop m s
cols = mkPair "cols"

-- | [HTML colspan attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/colspan). This is generated code.
colspan :: forall m s. String -> Prop m s
colspan = mkPair "colspan"

-- | [HTML content attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/content). This is generated code.
content :: forall m s. String -> Prop m s
content = mkPair "content"

-- | [HTML contenteditable attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/contenteditable). This is generated code.
contenteditable :: forall m s. String -> Prop m s
contenteditable = mkPair "contenteditable"

-- | [HTML controls attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/controls). This is generated code.
controls :: forall m s. Boolean -> Prop m s
controls bool = if bool then mkPair "controls" "controls" else mkNoop

-- | [HTML coords attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/coords). This is generated code.
coords :: forall m s. String -> Prop m s
coords = mkPair "coords"

-- | [HTML crossorigin attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/crossorigin). This is generated code.
crossorigin :: forall m s. String -> Prop m s
crossorigin = mkPair "crossorigin"

-- | [HTML data attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/data). This is generated code.
data_ :: forall m s. String -> Prop m s
data_ = mkPair "data"

-- | [HTML datetime attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/datetime). This is generated code.
datetime :: forall m s. String -> Prop m s
datetime = mkPair "datetime"

-- | [HTML decoding attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/decoding). This is generated code.
decoding :: forall m s. String -> Prop m s
decoding = mkPair "decoding"

-- | [HTML default attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/default). This is generated code.
default :: forall m s. Boolean -> Prop m s
default bool = if bool then mkPair "default" "default" else mkNoop

-- | [HTML defer attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/defer). This is generated code.
defer :: forall m s. Boolean -> Prop m s
defer bool = if bool then mkPair "defer" "defer" else mkNoop

-- | [HTML dir attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/dir). This is generated code.
dir :: forall m s. String -> Prop m s
dir = mkPair "dir"

-- | [HTML dirname attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/dirname). This is generated code.
dirname :: forall m s. String -> Prop m s
dirname = mkPair "dirname"

-- | [HTML disabled attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/disabled). This is generated code.
disabled :: forall m s. Boolean -> Prop m s
disabled bool = if bool then mkPair "disabled" "disabled" else mkNoop

-- | [HTML download attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/download). This is generated code.
download :: forall m s. String -> Prop m s
download = mkPair "download"

-- | [HTML draggable attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/draggable). This is generated code.
draggable :: forall m s. String -> Prop m s
draggable = mkPair "draggable"

-- | [HTML enctype attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/enctype). This is generated code.
enctype :: forall m s. String -> Prop m s
enctype = mkPair "enctype"

-- | [HTML enterkeyhint attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/enterkeyhint). This is generated code.
enterkeyhint :: forall m s. String -> Prop m s
enterkeyhint = mkPair "enterkeyhint"

-- | [HTML for attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/for). This is generated code.
for :: forall m s. String -> Prop m s
for = mkPair "for"

-- | [HTML form attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/form). This is generated code.
form :: forall m s. String -> Prop m s
form = mkPair "form"

-- | [HTML formaction attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/formaction). This is generated code.
formaction :: forall m s. String -> Prop m s
formaction = mkPair "formaction"

-- | [HTML formenctype attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/formenctype). This is generated code.
formenctype :: forall m s. String -> Prop m s
formenctype = mkPair "formenctype"

-- | [HTML formmethod attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/formmethod). This is generated code.
formmethod :: forall m s. String -> Prop m s
formmethod = mkPair "formmethod"

-- | [HTML formnovalidate attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/formnovalidate). This is generated code.
formnovalidate :: forall m s. Boolean -> Prop m s
formnovalidate bool = if bool then mkPair "formnovalidate" "formnovalidate" else mkNoop

-- | [HTML formtarget attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/formtarget). This is generated code.
formtarget :: forall m s. String -> Prop m s
formtarget = mkPair "formtarget"

-- | [HTML headers attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/headers). This is generated code.
headers :: forall m s. String -> Prop m s
headers = mkPair "headers"

-- | [HTML height attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/height). This is generated code.
height :: forall m s. String -> Prop m s
height = mkPair "height"

-- | [HTML hidden attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/hidden). This is generated code.
hidden :: forall m s. String -> Prop m s
hidden = mkPair "hidden"

-- | [HTML high attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/high). This is generated code.
high :: forall m s. String -> Prop m s
high = mkPair "high"

-- | [HTML href attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/href). This is generated code.
href :: forall m s. String -> Prop m s
href = mkPair "href"

-- | [HTML hreflang attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/hreflang). This is generated code.
hreflang :: forall m s. String -> Prop m s
hreflang = mkPair "hreflang"

-- | [HTML http-equiv attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/http-equiv). This is generated code.
httpEquiv :: forall m s. String -> Prop m s
httpEquiv = mkPair "http-equiv"

-- | [HTML id attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/id). This is generated code.
id :: forall m s. String -> Prop m s
id = mkPair "id"

-- | [HTML imagesizes attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/imagesizes). This is generated code.
imagesizes :: forall m s. String -> Prop m s
imagesizes = mkPair "imagesizes"

-- | [HTML imagesrcset attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/imagesrcset). This is generated code.
imagesrcset :: forall m s. String -> Prop m s
imagesrcset = mkPair "imagesrcset"

-- | [HTML inputmode attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/inputmode). This is generated code.
inputmode :: forall m s. String -> Prop m s
inputmode = mkPair "inputmode"

-- | [HTML integrity attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/integrity). This is generated code.
integrity :: forall m s. String -> Prop m s
integrity = mkPair "integrity"

-- | [HTML is attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/is). This is generated code.
is :: forall m s. String -> Prop m s
is = mkPair "is"

-- | [HTML ismap attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/ismap). This is generated code.
ismap :: forall m s. Boolean -> Prop m s
ismap bool = if bool then mkPair "ismap" "ismap" else mkNoop

-- | [HTML itemid attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/itemid). This is generated code.
itemid :: forall m s. String -> Prop m s
itemid = mkPair "itemid"

-- | [HTML itemprop attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/itemprop). This is generated code.
itemprop :: forall m s. String -> Prop m s
itemprop = mkPair "itemprop"

-- | [HTML itemref attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/itemref). This is generated code.
itemref :: forall m s. String -> Prop m s
itemref = mkPair "itemref"

-- | [HTML itemscope attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/itemscope). This is generated code.
itemscope :: forall m s. Boolean -> Prop m s
itemscope bool = if bool then mkPair "itemscope" "itemscope" else mkNoop

-- | [HTML itemtype attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/itemtype). This is generated code.
itemtype :: forall m s. String -> Prop m s
itemtype = mkPair "itemtype"

-- | [HTML kind attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/kind). This is generated code.
kind :: forall m s. String -> Prop m s
kind = mkPair "kind"

-- | [HTML label attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/label). This is generated code.
label :: forall m s. String -> Prop m s
label = mkPair "label"

-- | [HTML lang attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/lang). This is generated code.
lang :: forall m s. String -> Prop m s
lang = mkPair "lang"

-- | [HTML list attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/list). This is generated code.
list :: forall m s. String -> Prop m s
list = mkPair "list"

-- | [HTML loading attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/loading). This is generated code.
loading :: forall m s. String -> Prop m s
loading = mkPair "loading"

-- | [HTML loop attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/loop). This is generated code.
loop :: forall m s. Boolean -> Prop m s
loop bool = if bool then mkPair "loop" "loop" else mkNoop

-- | [HTML low attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/low). This is generated code.
low :: forall m s. String -> Prop m s
low = mkPair "low"

-- | [HTML manifest attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/manifest). This is generated code.
manifest :: forall m s. String -> Prop m s
manifest = mkPair "manifest"

-- | [HTML max attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/max). This is generated code.
max :: forall m s. String -> Prop m s
max = mkPair "max"

-- | [HTML maxlength attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/maxlength). This is generated code.
maxlength :: forall m s. String -> Prop m s
maxlength = mkPair "maxlength"

-- | [HTML media attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/media). This is generated code.
media :: forall m s. String -> Prop m s
media = mkPair "media"

-- | [HTML method attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/method). This is generated code.
method :: forall m s. String -> Prop m s
method = mkPair "method"

-- | [HTML min attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/min). This is generated code.
min :: forall m s. String -> Prop m s
min = mkPair "min"

-- | [HTML minlength attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/minlength). This is generated code.
minlength :: forall m s. String -> Prop m s
minlength = mkPair "minlength"

-- | [HTML multiple attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/multiple). This is generated code.
multiple :: forall m s. Boolean -> Prop m s
multiple bool = if bool then mkPair "multiple" "multiple" else mkNoop

-- | [HTML muted attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/muted). This is generated code.
muted :: forall m s. Boolean -> Prop m s
muted bool = if bool then mkPair "muted" "muted" else mkNoop

-- | [HTML name attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/name). This is generated code.
name :: forall m s. String -> Prop m s
name = mkPair "name"

-- | [HTML nomodule attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/nomodule). This is generated code.
nomodule :: forall m s. Boolean -> Prop m s
nomodule bool = if bool then mkPair "nomodule" "nomodule" else mkNoop

-- | [HTML nonce attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/nonce). This is generated code.
nonce :: forall m s. String -> Prop m s
nonce = mkPair "nonce"

-- | [HTML novalidate attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/novalidate). This is generated code.
novalidate :: forall m s. Boolean -> Prop m s
novalidate bool = if bool then mkPair "novalidate" "novalidate" else mkNoop

-- | [HTML open attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/open). This is generated code.
open :: forall m s. Boolean -> Prop m s
open bool = if bool then mkPair "open" "open" else mkNoop

-- | [HTML optimum attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/optimum). This is generated code.
optimum :: forall m s. String -> Prop m s
optimum = mkPair "optimum"

-- | [HTML pattern attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/pattern). This is generated code.
pattern :: forall m s. String -> Prop m s
pattern = mkPair "pattern"

-- | [HTML ping attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/ping). This is generated code.
ping :: forall m s. String -> Prop m s
ping = mkPair "ping"

-- | [HTML placeholder attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/placeholder). This is generated code.
placeholder :: forall m s. String -> Prop m s
placeholder = mkPair "placeholder"

-- | [HTML playsinline attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/playsinline). This is generated code.
playsinline :: forall m s. Boolean -> Prop m s
playsinline bool = if bool then mkPair "playsinline" "playsinline" else mkNoop

-- | [HTML poster attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/poster). This is generated code.
poster :: forall m s. String -> Prop m s
poster = mkPair "poster"

-- | [HTML preload attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/preload). This is generated code.
preload :: forall m s. String -> Prop m s
preload = mkPair "preload"

-- | [HTML readonly attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/readonly). This is generated code.
readonly :: forall m s. Boolean -> Prop m s
readonly bool = if bool then mkPair "readonly" "readonly" else mkNoop

-- | [HTML referrerpolicy attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/referrerpolicy). This is generated code.
referrerpolicy :: forall m s. String -> Prop m s
referrerpolicy = mkPair "referrerpolicy"

-- | [HTML rel attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/rel). This is generated code.
rel :: forall m s. String -> Prop m s
rel = mkPair "rel"

-- | [HTML required attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/required). This is generated code.
required :: forall m s. Boolean -> Prop m s
required bool = if bool then mkPair "required" "required" else mkNoop

-- | [HTML reversed attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/reversed). This is generated code.
reversed :: forall m s. Boolean -> Prop m s
reversed bool = if bool then mkPair "reversed" "reversed" else mkNoop

-- | [HTML role attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/role). This is generated code.
role :: forall m s. String -> Prop m s
role = mkPair "role"

-- | [HTML rows attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/rows). This is generated code.
rows :: forall m s. String -> Prop m s
rows = mkPair "rows"

-- | [HTML rowspan attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/rowspan). This is generated code.
rowspan :: forall m s. String -> Prop m s
rowspan = mkPair "rowspan"

-- | [HTML sandbox attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/sandbox). This is generated code.
sandbox :: forall m s. String -> Prop m s
sandbox = mkPair "sandbox"

-- | [HTML scope attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/scope). This is generated code.
scope :: forall m s. String -> Prop m s
scope = mkPair "scope"

-- | [HTML selected attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/selected). This is generated code.
selected :: forall m s. Boolean -> Prop m s
selected bool = if bool then mkPair "selected" "selected" else mkNoop

-- | [HTML shape attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/shape). This is generated code.
shape :: forall m s. String -> Prop m s
shape = mkPair "shape"

-- | [HTML size attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/size). This is generated code.
size :: forall m s. String -> Prop m s
size = mkPair "size"

-- | [HTML sizes attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/sizes). This is generated code.
sizes :: forall m s. String -> Prop m s
sizes = mkPair "sizes"

-- | [HTML slot attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/slot). This is generated code.
slot :: forall m s. String -> Prop m s
slot = mkPair "slot"

-- | [HTML span attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/span). This is generated code.
span :: forall m s. String -> Prop m s
span = mkPair "span"

-- | [HTML spellcheck attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/spellcheck). This is generated code.
spellcheck :: forall m s. String -> Prop m s
spellcheck = mkPair "spellcheck"

-- | [HTML src attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/src). This is generated code.
src :: forall m s. String -> Prop m s
src = mkPair "src"

-- | [HTML srcdoc attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/srcdoc). This is generated code.
srcdoc :: forall m s. String -> Prop m s
srcdoc = mkPair "srcdoc"

-- | [HTML srclang attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/srclang). This is generated code.
srclang :: forall m s. String -> Prop m s
srclang = mkPair "srclang"

-- | [HTML srcset attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/srcset). This is generated code.
srcset :: forall m s. String -> Prop m s
srcset = mkPair "srcset"

-- | [HTML start attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/start). This is generated code.
start :: forall m s. String -> Prop m s
start = mkPair "start"

-- | [HTML step attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/step). This is generated code.
step :: forall m s. String -> Prop m s
step = mkPair "step"

-- | [HTML style attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/style). This is generated code.
style :: forall m s. String -> Prop m s
style = mkPair "style"

-- | [HTML tabindex attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/tabindex). This is generated code.
tabindex :: forall m s. String -> Prop m s
tabindex = mkPair "tabindex"

-- | [HTML target attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/target). This is generated code.
target :: forall m s. String -> Prop m s
target = mkPair "target"

-- | [HTML title attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/title). This is generated code.
title :: forall m s. String -> Prop m s
title = mkPair "title"

-- | [HTML translate attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/translate). This is generated code.
translate :: forall m s. String -> Prop m s
translate = mkPair "translate"

-- | [HTML type attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/type). This is generated code.
type_ :: forall m s. String -> Prop m s
type_ = mkPair "type"

-- | [HTML usemap attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/usemap). This is generated code.
usemap :: forall m s. String -> Prop m s
usemap = mkPair "usemap"

-- | [HTML value attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/value). This is generated code.
value :: forall m s. String -> Prop m s
value = mkPair "value"

-- | [HTML width attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/width). This is generated code.
width :: forall m s. String -> Prop m s
width = mkPair "width"

-- | [HTML wrap attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/wrap). This is generated code.
wrap :: forall m s. String -> Prop m s
wrap = mkPair "wrap"

