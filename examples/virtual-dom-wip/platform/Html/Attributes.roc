module [
    attribute,
    accept,
    accept_charset,
    accesskey,
    action,
    align,
    allow,
    alt,
    async,
    autocapitalize,
    autocomplete,
    autofocus,
    autoplay,
    background,
    bgcolor,
    border,
    buffered,
    capture,
    challenge,
    charset,
    checked,
    cite,
    class,
    code,
    codebase,
    color,
    cols,
    colspan,
    content,
    contenteditable,
    contextmenu,
    controls,
    coords,
    crossorigin,
    csp,
    data,
    # dataAttr, TODO
    datetime,
    decoding,
    default,
    defer,
    dir,
    dirname,
    disabled,
    download,
    draggable,
    enctype,
    enterkeyhint,
    for,
    form,
    formaction,
    formenctype,
    formmethod,
    formnovalidate,
    formtarget,
    headers,
    height,
    hidden,
    high,
    href,
    hreflang,
    http_equiv,
    icon,
    id,
    importance,
    integrity,
    intrinsicsize,
    inputmode,
    ismap,
    itemprop,
    keytype,
    kind,
    label,
    lang,
    language,
    loading,
    list,
    loop,
    low,
    manifest,
    max,
    maxlength,
    minlength,
    media,
    method,
    min,
    multiple,
    muted,
    name,
    novalidate,
    open,
    optimum,
    pattern,
    ping,
    placeholder,
    poster,
    preload,
    radiogroup,
    readonly,
    referrerpolicy,
    rel,
    required,
    reversed,
    role,
    rows,
    rowspan,
    sandbox,
    scope,
    scoped,
    selected,
    shape,
    size,
    sizes,
    slot,
    span,
    spellcheck,
    src,
    srcdoc,
    srclang,
    srcset,
    start,
    step,
    style,
    summary,
    tabindex,
    target,
    title,
    translate,
    type,
    usemap,
    value,
    width,
    wrap,
]

import Html.Internal.Shared exposing [Attribute]

attribute : Str -> (Str -> Attribute state)
attribute = \attr_type ->
    \attr_value -> HtmlAttr(attr_type, attr_value)

accept = attribute("accept")
accept_charset = attribute("acceptCharset")
accesskey = attribute("accesskey")
action = attribute("action")
align = attribute("align")
allow = attribute("allow")
alt = attribute("alt")
async = attribute("async")
autocapitalize = attribute("autocapitalize")
autocomplete = attribute("autocomplete")
autofocus = attribute("autofocus")
autoplay = attribute("autoplay")
background = attribute("background")
bgcolor = attribute("bgcolor")
border = attribute("border")
buffered = attribute("buffered")
capture = attribute("capture")
challenge = attribute("challenge")
charset = attribute("charset")
checked = attribute("checked")
cite = attribute("cite")
class = attribute("class")
code = attribute("code")
codebase = attribute("codebase")
color = attribute("color")
cols = attribute("cols")
colspan = attribute("colspan")
content = attribute("content")
contenteditable = attribute("contenteditable")
contextmenu = attribute("contextmenu")
controls = attribute("controls")
coords = attribute("coords")
crossorigin = attribute("crossorigin")
csp = attribute("csp")
data = attribute("data")
datetime = attribute("datetime")
decoding = attribute("decoding")
default = attribute("default")
defer = attribute("defer")
dir = attribute("dir")
dirname = attribute("dirname")
disabled = attribute("disabled")
download = attribute("download")
draggable = attribute("draggable")
enctype = attribute("enctype")
enterkeyhint = attribute("enterkeyhint")
for = attribute("for")
form = attribute("form")
formaction = attribute("formaction")
formenctype = attribute("formenctype")
formmethod = attribute("formmethod")
formnovalidate = attribute("formnovalidate")
formtarget = attribute("formtarget")
headers = attribute("headers")
height = attribute("height")
hidden = attribute("hidden")
high = attribute("high")
href = attribute("href")
hreflang = attribute("hreflang")
http_equiv = attribute("httpEquiv")
icon = attribute("icon")
id = attribute("id")
importance = attribute("importance")
integrity = attribute("integrity")
intrinsicsize = attribute("intrinsicsize")
inputmode = attribute("inputmode")
ismap = attribute("ismap")
itemprop = attribute("itemprop")
keytype = attribute("keytype")
kind = attribute("kind")
label = attribute("label")
lang = attribute("lang")
language = attribute("language")
loading = attribute("loading")
list = attribute("list")
loop = attribute("loop")
low = attribute("low")
manifest = attribute("manifest")
max = attribute("max")
maxlength = attribute("maxlength")
minlength = attribute("minlength")
media = attribute("media")
method = attribute("method")
min = attribute("min")
multiple = attribute("multiple")
muted = attribute("muted")
name = attribute("name")
novalidate = attribute("novalidate")
open = attribute("open")
optimum = attribute("optimum")
pattern = attribute("pattern")
ping = attribute("ping")
placeholder = attribute("placeholder")
poster = attribute("poster")
preload = attribute("preload")
radiogroup = attribute("radiogroup")
readonly = attribute("readonly")
referrerpolicy = attribute("referrerpolicy")
rel = attribute("rel")
required = attribute("required")
reversed = attribute("reversed")
role = attribute("role")
rows = attribute("rows")
rowspan = attribute("rowspan")
sandbox = attribute("sandbox")
scope = attribute("scope")
scoped = attribute("scoped")
selected = attribute("selected")
shape = attribute("shape")
size = attribute("size")
sizes = attribute("sizes")
slot = attribute("slot")
span = attribute("span")
spellcheck = attribute("spellcheck")
src = attribute("src")
srcdoc = attribute("srcdoc")
srclang = attribute("srclang")
srcset = attribute("srcset")
start = attribute("start")
step = attribute("step")
style = attribute("style")
summary = attribute("summary")
tabindex = attribute("tabindex")
target = attribute("target")
title = attribute("title")
translate = attribute("translate")
type = attribute("type")
usemap = attribute("usemap")
value = attribute("value")
width = attribute("width")
wrap = attribute("wrap")
