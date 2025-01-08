module [
    App,
    Html,
    Attribute,
    render_static,
    render_static_without_doc_type,
    translate,
    translate_static,
    text,
    none,
    html,
    base,
    head,
    link,
    meta,
    style,
    title,
    body,
    address,
    article,
    aside,
    footer,
    header,
    h1,
    h2,
    h3,
    h4,
    h5,
    h6,
    main,
    nav,
    section,
    blockquote,
    dd,
    div,
    dl,
    dt,
    figcaption,
    figure,
    hr,
    li,
    menu,
    ol,
    p,
    pre,
    ul,
    a,
    abbr,
    b,
    bdi,
    bdo,
    br,
    cite,
    code,
    data,
    dfn,
    em,
    i,
    kbd,
    mark,
    q,
    rp,
    rt,
    ruby,
    s,
    samp,
    small,
    span,
    strong,
    sub,
    sup,
    time,
    u,
    var,
    wbr,
    area,
    audio,
    img,
    map,
    track,
    video,
    embed,
    iframe,
    object,
    picture,
    portal,
    source,
    svg,
    math,
    canvas,
    noscript,
    script,
    del,
    ins,
    caption,
    col,
    colgroup,
    table,
    tbody,
    td,
    tfoot,
    th,
    thead,
    tr,
    button,
    datalist,
    fieldset,
    form,
    input,
    label,
    legend,
    meter,
    optgroup,
    option,
    output,
    progress,
    select,
    textarea,
    details,
    dialog,
    summary,
    slot,
    template,
]

import Html.Internal.Shared
import Html.Internal.Server

App state init_data : Html.Internal.Shared.App state init_data
Html state : Html.Internal.Shared.Html state
Attribute state : Html.Internal.Shared.Attribute state

element = Html.Internal.Shared.element
text = Html.Internal.Shared.text
none = Html.Internal.Shared.none

translate = Html.Internal.Shared.translate
translate_static = Html.Internal.Shared.translate_static

## Render a static Html node to a string, for saving to disk or sending over a network
##
## The output has no whitespace between nodes, to make it small.
## This is intended for generating full HTML documents, so it
## automatically adds `<!DOCTYPE html>` to the start of the string.
## See also `renderStaticWithoutDocType`.
render_static : Html [] -> Str
render_static = \node ->
    buffer = Str.reserve("<!DOCTYPE html>", Html.Internal.Shared.node_size(node))

    Html.Internal.Server.append_rendered_static(buffer, node)

## Render a Html node to a static string, without a DOCTYPE
render_static_without_doc_type : Html [] -> Str
render_static_without_doc_type = \node ->
    buffer = Str.reserve("", Html.Internal.Shared.node_size(node))

    Html.Internal.Server.append_rendered_static(buffer, node)

html = element("html")
base = element("base")
head = element("head")
link = element("link")
meta = element("meta")
style = element("style")
title = element("title")
body = element("body")
address = element("address")
article = element("article")
aside = element("aside")
footer = element("footer")
header = element("header")
h1 = element("h1")
h2 = element("h2")
h3 = element("h3")
h4 = element("h4")
h5 = element("h5")
h6 = element("h6")
main = element("main")
nav = element("nav")
section = element("section")
blockquote = element("blockquote")
dd = element("dd")
div = element("div")
dl = element("dl")
dt = element("dt")
figcaption = element("figcaption")
figure = element("figure")
hr = element("hr")
li = element("li")
menu = element("menu")
ol = element("ol")
p = element("p")
pre = element("pre")
ul = element("ul")
a = element("a")
abbr = element("abbr")
b = element("b")
bdi = element("bdi")
bdo = element("bdo")
br = element("br")
cite = element("cite")
code = element("code")
data = element("data")
dfn = element("dfn")
em = element("em")
i = element("i")
kbd = element("kbd")
mark = element("mark")
q = element("q")
rp = element("rp")
rt = element("rt")
ruby = element("ruby")
s = element("s")
samp = element("samp")
small = element("small")
span = element("span")
strong = element("strong")
sub = element("sub")
sup = element("sup")
time = element("time")
u = element("u")
var = element("var")
wbr = element("wbr")
area = element("area")
audio = element("audio")
img = element("img")
map = element("map")
track = element("track")
video = element("video")
embed = element("embed")
iframe = element("iframe")
object = element("object")
picture = element("picture")
portal = element("portal")
source = element("source")
svg = element("svg")
math = element("math")
canvas = element("canvas")
noscript = element("noscript")
script = element("script")
del = element("del")
ins = element("ins")
caption = element("caption")
col = element("col")
colgroup = element("colgroup")
table = element("table")
tbody = element("tbody")
td = element("td")
tfoot = element("tfoot")
th = element("th")
thead = element("thead")
tr = element("tr")
button = element("button")
datalist = element("datalist")
fieldset = element("fieldset")
form = element("form")
input = element("input")
label = element("label")
legend = element("legend")
meter = element("meter")
optgroup = element("optgroup")
option = element("option")
output = element("output")
progress = element("progress")
select = element("select")
textarea = element("textarea")
details = element("details")
dialog = element("dialog")
summary = element("summary")
slot = element("slot")
template = element("template")
