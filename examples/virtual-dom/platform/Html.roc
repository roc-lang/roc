interface Html
    exposes [
        Node,
        Attribute,
        render,
        renderWithoutDocType,
        tagId,
        text,
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
    imports [Html.Attributes]

Node : [
    Text Str,
    Element Tag Nat (List Attribute) (List Node),
]

# TODO
#
# Node state : [
#     None,
#     Text Str,
#     Element Tag Nat (List (Fact state)) (List (Node state)),
#     CustomElement Str Nat (List (Fact state)) (List (Node state)),
#     Lazy (Result { state, elem : Elem state } [NotCached] -> { state, elem : Elem state }),
# ]
# 
# Fact state : [
#     Event (Handler state),
#     Attribute AttrType Str,
#     CustomAttribute Str Str,
#     Property PropType (List U8),
#     CustomProperty Str (List U8),
#     Style Str Str
# ]

Attribute : Html.Attributes.Attribute

text : Str -> Node
text = Text

## Define an HTML Element
element : Tag -> (List Attribute, List Node -> Node)
element = \tag ->
    \attrs, children ->
        # While building the node tree, calculate the size of Str it will render to
        withTag = 2 * (3 + Str.countUtf8Bytes (tagName tag))
        withAttrs = List.walk attrs withTag \acc, { type, value } ->
            acc
            + Str.countUtf8Bytes (Html.Attributes.attrTypeName type)
            + Str.countUtf8Bytes value
            + 4
        totalSize = List.walk children withAttrs \acc, child ->
            acc + nodeSize child

        Element tag totalSize attrs children

# internal helper
nodeSize : Node -> Nat
nodeSize = \node ->
    when node is
        Text content ->
            Str.countUtf8Bytes content

        Element _ size _ _ ->
            size

## Render a Node to an HTML string
##
## The output has no whitespace between nodes, to make it small.
## This is intended for generating full HTML documents, so it
## automatically adds `<!DOCTYPE html>` to the start of the string.
## See also `renderWithoutDocType`.
render : Node -> Str
render = \node ->
    buffer = Str.reserve "<!DOCTYPE html>" (nodeSize node)

    renderHelp buffer node

## Render a Node to a string, without a DOCTYPE tag
renderWithoutDocType : Node -> Str
renderWithoutDocType = \node ->
    buffer = Str.reserve "" (nodeSize node)

    renderHelp buffer node

# internal helper
renderHelp : Str, Node -> Str
renderHelp = \buffer, node ->
    when node is
        Text content ->
            Str.concat buffer content

        Element tag _ attrs children ->
            name = tagName tag
            withTagName = "\(buffer)<\(name)"
            withAttrs =
                if List.isEmpty attrs then
                    withTagName
                else
                    List.walk attrs "\(withTagName) " renderAttr
            withTag = Str.concat withAttrs ">"
            withChildren = List.walk children withTag renderHelp

            "\(withChildren)</\(name)>"

# internal helper
renderAttr : Str, Attribute -> Str
renderAttr = \buffer, { type, value } ->
    key = Html.Attributes.attrTypeName type

    "\(buffer) \(key)=\"\(value)\""

Tag : [
    Html,
    Base,
    Head,
    Link,
    Meta,
    Style,
    Title,
    Body,
    Address,
    Article,
    Aside,
    Footer,
    Header,
    H1,
    H2,
    H3,
    H4,
    H5,
    H6,
    Main,
    Nav,
    Section,
    Blockquote,
    Dd,
    Div,
    Dl,
    Dt,
    Figcaption,
    Figure,
    Hr,
    Li,
    Menu,
    Ol,
    P,
    Pre,
    Ul,
    A,
    Abbr,
    B,
    Bdi,
    Bdo,
    Br,
    Cite,
    Code,
    Data,
    Dfn,
    Em,
    I,
    Kbd,
    Mark,
    Q,
    Rp,
    Rt,
    Ruby,
    S,
    Samp,
    Small,
    Span,
    Strong,
    Sub,
    Sup,
    Time,
    U,
    Var,
    Wbr,
    Area,
    Audio,
    Img,
    Map,
    Track,
    Video,
    Embed,
    Iframe,
    Object,
    Picture,
    Portal,
    Source,
    Svg,
    Math,
    Canvas,
    Noscript,
    Script,
    Del,
    Ins,
    Caption,
    Col,
    Colgroup,
    Table,
    Tbody,
    Td,
    Tfoot,
    Th,
    Thead,
    Tr,
    Button,
    Datalist,
    Fieldset,
    Form,
    Input,
    Label,
    Legend,
    Meter,
    Optgroup,
    Option,
    Output,
    Progress,
    Select,
    Textarea,
    Details,
    Dialog,
    Summary,
    Slot,
    Template,
]

tagName : Tag -> Str
tagName = \tag ->
    when tag is
        Html -> "html"
        Base -> "base"
        Head -> "head"
        Link -> "link"
        Meta -> "meta"
        Style -> "style"
        Title -> "title"
        Body -> "body"
        Address -> "address"
        Article -> "article"
        Aside -> "aside"
        Footer -> "footer"
        Header -> "header"
        H1 -> "h1"
        H2 -> "h2"
        H3 -> "h3"
        H4 -> "h4"
        H5 -> "h5"
        H6 -> "h6"
        Main -> "main"
        Nav -> "nav"
        Section -> "section"
        Blockquote -> "blockquote"
        Dd -> "dd"
        Div -> "div"
        Dl -> "dl"
        Dt -> "dt"
        Figcaption -> "figcaption"
        Figure -> "figure"
        Hr -> "hr"
        Li -> "li"
        Menu -> "menu"
        Ol -> "ol"
        P -> "p"
        Pre -> "pre"
        Ul -> "ul"
        A -> "a"
        Abbr -> "abbr"
        B -> "b"
        Bdi -> "bdi"
        Bdo -> "bdo"
        Br -> "br"
        Cite -> "cite"
        Code -> "code"
        Data -> "data"
        Dfn -> "dfn"
        Em -> "em"
        I -> "i"
        Kbd -> "kbd"
        Mark -> "mark"
        Q -> "q"
        Rp -> "rp"
        Rt -> "rt"
        Ruby -> "ruby"
        S -> "s"
        Samp -> "samp"
        Small -> "small"
        Span -> "span"
        Strong -> "strong"
        Sub -> "sub"
        Sup -> "sup"
        Time -> "time"
        U -> "u"
        Var -> "var"
        Wbr -> "wbr"
        Area -> "area"
        Audio -> "audio"
        Img -> "img"
        Map -> "map"
        Track -> "track"
        Video -> "video"
        Embed -> "embed"
        Iframe -> "iframe"
        Object -> "object"
        Picture -> "picture"
        Portal -> "portal"
        Source -> "source"
        Svg -> "svg"
        Math -> "math"
        Canvas -> "canvas"
        Noscript -> "noscript"
        Script -> "script"
        Del -> "del"
        Ins -> "ins"
        Caption -> "caption"
        Col -> "col"
        Colgroup -> "colgroup"
        Table -> "table"
        Tbody -> "tbody"
        Td -> "td"
        Tfoot -> "tfoot"
        Th -> "th"
        Thead -> "thead"
        Tr -> "tr"
        Button -> "button"
        Datalist -> "datalist"
        Fieldset -> "fieldset"
        Form -> "form"
        Input -> "input"
        Label -> "label"
        Legend -> "legend"
        Meter -> "meter"
        Optgroup -> "optgroup"
        Option -> "option"
        Output -> "output"
        Progress -> "progress"
        Select -> "select"
        Textarea -> "textarea"
        Details -> "details"
        Dialog -> "dialog"
        Summary -> "summary"
        Slot -> "slot"
        Template -> "template"

# Generated using `roc glue` and some find-and-replace editing
# Should be completely optimized away
tagId : Tag -> U8
tagId = \tag ->
    when tag is
        A -> 0
        Abbr -> 1
        Address -> 2
        Area -> 3
        Article -> 4
        Aside -> 5
        Audio -> 6
        B -> 7
        Base -> 8
        Bdi -> 9
        Bdo -> 10
        Blockquote -> 11
        Body -> 12
        Br -> 13
        Button -> 14
        Canvas -> 15
        Caption -> 16
        Cite -> 17
        Code -> 18
        Col -> 19
        Colgroup -> 20
        Data -> 21
        Datalist -> 22
        Dd -> 23
        Del -> 24
        Details -> 25
        Dfn -> 26
        Dialog -> 27
        Div -> 28
        Dl -> 29
        Dt -> 30
        Em -> 31
        Embed -> 32
        Fieldset -> 33
        Figcaption -> 34
        Figure -> 35
        Footer -> 36
        Form -> 37
        H1 -> 38
        H2 -> 39
        H3 -> 40
        H4 -> 41
        H5 -> 42
        H6 -> 43
        Head -> 44
        Header -> 45
        Hr -> 46
        Html -> 47
        I -> 48
        Iframe -> 49
        Img -> 50
        Input -> 51
        Ins -> 52
        Kbd -> 53
        Label -> 54
        Legend -> 55
        Li -> 56
        Link -> 57
        Main -> 58
        Map -> 59
        Mark -> 60
        Math -> 61
        Menu -> 62
        Meta -> 63
        Meter -> 64
        Nav -> 65
        Noscript -> 66
        Object -> 67
        Ol -> 68
        Optgroup -> 69
        Option -> 70
        Output -> 71
        P -> 72
        Picture -> 73
        Portal -> 74
        Pre -> 75
        Progress -> 76
        Q -> 77
        Rp -> 78
        Rt -> 79
        Ruby -> 80
        S -> 81
        Samp -> 82
        Script -> 83
        Section -> 84
        Select -> 85
        Slot -> 86
        Small -> 87
        Source -> 88
        Span -> 89
        Strong -> 90
        Style -> 91
        Sub -> 92
        Summary -> 93
        Sup -> 94
        Svg -> 95
        Table -> 96
        Tbody -> 97
        Td -> 98
        Template -> 99
        Textarea -> 100
        Tfoot -> 101
        Th -> 102
        Thead -> 103
        Time -> 104
        Title -> 105
        Tr -> 106
        Track -> 107
        U -> 108
        Ul -> 109
        Var -> 110
        Video -> 111
        Wbr -> 112

html = element Html
base = element Base
head = element Head
link = element Link
meta = element Meta
style = element Style
title = element Title
body = element Body
address = element Address
article = element Article
aside = element Aside
footer = element Footer
header = element Header
h1 = element H1
h2 = element H2
h3 = element H3
h4 = element H4
h5 = element H5
h6 = element H6
main = element Main
nav = element Nav
section = element Section
blockquote = element Blockquote
dd = element Dd
div = element Div
dl = element Dl
dt = element Dt
figcaption = element Figcaption
figure = element Figure
hr = element Hr
li = element Li
menu = element Menu
ol = element Ol
p = element P
pre = element Pre
ul = element Ul
a = element A
abbr = element Abbr
b = element B
bdi = element Bdi
bdo = element Bdo
br = element Br
cite = element Cite
code = element Code
data = element Data
dfn = element Dfn
em = element Em
i = element I
kbd = element Kbd
mark = element Mark
q = element Q
rp = element Rp
rt = element Rt
ruby = element Ruby
s = element S
samp = element Samp
small = element Small
span = element Span
strong = element Strong
sub = element Sub
sup = element Sup
time = element Time
u = element U
var = element Var
wbr = element Wbr
area = element Area
audio = element Audio
img = element Img
map = element Map
track = element Track
video = element Video
embed = element Embed
iframe = element Iframe
object = element Object
picture = element Picture
portal = element Portal
source = element Source
svg = element Svg
math = element Math
canvas = element Canvas
noscript = element Noscript
script = element Script
del = element Del
ins = element Ins
caption = element Caption
col = element Col
colgroup = element Colgroup
table = element Table
tbody = element Tbody
td = element Td
tfoot = element Tfoot
th = element Th
thead = element Thead
tr = element Tr
button = element Button
datalist = element Datalist
fieldset = element Fieldset
form = element Form
input = element Input
label = element Label
legend = element Legend
meter = element Meter
optgroup = element Optgroup
option = element Option
output = element Output
progress = element Progress
select = element Select
textarea = element Textarea
details = element Details
dialog = element Dialog
summary = element Summary
slot = element Slot
template = element Template
