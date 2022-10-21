interface Html.Attributes
    exposes [
        Attribute,
        attribute,
        attrTypeName,
        attrTypeId,
        accept,
        acceptCharset,
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
        httpEquiv,
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
    imports []

# A single-tag union doesn't work with `roc glue` unless all of its tag union parameters are also single-tag
Attribute : { type : AttrType, value : Str }

attribute : AttrType -> (Str -> Attribute)
attribute = \attrType ->
    \attrValue -> { type: attrType, value: attrValue }

# TODO: data attributes
# dataAttr = \dataName, dataVal -> Attribute "data-\(dataName)" dataVal
AttrType : [
    Accept,
    AcceptCharset,
    Accesskey,
    Action,
    Align,
    Allow,
    Alt,
    Async,
    Autocapitalize,
    Autocomplete,
    Autofocus,
    Autoplay,
    Background,
    Bgcolor,
    Border,
    Buffered,
    Capture,
    Challenge,
    Charset,
    Checked,
    Cite,
    Class,
    Code,
    Codebase,
    Color,
    Cols,
    Colspan,
    Content,
    Contenteditable,
    Contextmenu,
    Controls,
    Coords,
    Crossorigin,
    Csp,
    Data,
    Datetime,
    Decoding,
    Default,
    Defer,
    Dir,
    Dirname,
    Disabled,
    Download,
    Draggable,
    Enctype,
    Enterkeyhint,
    For,
    Form,
    Formaction,
    Formenctype,
    Formmethod,
    Formnovalidate,
    Formtarget,
    Headers,
    Height,
    Hidden,
    High,
    Href,
    Hreflang,
    HttpEquiv,
    Icon,
    Id,
    Importance,
    Integrity,
    Intrinsicsize,
    Inputmode,
    Ismap,
    Itemprop,
    Keytype,
    Kind,
    Label,
    Lang,
    Language,
    Loading,
    List,
    Loop,
    Low,
    Manifest,
    Max,
    Maxlength,
    Minlength,
    Media,
    Method,
    Min,
    Multiple,
    Muted,
    Name,
    Novalidate,
    Open,
    Optimum,
    Pattern,
    Ping,
    Placeholder,
    Poster,
    Preload,
    Radiogroup,
    Readonly,
    Referrerpolicy,
    Rel,
    Required,
    Reversed,
    Role,
    Rows,
    Rowspan,
    Sandbox,
    Scope,
    Scoped,
    Selected,
    Shape,
    Size,
    Sizes,
    Slot,
    Span,
    Spellcheck,
    Src,
    Srcdoc,
    Srclang,
    Srcset,
    Start,
    Step,
    Style,
    Summary,
    Tabindex,
    Target,
    Title,
    Translate,
    Type,
    Usemap,
    Value,
    Width,
    Wrap,
]

accept = attribute Accept
acceptCharset = attribute AcceptCharset
accesskey = attribute Accesskey
action = attribute Action
align = attribute Align
allow = attribute Allow
alt = attribute Alt
async = attribute Async
autocapitalize = attribute Autocapitalize
autocomplete = attribute Autocomplete
autofocus = attribute Autofocus
autoplay = attribute Autoplay
background = attribute Background
bgcolor = attribute Bgcolor
border = attribute Border
buffered = attribute Buffered
capture = attribute Capture
challenge = attribute Challenge
charset = attribute Charset
checked = attribute Checked
cite = attribute Cite
class = attribute Class
code = attribute Code
codebase = attribute Codebase
color = attribute Color
cols = attribute Cols
colspan = attribute Colspan
content = attribute Content
contenteditable = attribute Contenteditable
contextmenu = attribute Contextmenu
controls = attribute Controls
coords = attribute Coords
crossorigin = attribute Crossorigin
csp = attribute Csp
data = attribute Data
datetime = attribute Datetime
decoding = attribute Decoding
default = attribute Default
defer = attribute Defer
dir = attribute Dir
dirname = attribute Dirname
disabled = attribute Disabled
download = attribute Download
draggable = attribute Draggable
enctype = attribute Enctype
enterkeyhint = attribute Enterkeyhint
for = attribute For
form = attribute Form
formaction = attribute Formaction
formenctype = attribute Formenctype
formmethod = attribute Formmethod
formnovalidate = attribute Formnovalidate
formtarget = attribute Formtarget
headers = attribute Headers
height = attribute Height
hidden = attribute Hidden
high = attribute High
href = attribute Href
hreflang = attribute Hreflang
httpEquiv = attribute HttpEquiv
icon = attribute Icon
id = attribute Id
importance = attribute Importance
integrity = attribute Integrity
intrinsicsize = attribute Intrinsicsize
inputmode = attribute Inputmode
ismap = attribute Ismap
itemprop = attribute Itemprop
keytype = attribute Keytype
kind = attribute Kind
label = attribute Label
lang = attribute Lang
language = attribute Language
loading = attribute Loading
list = attribute List
loop = attribute Loop
low = attribute Low
manifest = attribute Manifest
max = attribute Max
maxlength = attribute Maxlength
minlength = attribute Minlength
media = attribute Media
method = attribute Method
min = attribute Min
multiple = attribute Multiple
muted = attribute Muted
name = attribute Name
novalidate = attribute Novalidate
open = attribute Open
optimum = attribute Optimum
pattern = attribute Pattern
ping = attribute Ping
placeholder = attribute Placeholder
poster = attribute Poster
preload = attribute Preload
radiogroup = attribute Radiogroup
readonly = attribute Readonly
referrerpolicy = attribute Referrerpolicy
rel = attribute Rel
required = attribute Required
reversed = attribute Reversed
role = attribute Role
rows = attribute Rows
rowspan = attribute Rowspan
sandbox = attribute Sandbox
scope = attribute Scope
scoped = attribute Scoped
selected = attribute Selected
shape = attribute Shape
size = attribute Size
sizes = attribute Sizes
slot = attribute Slot
span = attribute Span
spellcheck = attribute Spellcheck
src = attribute Src
srcdoc = attribute Srcdoc
srclang = attribute Srclang
srcset = attribute Srcset
start = attribute Start
step = attribute Step
style = attribute Style
summary = attribute Summary
tabindex = attribute Tabindex
target = attribute Target
title = attribute Title
translate = attribute Translate
type = attribute Type
usemap = attribute Usemap
value = attribute Value
width = attribute Width
wrap = attribute Wrap

# Generated using `roc glue` and some find-and-replace editing
# Should be completely optimized away
attrTypeId : AttrType -> U8
attrTypeId = \attr ->
    when attr is
        Accept -> 0
        AcceptCharset -> 1
        Accesskey -> 2
        Action -> 3
        Align -> 4
        Allow -> 5
        Alt -> 6
        Async -> 7
        Autocapitalize -> 8
        Autocomplete -> 9
        Autofocus -> 10
        Autoplay -> 11
        Background -> 12
        Bgcolor -> 13
        Border -> 14
        Buffered -> 15
        Capture -> 16
        Challenge -> 17
        Charset -> 18
        Checked -> 19
        Cite -> 20
        Class -> 21
        Code -> 22
        Codebase -> 23
        Color -> 24
        Cols -> 25
        Colspan -> 26
        Content -> 27
        Contenteditable -> 28
        Contextmenu -> 29
        Controls -> 30
        Coords -> 31
        Crossorigin -> 32
        Csp -> 33
        Data -> 34
        Datetime -> 35
        Decoding -> 36
        Default -> 37
        Defer -> 38
        Dir -> 39
        Dirname -> 40
        Disabled -> 41
        Download -> 42
        Draggable -> 43
        Enctype -> 44
        Enterkeyhint -> 45
        For -> 46
        Form -> 47
        Formaction -> 48
        Formenctype -> 49
        Formmethod -> 50
        Formnovalidate -> 51
        Formtarget -> 52
        Headers -> 53
        Height -> 54
        Hidden -> 55
        High -> 56
        Href -> 57
        Hreflang -> 58
        HttpEquiv -> 59
        Icon -> 60
        Id -> 61
        Importance -> 62
        Inputmode -> 63
        Integrity -> 64
        Intrinsicsize -> 65
        Ismap -> 66
        Itemprop -> 67
        Keytype -> 68
        Kind -> 69
        Label -> 70
        Lang -> 71
        Language -> 72
        List -> 73
        Loading -> 74
        Loop -> 75
        Low -> 76
        Manifest -> 77
        Max -> 78
        Maxlength -> 79
        Media -> 80
        Method -> 81
        Min -> 82
        Minlength -> 83
        Multiple -> 84
        Muted -> 85
        Name -> 86
        Novalidate -> 87
        Open -> 88
        Optimum -> 89
        Pattern -> 90
        Ping -> 91
        Placeholder -> 92
        Poster -> 93
        Preload -> 94
        Radiogroup -> 95
        Readonly -> 96
        Referrerpolicy -> 97
        Rel -> 98
        Required -> 99
        Reversed -> 100
        Role -> 101
        Rows -> 102
        Rowspan -> 103
        Sandbox -> 104
        Scope -> 105
        Scoped -> 106
        Selected -> 107
        Shape -> 108
        Size -> 109
        Sizes -> 110
        Slot -> 111
        Span -> 112
        Spellcheck -> 113
        Src -> 114
        Srcdoc -> 115
        Srclang -> 116
        Srcset -> 117
        Start -> 118
        Step -> 119
        Style -> 120
        Summary -> 121
        Tabindex -> 122
        Target -> 123
        Title -> 124
        Translate -> 125
        Type -> 126
        Usemap -> 127
        Value -> 128
        Width -> 129
        Wrap -> 130

attrTypeName : AttrType -> Str
attrTypeName = \attrType ->
    when attrType is
        Accept -> "accept"
        AcceptCharset -> "accept-charset"
        Accesskey -> "accesskey"
        Action -> "action"
        Align -> "align"
        Allow -> "allow"
        Alt -> "alt"
        Async -> "async"
        Autocapitalize -> "autocapitalize"
        Autocomplete -> "autocomplete"
        Autofocus -> "autofocus"
        Autoplay -> "autoplay"
        Background -> "background"
        Bgcolor -> "bgcolor"
        Border -> "border"
        Buffered -> "buffered"
        Capture -> "capture"
        Challenge -> "challenge"
        Charset -> "charset"
        Checked -> "checked"
        Cite -> "cite"
        Class -> "class"
        Code -> "code"
        Codebase -> "codebase"
        Color -> "color"
        Cols -> "cols"
        Colspan -> "colspan"
        Content -> "content"
        Contenteditable -> "contenteditable"
        Contextmenu -> "contextmenu"
        Controls -> "controls"
        Coords -> "coords"
        Crossorigin -> "crossorigin"
        Csp -> "csp"
        Data -> "data"
        Datetime -> "datetime"
        Decoding -> "decoding"
        Default -> "default"
        Defer -> "defer"
        Dir -> "dir"
        Dirname -> "dirname"
        Disabled -> "disabled"
        Download -> "download"
        Draggable -> "draggable"
        Enctype -> "enctype"
        Enterkeyhint -> "enterkeyhint"
        For -> "for"
        Form -> "form"
        Formaction -> "formaction"
        Formenctype -> "formenctype"
        Formmethod -> "formmethod"
        Formnovalidate -> "formnovalidate"
        Formtarget -> "formtarget"
        Headers -> "headers"
        Height -> "height"
        Hidden -> "hidden"
        High -> "high"
        Href -> "href"
        Hreflang -> "hreflang"
        HttpEquiv -> "http-equiv"
        Icon -> "icon"
        Id -> "id"
        Importance -> "importance"
        Integrity -> "integrity"
        Intrinsicsize -> "intrinsicsize"
        Inputmode -> "inputmode"
        Ismap -> "ismap"
        Itemprop -> "itemprop"
        Keytype -> "keytype"
        Kind -> "kind"
        Label -> "label"
        Lang -> "lang"
        Language -> "language"
        Loading -> "loading"
        List -> "list"
        Loop -> "loop"
        Low -> "low"
        Manifest -> "manifest"
        Max -> "max"
        Maxlength -> "maxlength"
        Minlength -> "minlength"
        Media -> "media"
        Method -> "method"
        Min -> "min"
        Multiple -> "multiple"
        Muted -> "muted"
        Name -> "name"
        Novalidate -> "novalidate"
        Open -> "open"
        Optimum -> "optimum"
        Pattern -> "pattern"
        Ping -> "ping"
        Placeholder -> "placeholder"
        Poster -> "poster"
        Preload -> "preload"
        Radiogroup -> "radiogroup"
        Readonly -> "readonly"
        Referrerpolicy -> "referrerpolicy"
        Rel -> "rel"
        Required -> "required"
        Reversed -> "reversed"
        Role -> "role"
        Rows -> "rows"
        Rowspan -> "rowspan"
        Sandbox -> "sandbox"
        Scope -> "scope"
        Scoped -> "scoped"
        Selected -> "selected"
        Shape -> "shape"
        Size -> "size"
        Sizes -> "sizes"
        Slot -> "slot"
        Span -> "span"
        Spellcheck -> "spellcheck"
        Src -> "src"
        Srcdoc -> "srcdoc"
        Srclang -> "srclang"
        Srcset -> "srcset"
        Start -> "start"
        Step -> "step"
        Style -> "style"
        Summary -> "summary"
        Tabindex -> "tabindex"
        Target -> "target"
        Title -> "title"
        Translate -> "translate"
        Type -> "type"
        Usemap -> "usemap"
        Value -> "value"
        Width -> "width"
        Wrap -> "wrap"
