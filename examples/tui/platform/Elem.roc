interface Elem
    exposes [
        Elem, 
        Color,
        TextModifier,
        BorderModifier,
        BorderType,
        Alignment,
        Styles,
        ParagraphConfig,
    ]
    imports []


Color : [
    None,
    Black,
    Red,
    Green,
    Yellow,
    Blue,
    Magenta,
    Cyan,
    Gray,
    DarkGray,
    LightRed,
    LightGreen,
    LightYellow,
    LightBlue,
    LightMagenta,
    LightCyan,
    White,
]

TextModifier : [
    BOLD,
    DIM,
    ITALIC,
    UNDERLINED,
    SLOWBLINK,
    RAPIDBLINK,
    REVERSED,
    HIDDEN,
    CROSSEDOUT,
]

BorderModifier : [
    NONE,
    TOP,
    RIGHT,
    BOTTOM,
    LEFT,
    ALL,
]

BorderType : [
    Plain,
    Rounded,
    Double,
    Thick,
]

Alignment : [
    Left,
    Center,
    Right,
]

Styles : {
    fg : Color, 
    bg: Color, 
    modifiers : List TextModifier,
}

ParagraphConfig : {
    title : Str,
    titleStyle : Styles,
    titleAlignment : Alignment,
    borders : List BorderModifier,
    borderStyle : Styles,
    borderType : BorderType,
    style : Styles,
}

Span : {text : Str, style: Styles} # A single line string where all graphemes have the same style
Spans : List Span # A string composed of clusters of graphemes, each with their own style.

Elem : [
    Paragraph (List Spans) ParagraphConfig, # A widget to display some text
]