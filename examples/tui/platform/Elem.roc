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
        LayoutConfig,
        LayoutDirection,
        Constraint,
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
    Bold,
    Dim,
    Italic,
    Underlined,
    SlowBlink,
    RapidBlink,
    Reversed,
    Hidden,
    CrossedOut,
]

BorderModifier : [
    None,
    Top,
    Right,
    Bottom,
    Left,
    All,
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
    textAlignment : Alignment,
    borders : List BorderModifier,
    borderStyle : Styles,
    borderType : BorderType,
    style : Styles,
}

Span : {text : Str, style: Styles} # A single line string where all graphemes have the same style
Spans : List Span # A string composed of clusters of graphemes, each with their own style.

Elem : [
    Paragraph (List Spans) ParagraphConfig, # A widget to display some text
    Layout (List Elem) LayoutConfig, # Use cassowary-rs solver to split area into smaller ones based on the preferred widths or heights and the direction.
]

LayoutConfig : {
    constraints : List Constraint,
    vMargin : U16,
    hMargin : U16,
    direction : LayoutDirection,
}

LayoutDirection : [Horizontal,Vertical]

Constraint : [
    Percentage U16,
    Ratio U32 U32,
    Length U16,
    Max U16,
    Min U16,
]

