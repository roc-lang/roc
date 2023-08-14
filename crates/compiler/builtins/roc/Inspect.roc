interface Inspect
    exposes [
        Inspect,
        Inspector,
        InspectFormatter,
        ElemWalker,
        KeyValWalker,
        inspect,
        init,
        list,
        set,
        dict,
        tag,
        tuple,
        record,
        bool,
        str,
        opaque,
        u8,
        i8,
        u16,
        i16,
        u32,
        i32,
        u64,
        i64,
        u128,
        i128,
        f32,
        f64,
        dec,
        custom,
        apply,
        toInspector,
    ]
    imports [
        Bool.{ Bool },
        Num.{ U8, U16, U32, U64, U128, I8, I16, I32, I64, I128, F32, F64, Dec },
        List,
        Str,
    ]

KeyValWalker state collection key val : collection, state, (state, key, val -> state) -> state
ElemWalker state collection elem : collection, state, (state, elem -> state) -> state

InspectFormatter has
    init : {} -> f | f has InspectFormatter

    tag : Str, List (Inspector f) -> Inspector f | f has InspectFormatter
    tuple : List (Inspector f) -> Inspector f | f has InspectFormatter
    record : List { key : Str, value : Inspector f } -> Inspector f | f has InspectFormatter
    bool : Bool -> Inspector f | f has InspectFormatter
    str : Str -> Inspector f | f has InspectFormatter

    list : list, ElemWalker state list elem, (elem -> Inspector f) -> Inspector f | f has InspectFormatter
    set : set, ElemWalker state set elem, (elem -> Inspector f) -> Inspector f | f has InspectFormatter
    dict : dict, KeyValWalker state dict key value, (key -> Inspector f), (value -> Inspector f) -> Inspector f | f has InspectFormatter

    # Note opaque is used for both opaque types and functions.
    # The auto deriver for functions probably could put the function type.
    # For regular opaque types, I think we can use the type name, though that may lead to some reflection related issues that still need to be discussed.
    # As a simple baseline, it can just use the exact words `opaque` and `function` for now.
    # In text, this would render as `<opaque>`, `<function>`, etc
    opaque : Str -> Inspector f | f has InspectFormatter

    u8 : U8 -> Inspector f | f has InspectFormatter
    i8 : I8 -> Inspector f | f has InspectFormatter
    u16 : U16 -> Inspector f | f has InspectFormatter
    i16 : I16 -> Inspector f | f has InspectFormatter
    u32 : U32 -> Inspector f | f has InspectFormatter
    i32 : I32 -> Inspector f | f has InspectFormatter
    u64 : U64 -> Inspector f | f has InspectFormatter
    i64 : I64 -> Inspector f | f has InspectFormatter
    u128 : U128 -> Inspector f | f has InspectFormatter
    i128 : I128 -> Inspector f | f has InspectFormatter
    f32 : F32 -> Inspector f | f has InspectFormatter
    f64 : F64 -> Inspector f | f has InspectFormatter
    dec : Dec -> Inspector f | f has InspectFormatter

Inspector f := f -> f | f has InspectFormatter

custom : (f -> f) -> Inspector f | f has InspectFormatter
custom = @Inspector

apply : Inspector f, f -> f | f has InspectFormatter
apply = \@Inspector fn, fmt -> fn fmt

Inspect has
    toInspector : val -> Inspector f | val has Inspect, f has InspectFormatter

inspect : val -> f | val has Inspect, f has InspectFormatter
inspect = \val ->
    (@Inspector valFn) = toInspector val
    valFn (init {})
