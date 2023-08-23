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

InspectFormatter implements
    init : {} -> f where f implements InspectFormatter

    tag : Str, List (Inspector f) -> Inspector f where f implements InspectFormatter
    tuple : List (Inspector f) -> Inspector f where f implements InspectFormatter
    record : List { key : Str, value : Inspector f } -> Inspector f where f implements InspectFormatter
    bool : Bool -> Inspector f where f implements InspectFormatter
    str : Str -> Inspector f where f implements InspectFormatter

    list : list, ElemWalker state list elem, (elem -> Inspector f) -> Inspector f where f implements InspectFormatter
    set : set, ElemWalker state set elem, (elem -> Inspector f) -> Inspector f where f implements InspectFormatter
    dict : dict, KeyValWalker state dict key value, (key -> Inspector f), (value -> Inspector f) -> Inspector f where f implements InspectFormatter

    # Note opaque is used for both opaque types and functions.
    # The auto deriver for functions probably could put the function type.
    # For regular opaque types, I think we can use the type name, though that may lead to some reflection related issues that still need to be discussed.
    # As a simple baseline, it can just use the exact words `opaque` and `function` for now.
    # In text, this would render as `<opaque>`, `<function>`, etc
    opaque : Str -> Inspector f where f implements InspectFormatter

    u8 : U8 -> Inspector f where f implements InspectFormatter
    i8 : I8 -> Inspector f where f implements InspectFormatter
    u16 : U16 -> Inspector f where f implements InspectFormatter
    i16 : I16 -> Inspector f where f implements InspectFormatter
    u32 : U32 -> Inspector f where f implements InspectFormatter
    i32 : I32 -> Inspector f where f implements InspectFormatter
    u64 : U64 -> Inspector f where f implements InspectFormatter
    i64 : I64 -> Inspector f where f implements InspectFormatter
    u128 : U128 -> Inspector f where f implements InspectFormatter
    i128 : I128 -> Inspector f where f implements InspectFormatter
    f32 : F32 -> Inspector f where f implements InspectFormatter
    f64 : F64 -> Inspector f where f implements InspectFormatter
    dec : Dec -> Inspector f where f implements InspectFormatter

Inspector f := f -> f where f implements InspectFormatter

custom : (f -> f) -> Inspector f where f implements InspectFormatter
custom = @Inspector

apply : Inspector f, f -> f where f implements InspectFormatter
apply = \@Inspector fn, fmt -> fn fmt

Inspect implements
    toInspector : val -> Inspector f where val implements Inspect, f implements InspectFormatter

inspect : val -> f where val implements Inspect, f implements InspectFormatter
inspect = \val ->
    (@Inspector valFn) = toInspector val
    valFn (init {})
