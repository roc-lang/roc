interface Inspect
    exposes [
        Formatter,
        init,
        list,
        set,
        dict,
        tag,
        tuple,
        record,
        bool,
        str,
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
        Inspector,
        custom,
        apply,
        Inspect,
        inspect,
        toInspector,
        KeyValWalkFn,
        ElemWalkFn,
    ]
    imports []

KeyValWalkFn state container key value : container, state, (state, key, value -> state) -> state
ElemWalkFn state container elem : container, state, (state, elem -> state) -> state

Formatter has
    init : {} -> f | f has Formatter

    tag : Str, List (Inspector f) -> Inspector f | f has Formatter
    tuple : List (Inspector f) -> Inspector f | f has Formatter
    record : List { key : Str, value : Inspector f } -> Inspector f | f has Formatter
    bool : Bool -> Inspector f | f has Formatter
    str : Str -> Inspector f | f has Formatter

    list : list, ElemWalkFn state list elem, (elem -> Inspector f) -> Inspector f | f has Formatter
    set : set, ElemWalkFn state set elem, (elem -> Inspector f) -> Inspector f | f has Formatter
    dict : dict, KeyValWalkFn state dict key value, (key -> Inspector f), (value -> Inspector f) -> Inspector f | f has Formatter

    u8 : U8 -> Inspector f | f has Formatter
    i8 : I8 -> Inspector f | f has Formatter
    u16 : U16 -> Inspector f | f has Formatter
    i16 : I16 -> Inspector f | f has Formatter
    u32 : U32 -> Inspector f | f has Formatter
    i32 : I32 -> Inspector f | f has Formatter
    u64 : U64 -> Inspector f | f has Formatter
    i64 : I64 -> Inspector f | f has Formatter
    u128 : U128 -> Inspector f | f has Formatter
    i128 : I128 -> Inspector f | f has Formatter
    f32 : F32 -> Inspector f | f has Formatter
    f64 : F64 -> Inspector f | f has Formatter
    dec : Dec -> Inspector f | f has Formatter

Inspector f := f -> f | f has Formatter

custom = @Inspector

apply : Inspector f, f -> f | f has Formatter
apply = \@Inspector fn, fmt -> fn fmt

Inspect has
    toInspector : val -> Inspector f | val has Inspect, f has Formatter

inspect : val -> f | val has Inspect, f has Formatter
inspect = \val ->
    (@Inspector valFn) = toInspector val
    valFn (init {})
