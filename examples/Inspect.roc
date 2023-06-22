interface Inspect
    exposes [
        Formatter,
        init,
        list,
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
        DictWalkFn,
    ]
    imports []

DictWalkFn state dict key value : dict, state, (state, key, value -> state) -> state

Formatter has
    init : {} -> f | f has Formatter

    list : List elem, (elem -> Inspector f) -> Inspector f | f has Formatter
    tag : Str, List (Inspector f) -> Inspector f | f has Formatter
    tuple : List (Inspector f) -> Inspector f | f has Formatter
    record : List { key : Str, value : Inspector f } -> Inspector f | f has Formatter
    bool : Bool -> Inspector f | f has Formatter
    str : Str -> Inspector f | f has Formatter

    # I am not yet sold on this function. Is there a better api.
    # Specifying the walk function makes this feel very verbose.
    # This is needed to make dicts special so that can print like giant records instead of lists of tuples.
    dict : dict, DictWalkFn state dict key value, (key -> Inspector f), (value -> Inspector f) -> Inspector f | f has Formatter

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
