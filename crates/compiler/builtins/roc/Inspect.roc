module [
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
    function,
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
    to_inspector,
    to_str,
]

import Bool exposing [Bool]
import Num exposing [U8, U16, U32, U64, U128, I8, I16, I32, I64, I128, F32, F64, Dec]
import List
import Str

KeyValWalker state collection key val : collection, state, (state, key, val -> state) -> state
ElemWalker state collection elem : collection, state, (state, elem -> state) -> state

InspectFormatter implements
    init : () -> f where f implements InspectFormatter

    tag : Str, List (Inspector f) -> Inspector f where f implements InspectFormatter
    tuple : List (Inspector f) -> Inspector f where f implements InspectFormatter
    record : List { key : Str, value : Inspector f } -> Inspector f where f implements InspectFormatter
    bool : Bool -> Inspector f where f implements InspectFormatter
    str : Str -> Inspector f where f implements InspectFormatter

    list : list, ElemWalker state list elem, (elem -> Inspector f) -> Inspector f where f implements InspectFormatter
    set : set, ElemWalker state set elem, (elem -> Inspector f) -> Inspector f where f implements InspectFormatter
    dict : dict, KeyValWalker state dict key value, (key -> Inspector f), (value -> Inspector f) -> Inspector f where f implements InspectFormatter

    # In text, this would render as `<opaque>`
    # TODO: Pass the type name to opaque so that it can be displayed.
    opaque : * -> Inspector f where f implements InspectFormatter

    # In text, this would render as `<function>`
    # TODO: Maybe pass the the function name or signiture to function so that it can be displayed.
    function : * -> Inspector f where f implements InspectFormatter

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
custom = |fn| @Inspector(fn)

apply : Inspector f, f -> f where f implements InspectFormatter
apply = |@Inspector(fn), fmt| fn(fmt)

Inspect implements
    to_inspector : val -> Inspector f where val implements Inspect, f implements InspectFormatter

inspect : val -> f where val implements Inspect, f implements InspectFormatter
inspect = |val|
    @Inspector(val_fn) = to_inspector(val)
    val_fn(init())

to_str : val -> Str where val implements Inspect
to_str = |val|
    val
    |> inspect
    |> to_dbg_str

# The current default formatter for inspect.
# This just returns a simple string for debugging.
# More powerful formatters will likely be wanted in the future.
DbgFormatter := { data : Str }
    implements [
        InspectFormatter {
            init: dbg_init,
            list: dbg_list,
            set: dbg_set,
            dict: dbg_dict,
            tag: dbg_tag,
            tuple: dbg_tuple,
            record: dbg_record,
            bool: dbg_bool,
            str: dbg_str,
            opaque: dbg_opaque,
            function: dbg_function,
            u8: dbg_u8,
            i8: dbg_i8,
            u16: dbg_u16,
            i16: dbg_i16,
            u32: dbg_u32,
            i32: dbg_i32,
            u64: dbg_u64,
            i64: dbg_i64,
            u128: dbg_u128,
            i128: dbg_i128,
            f32: dbg_f32,
            f64: dbg_f64,
            dec: dbg_dec,
        },
    ]

dbg_init : () -> DbgFormatter
dbg_init = || @DbgFormatter({ data: "" })

dbg_list : list, ElemWalker (DbgFormatter, Bool) list elem, (elem -> Inspector DbgFormatter) -> Inspector DbgFormatter
dbg_list = |content, walk_fn, to_dbg_inspector|
    custom_list_dbg = |f0|
        f1 = dbg_write(f0, "[")
        (f5, _) = walk_fn(
            content,
            (f1, Bool.false),
            |(f2, prepend_sep), elem|
                f3 =
                    if prepend_sep then
                        dbg_write(f2, ", ")
                    else
                        f2

                elem
                |> to_dbg_inspector
                |> apply(f3)
                |> |f4| (f4, Bool.true),
        )

        dbg_write(f5, "]")

    custom(custom_list_dbg)

dbg_set : set, ElemWalker (DbgFormatter, Bool) set elem, (elem -> Inspector DbgFormatter) -> Inspector DbgFormatter
dbg_set = |content, walk_fn, to_dbg_inspector|
    custom_dbg_set = |f0|
        f1 = dbg_write(f0, "{")
        (f5, _) = walk_fn(
            content,
            (f1, Bool.false),
            |(f2, prepend_sep), elem|
                f3 =
                    if prepend_sep then
                        dbg_write(f2, ", ")
                    else
                        f2

                elem
                |> to_dbg_inspector
                |> apply(f3)
                |> |f4| (f4, Bool.true),
        )

        dbg_write(f5, "}")

    custom(custom_dbg_set)

dbg_dict : dict, KeyValWalker (DbgFormatter, Bool) dict key value, (key -> Inspector DbgFormatter), (value -> Inspector DbgFormatter) -> Inspector DbgFormatter
dbg_dict = |d, walk_fn, key_to_inspector, value_to_inspector|
    custom_dbg_dict = |f0|
        f1 = dbg_write(f0, "{")
        (f5, _) = walk_fn(
            d,
            (f1, Bool.false),
            |(f2, prepend_sep), key, value|
                f3 =
                    if prepend_sep then
                        dbg_write(f2, ", ")
                    else
                        f2

                apply(key_to_inspector(key), f3)
                |> dbg_write(": ")
                |> |x| apply(value_to_inspector(value), x)
                |> |f4| (f4, Bool.true),
        )

        dbg_write(f5, "}")

    custom(custom_dbg_dict)

dbg_tag : Str, List (Inspector DbgFormatter) -> Inspector DbgFormatter
dbg_tag = |name, fields|
    if List.is_empty(fields) then
        custom(|f0| dbg_write(f0, name))
    else
        custom_dbg_tag = |f0|
            f1 =
                dbg_write(f0, "(")
                |> dbg_write(name)

            f3 = List.walk(
                fields,
                f1,
                |f2, inspector|
                    dbg_write(f2, " ")
                    |> |x| apply(inspector, x),
            )

            dbg_write(f3, ")")

        custom(custom_dbg_tag)

dbg_tuple : List (Inspector DbgFormatter) -> Inspector DbgFormatter
dbg_tuple = |fields|
    custom_dbg_tuple = |f0|
        f1 = dbg_write(f0, "(")
        (f5, _) = List.walk(
            fields,
            (f1, Bool.false),
            |(f2, prepend_sep), inspector|
                f3 =
                    if prepend_sep then
                        dbg_write(f2, ", ")
                    else
                        f2

                apply(inspector, f3)
                |> |f4| (f4, Bool.true),
        )

        dbg_write(f5, ")")

    custom(custom_dbg_tuple)

dbg_record : List { key : Str, value : Inspector DbgFormatter } -> Inspector DbgFormatter
dbg_record = |fields|
    custom_dbg_record = |f0|
        f1 = dbg_write(f0, "{")
        (f5, _) = List.walk(
            fields,
            (f1, Bool.false),
            |(f2, prepend_sep), { key, value }|
                f3 =
                    if prepend_sep then
                        dbg_write(f2, ", ")
                    else
                        f2

                dbg_write(f3, key)
                |> dbg_write(": ")
                |> |x| apply(value, x)
                |> |f4| (f4, Bool.true),
        )

        dbg_write(f5, "}")

    custom(custom_dbg_record)

dbg_bool : Bool -> Inspector DbgFormatter
dbg_bool = |b|
    text = if b then "Bool.true" else "Bool.false"
    custom(|f0| dbg_write(f0, text))

dbg_str : Str -> Inspector DbgFormatter
dbg_str = |s|
    # escape invisible unicode characters as in fmt_str_body crates/compiler/fmt/src/expr.rs
    escape_s =
        Str.replace_each(s, "\u(feff)", "\\u(feff)")
        |> Str.replace_each("\u(200b)", "\\u(200b)")
        |> Str.replace_each("\u(200c)", "\\u(200c)")
        |> Str.replace_each("\u(200d)", "\\u(200d)")

    custom_dbg_str = |f0|
        dbg_write(f0, "\"")
        |> dbg_write(escape_s)
        |> dbg_write("\"")

    custom(custom_dbg_str)

dbg_opaque : * -> Inspector DbgFormatter
dbg_opaque = |_|
    custom(|f0| dbg_write(f0, "<opaque>"))

dbg_function : * -> Inspector DbgFormatter
dbg_function = |_|
    custom(|f0| dbg_write(f0, "<function>"))

dbg_u8 : U8 -> Inspector DbgFormatter
dbg_u8 = |num|
    custom(|f0| dbg_write(f0, Num.to_str(num)))

dbg_i8 : I8 -> Inspector DbgFormatter
dbg_i8 = |num|
    custom(|f0| dbg_write(f0, Num.to_str(num)))

dbg_u16 : U16 -> Inspector DbgFormatter
dbg_u16 = |num|
    custom(|f0| dbg_write(f0, Num.to_str(num)))

dbg_i16 : I16 -> Inspector DbgFormatter
dbg_i16 = |num|
    custom(|f0| dbg_write(f0, Num.to_str(num)))

dbg_u32 : U32 -> Inspector DbgFormatter
dbg_u32 = |num|
    custom(|f0| dbg_write(f0, Num.to_str(num)))

dbg_i32 : I32 -> Inspector DbgFormatter
dbg_i32 = |num|
    custom(|f0| dbg_write(f0, Num.to_str(num)))

dbg_u64 : U64 -> Inspector DbgFormatter
dbg_u64 = |num|
    custom(|f0| dbg_write(f0, Num.to_str(num)))

dbg_i64 : I64 -> Inspector DbgFormatter
dbg_i64 = |num|
    custom(|f0| dbg_write(f0, Num.to_str(num)))

dbg_u128 : U128 -> Inspector DbgFormatter
dbg_u128 = |num|
    custom(|f0| dbg_write(f0, Num.to_str(num)))

dbg_i128 : I128 -> Inspector DbgFormatter
dbg_i128 = |num|
    custom(|f0| dbg_write(f0, Num.to_str(num)))

dbg_f32 : F32 -> Inspector DbgFormatter
dbg_f32 = |num|
    custom(|f0| dbg_write(f0, Num.to_str(num)))

dbg_f64 : F64 -> Inspector DbgFormatter
dbg_f64 = |num|
    custom(|f0| dbg_write(f0, Num.to_str(num)))

dbg_dec : Dec -> Inspector DbgFormatter
dbg_dec = |num|
    custom(|f0| dbg_write(f0, Num.to_str(num)))

dbg_write : DbgFormatter, Str -> DbgFormatter
dbg_write = |@DbgFormatter({ data }), added|
    @DbgFormatter({ data: Str.concat(data, added) })

to_dbg_str : DbgFormatter -> Str
to_dbg_str = |@DbgFormatter({ data })| data
