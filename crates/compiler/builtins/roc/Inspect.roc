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
    toInspector,
    toStr,
]

import Bool exposing [Bool]
import Num exposing [U8, U16, U32, U64, U128, I8, I16, I32, I64, I128, F32, F64, Dec]
import List
import Str

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
custom = \fn -> @Inspector fn

apply : Inspector f, f -> f where f implements InspectFormatter
apply = \@Inspector fn, fmt -> fn fmt

Inspect implements
    toInspector : val -> Inspector f where val implements Inspect, f implements InspectFormatter

inspect : val -> f where val implements Inspect, f implements InspectFormatter
inspect = \val ->
    (@Inspector valFn) = toInspector val
    valFn (init {})

toStr : val -> Str where val implements Inspect
toStr = \val ->
    val
    |> inspect
    |> toDbgStr

# The current default formatter for inspect.
# This just returns a simple string for debugging.
# More powerful formatters will likely be wanted in the future.
DbgFormatter := { data : Str }
    implements [
        InspectFormatter {
            init: dbgInit,
            list: dbgList,
            set: dbgSet,
            dict: dbgDict,
            tag: dbgTag,
            tuple: dbgTuple,
            record: dbgRecord,
            bool: dbgBool,
            str: dbgStr,
            opaque: dbgOpaque,
            function: dbgFunction,
            u8: dbgU8,
            i8: dbgI8,
            u16: dbgU16,
            i16: dbgI16,
            u32: dbgU32,
            i32: dbgI32,
            u64: dbgU64,
            i64: dbgI64,
            u128: dbgU128,
            i128: dbgI128,
            f32: dbgF32,
            f64: dbgF64,
            dec: dbgDec,
        },
    ]

dbgInit : {} -> DbgFormatter
dbgInit = \{} -> @DbgFormatter { data: "" }

dbgList : list, ElemWalker (DbgFormatter, Bool) list elem, (elem -> Inspector DbgFormatter) -> Inspector DbgFormatter
dbgList = \content, walkFn, toDbgInspector ->
    custom \f0 ->
        dbgWrite f0 "["
        |> \f1 ->
            walkFn content (f1, Bool.false) \(f2, prependSep), elem ->
                f3 =
                    if prependSep then
                        dbgWrite f2 ", "
                    else
                        f2

                elem
                |> toDbgInspector
                |> apply f3
                |> \f4 -> (f4, Bool.true)
        |> .0
        |> dbgWrite "]"

dbgSet : set, ElemWalker (DbgFormatter, Bool) set elem, (elem -> Inspector DbgFormatter) -> Inspector DbgFormatter
dbgSet = \content, walkFn, toDbgInspector ->
    custom \f0 ->
        dbgWrite f0 "{"
        |> \f1 ->
            walkFn content (f1, Bool.false) \(f2, prependSep), elem ->
                f3 =
                    if prependSep then
                        dbgWrite f2 ", "
                    else
                        f2

                elem
                |> toDbgInspector
                |> apply f3
                |> \f4 -> (f4, Bool.true)
        |> .0
        |> dbgWrite "}"

dbgDict : dict, KeyValWalker (DbgFormatter, Bool) dict key value, (key -> Inspector DbgFormatter), (value -> Inspector DbgFormatter) -> Inspector DbgFormatter
dbgDict = \d, walkFn, keyToInspector, valueToInspector ->
    custom \f0 ->
        dbgWrite f0 "{"
        |> \f1 ->
            walkFn d (f1, Bool.false) \(f2, prependSep), key, value ->
                f3 =
                    if prependSep then
                        dbgWrite f2 ", "
                    else
                        f2

                apply (keyToInspector key) f3
                |> dbgWrite ": "
                |> \x -> apply (valueToInspector value) x
                |> \f4 -> (f4, Bool.true)
        |> .0
        |> dbgWrite "}"

dbgTag : Str, List (Inspector DbgFormatter) -> Inspector DbgFormatter
dbgTag = \name, fields ->
    if List.isEmpty fields then
        custom \f0 ->
            dbgWrite f0 name
    else
        custom \f0 ->
            dbgWrite f0 "("
            |> dbgWrite name
            |> \f1 ->
                List.walk fields f1 \f2, inspector ->
                    dbgWrite f2 " "
                    |> \x -> apply inspector x
            |> dbgWrite ")"

dbgTuple : List (Inspector DbgFormatter) -> Inspector DbgFormatter
dbgTuple = \fields ->
    custom \f0 ->
        dbgWrite f0 "("
        |> \f1 ->
            List.walk fields (f1, Bool.false) \(f2, prependSep), inspector ->
                f3 =
                    if prependSep then
                        dbgWrite f2 ", "
                    else
                        f2

                apply inspector f3
                |> \f4 -> (f4, Bool.true)
        |> .0
        |> dbgWrite ")"

dbgRecord : List { key : Str, value : Inspector DbgFormatter } -> Inspector DbgFormatter
dbgRecord = \fields ->
    custom \f0 ->
        dbgWrite f0 "{"
        |> \f1 ->
            List.walk fields (f1, Bool.false) \(f2, prependSep), { key, value } ->
                f3 =
                    if prependSep then
                        dbgWrite f2 ", "
                    else
                        f2

                dbgWrite f3 key
                |> dbgWrite ": "
                |> \x -> apply value x
                |> \f4 -> (f4, Bool.true)
        |> .0
        |> dbgWrite "}"

dbgBool : Bool -> Inspector DbgFormatter
dbgBool = \b ->
    if b then
        custom \f0 ->
            dbgWrite f0 "Bool.true"
    else
        custom \f0 ->
            dbgWrite f0 "Bool.false"

dbgStr : Str -> Inspector DbgFormatter
dbgStr = \s ->
    custom \f0 ->
        f0
        |> dbgWrite "\""
        |> \f1 -> 
            # escape invisible unicode characters as in fmt_str_body crates/compiler/fmt/src/expr.rs
            escapeS = Str.replaceEach s "\u(feff)" "\\u(feff)"
                |> Str.replaceEach "\u(200b)" "\\u(200b)" 
                |> Str.replaceEach "\u(200c)" "\\u(200c)"
                |> Str.replaceEach "\u(200d)" "\\u(200d)"
            dbgWrite f1 escapeS
        |> dbgWrite "\""
        

dbgOpaque : * -> Inspector DbgFormatter
dbgOpaque = \_ ->
    custom \f0 ->
        dbgWrite f0 "<opaque>"

dbgFunction : * -> Inspector DbgFormatter
dbgFunction = \_ ->
    custom \f0 ->
        dbgWrite f0 "<function>"

dbgU8 : U8 -> Inspector DbgFormatter
dbgU8 = \num ->
    custom \f0 ->
        dbgWrite f0 (num |> Num.toStr)

dbgI8 : I8 -> Inspector DbgFormatter
dbgI8 = \num ->
    custom \f0 ->
        dbgWrite f0 (num |> Num.toStr)

dbgU16 : U16 -> Inspector DbgFormatter
dbgU16 = \num ->
    custom \f0 ->
        dbgWrite f0 (num |> Num.toStr)

dbgI16 : I16 -> Inspector DbgFormatter
dbgI16 = \num ->
    custom \f0 ->
        dbgWrite f0 (num |> Num.toStr)

dbgU32 : U32 -> Inspector DbgFormatter
dbgU32 = \num ->
    custom \f0 ->
        dbgWrite f0 (num |> Num.toStr)

dbgI32 : I32 -> Inspector DbgFormatter
dbgI32 = \num ->
    custom \f0 ->
        dbgWrite f0 (num |> Num.toStr)

dbgU64 : U64 -> Inspector DbgFormatter
dbgU64 = \num ->
    custom \f0 ->
        dbgWrite f0 (num |> Num.toStr)

dbgI64 : I64 -> Inspector DbgFormatter
dbgI64 = \num ->
    custom \f0 ->
        dbgWrite f0 (num |> Num.toStr)

dbgU128 : U128 -> Inspector DbgFormatter
dbgU128 = \num ->
    custom \f0 ->
        dbgWrite f0 (num |> Num.toStr)

dbgI128 : I128 -> Inspector DbgFormatter
dbgI128 = \num ->
    custom \f0 ->
        dbgWrite f0 (num |> Num.toStr)

dbgF32 : F32 -> Inspector DbgFormatter
dbgF32 = \num ->
    custom \f0 ->
        dbgWrite f0 (num |> Num.toStr)

dbgF64 : F64 -> Inspector DbgFormatter
dbgF64 = \num ->
    custom \f0 ->
        dbgWrite f0 (num |> Num.toStr)

dbgDec : Dec -> Inspector DbgFormatter
dbgDec = \num ->
    custom \f0 ->
        dbgWrite f0 (num |> Num.toStr)

dbgWrite : DbgFormatter, Str -> DbgFormatter
dbgWrite = \@DbgFormatter { data }, added ->
    @DbgFormatter { data: Str.concat data added }

toDbgStr : DbgFormatter -> Str
toDbgStr = \@DbgFormatter { data } -> data
