interface LogFormatter
    exposes [
        LogFormatter,
        toStr,
    ]
    imports [
        Inspect.{
            Formatter,
            Inspector,
        },
    ]

LogFormatter := { data : Str }
     has [
         Formatter {
             init: init,
             list: list,
             set: set,
             dict: dict,
             tag: tag,
             tuple: tuple,
             record: record,
             bool: bool,
             str: str,
             opaque: opaque,
             u8: u8,
             i8: i8,
             u16: u16,
             i16: i16,
             u32: u32,
             i32: i32,
             u64: u64,
             i64: i64,
             u128: u128,
             i128: i128,
             f32: f32,
             f64: f64,
             dec: dec,

         },
     ]

init : {} -> LogFormatter
init = \{} -> @LogFormatter { data: "" }

list : list, Inspect.ElemWalkFn (LogFormatter, Bool) list elem, (elem -> Inspector LogFormatter) -> Inspector LogFormatter
list = \content, walkFn, toInspector ->
    f0 <- Inspect.custom
    write f0 "["
    |> \f1 ->
        (f2, prependSep), elem <- walkFn content (f1, Bool.false)
        f3 =
            if prependSep then
                write f2 ", "
            else
                f2

        elem
        |> toInspector
        |> Inspect.apply f3
        |> \f4 -> (f4, Bool.true)
    |> .0
    |> write "]"

set : set, Inspect.ElemWalkFn (LogFormatter, Bool) set elem, (elem -> Inspector LogFormatter) -> Inspector LogFormatter
set = \content, walkFn, toInspector ->
    f0 <- Inspect.custom
    write f0 "{"
    |> \f1 ->
        (f2, prependSep), elem <- walkFn content (f1, Bool.false)
        f3 =
            if prependSep then
                write f2 ", "
            else
                f2

        elem
        |> toInspector
        |> Inspect.apply f3
        |> \f4 -> (f4, Bool.true)
    |> .0
    |> write "}"

dict : dict, Inspect.KeyValWalkFn (LogFormatter, Bool) dict key value, (key -> Inspector LogFormatter), (value -> Inspector LogFormatter) -> Inspector LogFormatter
dict = \d, walkFn, keyToInspector, valueToInspector ->
    f0 <- Inspect.custom
    write f0 "{"
    |> \f1 ->
        (f2, prependSep), key, value <- walkFn d (f1, Bool.false)
        f3 =
            if prependSep then
                write f2 ", "
            else
                f2

        Inspect.apply (keyToInspector key) f3
        |> write ": "
        |> \x -> Inspect.apply (valueToInspector value) x
        |> \f4 -> (f4, Bool.true)
    |> .0
    |> write "}"

tag : Str, List (Inspector LogFormatter) -> Inspector LogFormatter
tag = \name, fields ->
    if List.isEmpty fields then
        f0 <- Inspect.custom
        write f0 name
    else
        f0 <- Inspect.custom
        write f0 "("
        |> write name
        |> \f1 ->
            f2, inspector <- List.walk fields f1
            write f2 " "
            |> \x -> Inspect.apply inspector x
        |> write ")"

tuple : List (Inspector LogFormatter) -> Inspector LogFormatter
tuple = \fields ->
    f0 <- Inspect.custom
    write f0 "("
    |> \f1 ->
        (f2, prependSep), inspector <- List.walk fields (f1, Bool.false)
        f3 =
            if prependSep then
                write f2 ", "
            else
                f2

        Inspect.apply inspector f3
        |> \f4 -> (f4, Bool.true)
    |> .0
    |> write ")"

record : List { key : Str, value : Inspector LogFormatter } -> Inspector LogFormatter
record = \fields ->
    f0 <- Inspect.custom
    write f0 "{"
    |> \f1 ->
        (f2, prependSep), { key, value } <- List.walk fields (f1, Bool.false)
        f3 =
            if prependSep then
                write f2 ", "
            else
                f2

        write f3 key
        |> write ": "
        |> \x -> Inspect.apply value x
        |> \f4 -> (f4, Bool.true)
    |> .0
    |> write "}"

bool : Bool -> Inspector LogFormatter
bool = \b ->
    if b then
        f0 <- Inspect.custom
        write f0 "true"
    else
        f0 <- Inspect.custom
        write f0 "false"

str : Str -> Inspector LogFormatter
str = \s ->
    f0 <- Inspect.custom
    f0
    |> write "\""
    |> write s
    |> write "\""

opaque : Str -> Inspector LogFormatter
opaque = \s ->
    f0 <- Inspect.custom
    f0
    |> write "<"
    |> write s
    |> write ">"

u8 : U8 -> Inspector LogFormatter
u8 = \num ->
    f0 <- Inspect.custom
    write f0 (num |> Num.toStr)

i8 : I8 -> Inspector LogFormatter
i8 = \num ->
    f0 <- Inspect.custom
    write f0 (num |> Num.toStr)

u16 : U16 -> Inspector LogFormatter
u16 = \num ->
    f0 <- Inspect.custom
    write f0 (num |> Num.toStr)

i16 : I16 -> Inspector LogFormatter
i16 = \num ->
    f0 <- Inspect.custom
    write f0 (num |> Num.toStr)

u32 : U32 -> Inspector LogFormatter
u32 = \num ->
    f0 <- Inspect.custom
    write f0 (num |> Num.toStr)

i32 : I32 -> Inspector LogFormatter
i32 = \num ->
    f0 <- Inspect.custom
    write f0 (num |> Num.toStr)

u64 : U64 -> Inspector LogFormatter
u64 = \num ->
    f0 <- Inspect.custom
    write f0 (num |> Num.toStr)

i64 : I64 -> Inspector LogFormatter
i64 = \num ->
    f0 <- Inspect.custom
    write f0 (num |> Num.toStr)

u128 : U128 -> Inspector LogFormatter
u128 = \num ->
    f0 <- Inspect.custom
    write f0 (num |> Num.toStr)

i128 : I128 -> Inspector LogFormatter
i128 = \num ->
    f0 <- Inspect.custom
    write f0 (num |> Num.toStr)

f32 : F32 -> Inspector LogFormatter
f32 = \num ->
    f0 <- Inspect.custom
    write f0 (num |> Num.toStr)

f64 : F64 -> Inspector LogFormatter
f64 = \num ->
    f0 <- Inspect.custom
    write f0 (num |> Num.toStr)

dec : Dec -> Inspector LogFormatter
dec = \num ->
    f0 <- Inspect.custom
    write f0 (num |> Num.toStr)

write : LogFormatter, Str -> LogFormatter
write = \@LogFormatter { data }, added ->
    @LogFormatter { data: Str.concat data added }

toStr : LogFormatter -> Str
toStr = \@LogFormatter { data } -> data
