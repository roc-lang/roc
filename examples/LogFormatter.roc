interface LogFormatter
    exposes [
        LogFormatter,
        toBytes,
    ]
    imports [
        Inspect.{
            Formatter,
            Inspector,
        },
    ]

LogFormatter := { bytes : List U8 }
     has [
         Formatter {
             init: init,
             list: list,
             dict: dict,
             tag: tag,
             tuple: tuple,
             record: record,
             bool: bool,
             str: str,
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
init = \{} -> @LogFormatter { bytes: [] }

list : List elem, (elem -> Inspector LogFormatter) -> Inspector LogFormatter
list = \content, toInspector ->
    f0 <- Inspect.custom
    write f0 (Str.toUtf8 "[")
    |> \f1 ->
        (f2, prependSep), elem <- List.walk content (f1, Bool.false)
        f3 =
            if prependSep then
                write f2 (Str.toUtf8 ", ")
            else
                f2

        elem
        |> toInspector
        |> Inspect.apply f3
        |> \f4 -> (f4, Bool.true)
    |> .0
    |> write (Str.toUtf8 "]")

tag : Str, List (Inspector LogFormatter) -> Inspector LogFormatter
tag = \name, fields ->
    if List.isEmpty fields then
        f0 <- Inspect.custom
        write f0 (Str.toUtf8 name)
    else
        f0 <- Inspect.custom
        write f0 (Str.toUtf8 "(")
        |> write (Str.toUtf8 name)
        |> \f1 ->
            f2, inspector <- List.walk fields f1
            write f2 (Str.toUtf8 " ")
            |> \x -> Inspect.apply inspector x
        |> write (Str.toUtf8 ")")

tuple : List (Inspector LogFormatter) -> Inspector LogFormatter
tuple = \fields ->
    f0 <- Inspect.custom
    write f0 (Str.toUtf8 "(")
    |> \f1 ->
        (f2, prependSep), inspector <- List.walk fields (f1, Bool.false)
        f3 =
            if prependSep then
                write f2 (Str.toUtf8 ", ")
            else
                f2

        Inspect.apply inspector f3
        |> \f4 -> (f4, Bool.true)
    |> .0
    |> write (Str.toUtf8 ")")

record : List { key : Str, value : Inspector LogFormatter } -> Inspector LogFormatter
record = \fields ->
    f0 <- Inspect.custom
    write f0 (Str.toUtf8 "{")
    |> \f1 ->
        (f2, prependSep), { key, value } <- List.walk fields (f1, Bool.false)
        f3 =
            if prependSep then
                write f2 (Str.toUtf8 ", ")
            else
                f2

        write f3 (Str.toUtf8 key)
        |> write (Str.toUtf8 ": ")
        |> \x -> Inspect.apply value x
        |> \f4 -> (f4, Bool.true)
    |> .0
    |> write (Str.toUtf8 "}")

dict : dict, Inspect.DictWalkFn (LogFormatter, Bool) dict key value, (key -> Inspector LogFormatter), (value -> Inspector LogFormatter) -> Inspector LogFormatter
dict = \d, walkFn, keyToInspector, valueToInspector ->
    f0 <- Inspect.custom
    write f0 (Str.toUtf8 "{")
    |> \f1 ->
        (f2, prependSep), key, value <- walkFn d (f1, Bool.false)
        f3 =
            if prependSep then
                write f2 (Str.toUtf8 ", ")
            else
                f2

        Inspect.apply (keyToInspector key) f3
        |> write (Str.toUtf8 ": ")
        |> \x -> Inspect.apply (valueToInspector value) x
        |> \f4 -> (f4, Bool.true)
    |> .0
    |> write (Str.toUtf8 "}")

bool : Bool -> Inspector LogFormatter
bool = \b ->
    if b then
        f0 <- Inspect.custom
        write f0 (Str.toUtf8 "true")
    else
        f0 <- Inspect.custom
        write f0 (Str.toUtf8 "false")

str : Str -> Inspector LogFormatter
str = \s ->
    f0 <- Inspect.custom
    f0
    |> write (Str.toUtf8 "\"")
    |> write (Str.toUtf8 s)
    |> write (Str.toUtf8 "\"")

u8 : U8 -> Inspector LogFormatter
u8 = \num ->
    f0 <- Inspect.custom
    write f0 (num |> Num.toStr |> Str.toUtf8)

i8 : I8 -> Inspector LogFormatter
i8 = \num ->
    f0 <- Inspect.custom
    write f0 (num |> Num.toStr |> Str.toUtf8)

u16 : U16 -> Inspector LogFormatter
u16 = \num ->
    f0 <- Inspect.custom
    write f0 (num |> Num.toStr |> Str.toUtf8)

i16 : I16 -> Inspector LogFormatter
i16 = \num ->
    f0 <- Inspect.custom
    write f0 (num |> Num.toStr |> Str.toUtf8)

u32 : U32 -> Inspector LogFormatter
u32 = \num ->
    f0 <- Inspect.custom
    write f0 (num |> Num.toStr |> Str.toUtf8)

i32 : I32 -> Inspector LogFormatter
i32 = \num ->
    f0 <- Inspect.custom
    write f0 (num |> Num.toStr |> Str.toUtf8)

u64 : U64 -> Inspector LogFormatter
u64 = \num ->
    f0 <- Inspect.custom
    write f0 (num |> Num.toStr |> Str.toUtf8)

i64 : I64 -> Inspector LogFormatter
i64 = \num ->
    f0 <- Inspect.custom
    write f0 (num |> Num.toStr |> Str.toUtf8)

u128 : U128 -> Inspector LogFormatter
u128 = \num ->
    f0 <- Inspect.custom
    write f0 (num |> Num.toStr |> Str.toUtf8)

i128 : I128 -> Inspector LogFormatter
i128 = \num ->
    f0 <- Inspect.custom
    write f0 (num |> Num.toStr |> Str.toUtf8)

f32 : F32 -> Inspector LogFormatter
f32 = \num ->
    f0 <- Inspect.custom
    write f0 (num |> Num.toStr |> Str.toUtf8)

f64 : F64 -> Inspector LogFormatter
f64 = \num ->
    f0 <- Inspect.custom
    write f0 (num |> Num.toStr |> Str.toUtf8)

dec : Dec -> Inspector LogFormatter
dec = \num ->
    f0 <- Inspect.custom
    write f0 (num |> Num.toStr |> Str.toUtf8)

write : LogFormatter, List U8 -> LogFormatter
write = \@LogFormatter { bytes }, added ->
    @LogFormatter { bytes: List.concat bytes added }

toBytes : LogFormatter -> List U8
toBytes = \@LogFormatter { bytes } -> bytes
