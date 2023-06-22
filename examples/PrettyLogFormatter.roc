interface PrettyLogFormatter
    exposes [
        PrettyLogFormatter,
        toBytes,
    ]
    imports [
        Inspect.{
            Formatter,
            Inspector,
        },
    ]

PrettyLogFormatter := { bytes : List U8, indents : List U8 }
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

init : {} -> PrettyLogFormatter
init = \{} -> @PrettyLogFormatter { bytes: [], indents: [] }

list : List elem, (elem -> Inspector PrettyLogFormatter) -> Inspector PrettyLogFormatter
list = \content, toInspector ->
    f0 <- Inspect.custom
    write f0 (Str.toUtf8 "[\n")
    |> indent
    |> \f1 ->
        (f2, prependSep), elem <- List.walk content (f1, Bool.false)
        f3 =
            if prependSep then
                write f2 (Str.toUtf8 ",\n")
            else
                f2

        elemInspector = toInspector elem
        f3
        |> writeIndent
        |> \x -> Inspect.apply elemInspector x
        |> \f4 -> (f4, Bool.true)
    |> .0
    |> write (Str.toUtf8 "\n")
    |> outdent
    |> writeIndent
    |> write (Str.toUtf8 "]")

tag : Str, List (Inspector PrettyLogFormatter) -> Inspector PrettyLogFormatter
tag = \name, fields ->
    if List.isEmpty fields then
        f0 <- Inspect.custom
        write f0 (Str.toUtf8 name)
    else
        f0 <- Inspect.custom
        write f0 (Str.toUtf8 "(")
        |> write (Str.toUtf8 name)
        |> indent
        |> \f1 ->
            f2, inspector <- List.walk fields f1
            write f2 (Str.toUtf8 " ")
            |> \x -> Inspect.apply inspector x
        |> outdent
        |> write (Str.toUtf8 ")")

tuple : List (Inspector PrettyLogFormatter) -> Inspector PrettyLogFormatter
tuple = \fields ->
    f0 <- Inspect.custom
    write f0 (Str.toUtf8 "(\n")
    |> indent
    |> \f1 ->
        (f2, prependSep), inspector <- List.walk fields (f1, Bool.false)
        f3 =
            if prependSep then
                write f2 (Str.toUtf8 ",\n")
            else
                f2

        f3
        |> writeIndent
        |> \x -> Inspect.apply inspector x
        |> \f4 -> (f4, Bool.true)
    |> .0
    |> write (Str.toUtf8 "\n")
    |> outdent
    |> write (Str.toUtf8 ")")

record : List { key : Str, value : Inspector PrettyLogFormatter } -> Inspector PrettyLogFormatter
record = \fields ->
    f0 <- Inspect.custom
    write f0 (Str.toUtf8 "{\n")
    |> indent
    |> \f1 ->
        (f2, prependSep), { key, value } <- List.walk fields (f1, Bool.false)
        f3 =
            if prependSep then
                write f2 (Str.toUtf8 ",\n")
            else
                f2

        writeIndent f3
        |> write (Str.toUtf8 key)
        |> write (Str.toUtf8 ": ")
        |> \x -> Inspect.apply value x
        |> \f4 -> (f4, Bool.true)
    |> .0
    |> write (Str.toUtf8 "\n")
    |> outdent
    |> writeIndent
    |> write (Str.toUtf8 "}")

dict : dict, Inspect.DictWalkFn (PrettyLogFormatter, Bool) dict key value, (key -> Inspector PrettyLogFormatter), (value -> Inspector PrettyLogFormatter) -> Inspector PrettyLogFormatter
dict = \d, walkFn, keyToInspector, valueToInspector ->
    f0 <- Inspect.custom
    write f0 (Str.toUtf8 "{\n")
    |> indent
    |> \f1 ->
        (f2, prependSep), key, value <- walkFn d (f1, Bool.false)
        f3 =
            if prependSep then
                write f2 (Str.toUtf8 ",\n")
            else
                f2

        writeIndent f3
        |> \x -> Inspect.apply (keyToInspector key) x
        |> write (Str.toUtf8 ": ")
        |> \x -> Inspect.apply (valueToInspector value) x
        |> \f4 -> (f4, Bool.true)
    |> .0
    |> write (Str.toUtf8 "\n")
    |> outdent
    |> writeIndent
    |> write (Str.toUtf8 "}")

bool : Bool -> Inspector PrettyLogFormatter
bool = \b ->
    if b then
        f0 <- Inspect.custom
        write f0 (Str.toUtf8 "true")
    else
        f0 <- Inspect.custom
        write f0 (Str.toUtf8 "false")

str : Str -> Inspector PrettyLogFormatter
str = \s ->
    f0 <- Inspect.custom
    f0
    |> write (Str.toUtf8 "\"")
    |> write (Str.toUtf8 s)
    |> write (Str.toUtf8 "\"")

u8 : U8 -> Inspector PrettyLogFormatter
u8 = \num ->
    f0 <- Inspect.custom
    write f0 (num |> Num.toStr |> Str.toUtf8)

i8 : I8 -> Inspector PrettyLogFormatter
i8 = \num ->
    f0 <- Inspect.custom
    write f0 (num |> Num.toStr |> Str.toUtf8)

u16 : U16 -> Inspector PrettyLogFormatter
u16 = \num ->
    f0 <- Inspect.custom
    write f0 (num |> Num.toStr |> Str.toUtf8)

i16 : I16 -> Inspector PrettyLogFormatter
i16 = \num ->
    f0 <- Inspect.custom
    write f0 (num |> Num.toStr |> Str.toUtf8)

u32 : U32 -> Inspector PrettyLogFormatter
u32 = \num ->
    f0 <- Inspect.custom
    write f0 (num |> Num.toStr |> Str.toUtf8)

i32 : I32 -> Inspector PrettyLogFormatter
i32 = \num ->
    f0 <- Inspect.custom
    write f0 (num |> Num.toStr |> Str.toUtf8)

u64 : U64 -> Inspector PrettyLogFormatter
u64 = \num ->
    f0 <- Inspect.custom
    write f0 (num |> Num.toStr |> Str.toUtf8)

i64 : I64 -> Inspector PrettyLogFormatter
i64 = \num ->
    f0 <- Inspect.custom
    write f0 (num |> Num.toStr |> Str.toUtf8)

u128 : U128 -> Inspector PrettyLogFormatter
u128 = \num ->
    f0 <- Inspect.custom
    write f0 (num |> Num.toStr |> Str.toUtf8)

i128 : I128 -> Inspector PrettyLogFormatter
i128 = \num ->
    f0 <- Inspect.custom
    write f0 (num |> Num.toStr |> Str.toUtf8)

f32 : F32 -> Inspector PrettyLogFormatter
f32 = \num ->
    f0 <- Inspect.custom
    write f0 (num |> Num.toStr |> Str.toUtf8)

f64 : F64 -> Inspector PrettyLogFormatter
f64 = \num ->
    f0 <- Inspect.custom
    write f0 (num |> Num.toStr |> Str.toUtf8)

dec : Dec -> Inspector PrettyLogFormatter
dec = \num ->
    f0 <- Inspect.custom
    write f0 (num |> Num.toStr |> Str.toUtf8)

write : PrettyLogFormatter, List U8 -> PrettyLogFormatter
write = \@PrettyLogFormatter { bytes, indents }, added ->
    @PrettyLogFormatter { bytes: List.concat bytes added, indents }

writeIndent : PrettyLogFormatter -> PrettyLogFormatter
writeIndent = \@PrettyLogFormatter { bytes, indents } ->
    @PrettyLogFormatter { bytes: List.concat bytes indents, indents }

indent : PrettyLogFormatter -> PrettyLogFormatter
indent = \@PrettyLogFormatter { bytes, indents } ->
    @PrettyLogFormatter { bytes, indents: indents |> List.append ' ' |> List.append ' ' }

outdent : PrettyLogFormatter -> PrettyLogFormatter
outdent = \@PrettyLogFormatter { bytes, indents } ->
    len = List.len indents
    @PrettyLogFormatter { bytes, indents: List.takeFirst indents (len - 2) }

toBytes : PrettyLogFormatter -> List U8
toBytes = \@PrettyLogFormatter { bytes } -> bytes
