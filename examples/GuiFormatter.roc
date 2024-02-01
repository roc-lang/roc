interface GuiFormatter
    exposes [
        GuiFormatter,
        toGui,
    ]

## Creates GUI representations of Roc values, for use in inspect-gui.roc

## This can't depend on the platform, so I just copied all of this.

Rgba : { r : F32, g : F32, b : F32, a : F32 }
ButtonStyles : { bgColor : Rgba, borderColor : Rgba, borderWidth : F32, textColor : Rgba }
Elem : [Button Elem ButtonStyles, Col (List Elem), Row (List Elem), Text Str]

GuiFormatter := { nodes : List Elem }
    implements [
        InspectFormatter {
            init: init,
            list: list,
            set: set,
            dict: dict,
            tag: tag,
            tuple: tuple,
            record: record,
            bool: bool,
            str: str,
            function: function,
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
            nat: nat,
            f32: f32,
            f64: f64,
            dec: dec,

        },
    ]

init : {} -> GuiFormatter
init = \{} -> @GuiFormatter { nodes: [] }

list : list, ElemWalker GuiFormatter list elem, (elem -> Inspector GuiFormatter) -> Inspector GuiFormatter
list = \content, walkFn, toInspector ->
    f0 <- Inspect.custom
    # Use a temporary buffer for the children nodes
    (@GuiFormatter { nodes }) =
        init {}
        |> \f1 ->
            f2, elem <- walkFn content f1
            elem
            |> toInspector
            |> Inspect.apply f2

    addNode f0 (Col nodes)

set : set, ElemWalker GuiFormatter set elem, (elem -> Inspector GuiFormatter) -> Inspector GuiFormatter
set = \content, walkFn, toInspector ->
    f0 <- Inspect.custom
    # Use a temporary buffer for the children nodes
    (@GuiFormatter { nodes }) =
        init {}
        |> \f1 ->
            f2, elem <- walkFn content f1
            elem
            |> toInspector
            |> Inspect.apply f2

    addNode f0 (Col nodes)

dict : dict, KeyValWalker GuiFormatter dict key value, (key -> Inspector GuiFormatter), (value -> Inspector GuiFormatter) -> Inspector GuiFormatter
dict = \d, walkFn, keyToInspector, valueToInspector ->
    f0 <- Inspect.custom
    # Use a temporary buffer for the children nodes
    (@GuiFormatter { nodes }) =
        init {}
        |> \f1 ->
            f2, key, value <- walkFn d f1
            (@GuiFormatter { nodes: innerNodes }) =
                init {}
                |> \x -> Inspect.apply (keyToInspector key) x
                |> addNode (Text ":")
                |> \x -> Inspect.apply (valueToInspector value) x

            addNode f2 (Row innerNodes)

    addNode f0 (Col nodes)

tag : Str, List (Inspector GuiFormatter) -> Inspector GuiFormatter
tag = \name, fields ->
    f0 <- Inspect.custom
    # Use a temporary buffer for the children nodes
    (@GuiFormatter { nodes }) =
        init {}
        |> addNode (Text name)
        |> \f1 ->
            f2, fieldInspector <- List.walk fields f1
            Inspect.apply fieldInspector f2

    addNode f0 (Row nodes)

tuple : List (Inspector GuiFormatter) -> Inspector GuiFormatter
tuple = \fields ->
    f0 <- Inspect.custom
    # Use a temporary buffer for the children nodes
    (@GuiFormatter { nodes }) =
        init {}
        |> \f1 ->
            f2, fieldInspector <- List.walk fields f1
            Inspect.apply fieldInspector f2

    addNode f0 (Row nodes)

record : List { key : Str, value : Inspector GuiFormatter } -> Inspector GuiFormatter
record = \fields ->
    f0 <- Inspect.custom
    # Use a temporary buffer for the children nodes
    (@GuiFormatter { nodes }) =
        init {}
        |> \f1 ->
            f2, { key, value } <- List.walk fields f1
            (@GuiFormatter { nodes: innerNodes }) =
                init {}
                |> addNode (Text key)
                |> addNode (Text ":")
                |> \x -> Inspect.apply value x

            addNode f2 (Row innerNodes)

    addNode f0 (Col nodes)

bool : Bool -> Inspector GuiFormatter
bool = \b ->
    if b then
        f0 <- Inspect.custom
        addNode f0 (Text "true")
    else
        f0 <- Inspect.custom
        addNode f0 (Text "false")

str : Str -> Inspector GuiFormatter
str = \s ->
    f0 <- Inspect.custom
    addNode f0 (Text "\"\(s)\"")

opaque : * -> Inspector GuiFormatter
opaque = \_ ->
    f0 <- Inspect.custom
    addNode f0 (Text "<opaque>")

function : * -> Inspector GuiFormatter
function = \_ ->
    f0 <- Inspect.custom
    addNode f0 (Text "<function>")

u8 : U8 -> Inspector GuiFormatter
u8 = \num ->
    f0 <- Inspect.custom
    addNode f0 (num |> Num.toStr |> Text)

i8 : I8 -> Inspector GuiFormatter
i8 = \num ->
    f0 <- Inspect.custom
    addNode f0 (num |> Num.toStr |> Text)

u16 : U16 -> Inspector GuiFormatter
u16 = \num ->
    f0 <- Inspect.custom
    addNode f0 (num |> Num.toStr |> Text)

i16 : I16 -> Inspector GuiFormatter
i16 = \num ->
    f0 <- Inspect.custom
    addNode f0 (num |> Num.toStr |> Text)

u32 : U32 -> Inspector GuiFormatter
u32 = \num ->
    f0 <- Inspect.custom
    addNode f0 (num |> Num.toStr |> Text)

i32 : I32 -> Inspector GuiFormatter
i32 = \num ->
    f0 <- Inspect.custom
    addNode f0 (num |> Num.toStr |> Text)

u64 : U64 -> Inspector GuiFormatter
u64 = \num ->
    f0 <- Inspect.custom
    addNode f0 (num |> Num.toStr |> Text)

i64 : I64 -> Inspector GuiFormatter
i64 = \num ->
    f0 <- Inspect.custom
    addNode f0 (num |> Num.toStr |> Text)

u128 : U128 -> Inspector GuiFormatter
u128 = \num ->
    f0 <- Inspect.custom
    addNode f0 (num |> Num.toStr |> Text)

i128 : I128 -> Inspector GuiFormatter
i128 = \num ->
    f0 <- Inspect.custom
    addNode f0 (num |> Num.toStr |> Text)

nat : Nat -> Inspector GuiFormatter
nat = \num ->
    f0 <- Inspect.custom
    addNode f0 (num |> Num.toStr |> Text)

f32 : F32 -> Inspector GuiFormatter
f32 = \num ->
    f0 <- Inspect.custom
    addNode f0 (num |> Num.toStr |> Text)

f64 : F64 -> Inspector GuiFormatter
f64 = \num ->
    f0 <- Inspect.custom
    addNode f0 (num |> Num.toStr |> Text)

dec : Dec -> Inspector GuiFormatter
dec = \num ->
    f0 <- Inspect.custom
    addNode f0 (num |> Num.toStr |> Text)

addNode : GuiFormatter, Elem -> GuiFormatter
addNode = \@GuiFormatter { nodes }, node ->
    @GuiFormatter { nodes: List.append nodes node }

toGui : GuiFormatter -> Elem
toGui = \@GuiFormatter { nodes } -> Col nodes
