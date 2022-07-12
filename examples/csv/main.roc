app "main"
    packages { pf: "platform/main.roc" }
    imports []
    provides [main] to pf

# Use after free. List has 24 elems.
# Making list 1 elem shorter resolves problem (short string optimization?)
main =
    [65u8,65u8,65u8,65u8,65u8,65u8,65u8,65u8,65u8,65u8,65u8,65u8,65u8,65u8,65u8,65u8,65u8,65u8,65u8,65u8,65u8,65u8,65u8,65u8,]
    |> Str.fromUtf8
    |> Result.withDefault "(not used)"
