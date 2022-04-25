##
##
## Parse an [IPv4](https://en.wikipedia.org/wiki/IPv4) address:
##
##     parser : Parser Ip4
##     parser =
##         a <- keep u8
##         _ <- skip (usv '.')
##         b <- keep u8
##         _ <- skip (usv '.')
##         c <- keep u8
##         _ <- skip (usv '.')
##         d <- keep u8
##
##         Parser.succeed (Ip4.fromOctets a b c d)
interface Parser
    exposes [ Parser ]
    imports []

Parser a := Bytes -> Result { answer : a, rest : Bytes } Problem

Problem :
    [
        Expected
            [
                NumU8,
                NumI8,
                NumU16 Endi,
                NumI16 Endi,
                NumU32 Endi,
                NumI32 Endi,
                NumU64 Endi,
                NumI64 Endi,
                NumU128 Endi,
                NumI128 Endi,
                NumF64 Endi,
                NumF32 Endi,
                Utf8 Str,
                Utf16 Str Endi,
                CodePtUtf8,
                CodePtUtf16 Endi,
                GraphemeUtf8,
                GraphemeUtf16 Endi,
                End,
            ]
            Str
    ]

keep : Parser a, (a -> Parser b) -> Parser b
skip : Parser *, ({} -> Parser b) -> Parser b

utf8 : Str -> Parser Str
utf16 : Str, Endi -> Parser Str
graphemeUtf8 : Parser Str
graphemeUtf16 : Endi -> Parser Str

u8 : Parser U8
i8 : Parser I8
u16 : Endi -> Parser U16
i16 : Endi -> Parser I16
u32 : Endi -> Parser U32
i32 : Endi -> Parser I32
u64 : Endi -> Parser U64
i64 : Endi -> Parser I64
u128 : Endi -> Parser U128
i128 : Endi -> Parser I128
