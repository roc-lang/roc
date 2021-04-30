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

Parser a :
    [
        @Parser (Bytes -> Result { answer : a, rest : Bytes } Problem)
    ]

Problem :
    [
        Expected
            [
                NumU8,
                NumI8,
                NumU16,
                NumI16,
                NumU32,
                NumI32,
                NumU64,
                NumI64,
                NumU128,
                NumI128,
                NumF64,
                NumF32,
                Usv U32,
                Utf8 Str,
                Utf16Le Str,
                Utf16Be Str,
                GraphemeUtf8,
                GraphemeUtf16Le,
                GraphemeUtf16Be,
                End,
            ]
            Str
    ]

keep : Parser a, (a -> Parser b) -> Parser b

skip : Parser *, ({} -> Parser b) -> Parser b

utf8 : Parser Str
utf16 : Parser Str

graphemeUtf8 : Parser Str
graphemeUtf16Le : Parser Str
graphemeUtf16Be : Parser Str

usv : Parser U32

u8 : Parser U8
i8 : Parser I8
