##
##
## Parse an [IPv4](https://en.wikipedia.org/wiki/IPv4) address:
##
## parser : Parser Ip4
## parser =
##     a <- keep u8
##     _ <- skip (usv '.')
##     b <- keep u8
##     _ <- skip (usv '.')
##     c <- keep u8
##     _ <- skip (usv '.')
##     d <- keep u8
##
##     Parser.succeed (Ip4.fromOctets a b c d)


interface Parser
    exposes [ Parser ]
    imports []

Parser a := Str -> Result { answer : a, rest : Str } RawProblem

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
                ExactStr Str,
                Grapheme,
                End,
            ]
            Str
    ]

keep : Parser a, (a -> Parser b) -> Parser b
skip : Parser *, ({} -> Parser b) -> Parser b

symbol : Str -> Parser {}
symbol = \symbol -> @Parser Str.chompStr symbol

u8 : Parser U8
u8 = @Parser Str.parseU8

i8 : Parser I8
i8 = @Parser Str.parseI8

end : Parser {}
end = @Parser \str ->
    if Str.isEmpty str then
        Ok {}
    else
        Err (Expected End)

lazy : ({} -> Parser a) -> Parser a
lazy = \thunk ->
    @Parser \str ->
        @Parser parse = thunk {}

        parse str
