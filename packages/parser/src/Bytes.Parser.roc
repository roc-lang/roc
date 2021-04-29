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

Parser a :
    [
        @Parser (Bytes -> Result { answer : a, rest : Bytes } RawProblem)
    ]

RawProblem : [ ... ]

keep : Parser a, (a -> Parser b) -> Parser b

skip : Parser *, ({} -> Parser b) -> Parser b

str : Parser Str

u8 : Parser U8
u8 = @Parser Str.parseU8 # TODO doesn't work for bytes!
                         # TODO do we need a separate parser for Str vs Bytes because of encoding?  e.g. Parser.Bytes.utf8

i8 : Parser I8
