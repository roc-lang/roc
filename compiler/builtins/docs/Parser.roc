interface Parser
    exposes [ Parser ]
    imports []


Parser ok err : [ @Parser (List U8 -> Result ok err) ]

Type :
    [
        Bool,
        Utf8,
        Utf16,
        U8,
        I8,
        U16,
        I16,
        U32,
        I32,
        U64,
        I64,
        U128,
        I128,
        OneOrMore
        Type,
        ZeroOrMore
        Type
    ]

# Use cases:
#
# * More readable alternative to regex (e.g. for phone numbers, email addresses)
# * Parsing string serialization formats like JSON, CSV, XML, EDN
# * Parsing string configuration formats like YAML, TOML
# * Parsing programming language source code
#
# This could be used to give Elm-like error messages with "surrounding lines"
# context, by taking the "before" string and the "after" string, splitting them
# into lines, and printing only the last few lines. Is that good enough though?
# Like if the "before" string is *the entire file* up to that point, won't that
# be too much? We could do it by byte, I suppose, but then you're sort of stuck
# guessing. I guess we can do better with a line number - that way, you can just
# split the original string into lines, and you know the index of the line
# on which it happened, so you can just show a few lines before.
# Also, you have (as a bonus, if you like) the entire "before", "after", and
# "attempting" strings.
StrProblem a err : { line : Nat, attempting : Str a, before : Str a, after : Str a }

# Like StrProblem but without the line number, since that doesn't make sense
# with raw bytes. (Byte parsers don't even record this.)
BytesProblem err : { attempting : List U8, before : List U8, after : List U8, err }

parseStr : Parser ok err, Str a -> Result ok (List (StrProblem a err))
parseBytes : Parser ok err, List U8 -> Result ok (List (BytesProblem err))

# Primitives

utf8 : Parser ok err, Utf8 -> Parser ok [ ExpectedUtf8 Utf8 ]err
utf16 : Parser ok err, Utf16 -> Parser ok [ ExpectedUtf16 Utf16 ]err
u8 : Parser ok err, U8 -> Parser ok [ ExpectedU8 U8 ]err
i8 : Parser ok err, I8 -> Parser ok [ ExpectedI8 I8 ]err
u16 : Parser ok err, U16 -> Parser ok [ ExpectedU16 U16 ]err
i16 : Parser ok err, I16 -> Parser ok [ ExpectedI16 I16 ]err
u32 : Parser ok err, U32 -> Parser ok [ ExpectedU32 U32 ]err
i32 : Parser ok err, I32 -> Parser ok [ ExpectedI32 I32 ]err
u64 : Parser ok err, U64 -> Parser ok [ ExpectedU64 U64 ]err
i64 : Parser ok err, I64 -> Parser ok [ ExpectedI64 I64 ]err
u128 : Parser ok err, U128 -> Parser ok [ ExpectedU128 U128 ]err
i128 : Parser ok err, I128 -> Parser ok [ ExpectedI128 I128 ]err

# Collections

oneOrMore : Parser ok err -> Parser (List ok) [ ExpectedOneOrMore Type ]err
zeroOrMore : Parser ok err -> Parser (List ok) err
