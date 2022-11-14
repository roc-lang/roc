interface Str
    exposes [
        Utf8Problem,
        Utf8ByteProblem,
        concat,
        isEmpty,
        joinWith,
        split,
        repeat,
        countGraphemes,
        countUtf8Bytes,
        startsWithScalar,
        toUtf8,
        fromUtf8,
        fromUtf8Range,
        startsWith,
        endsWith,
        trim,
        trimLeft,
        trimRight,
        toDec,
        toF64,
        toF32,
        toNat,
        toU128,
        toI128,
        toU64,
        toI64,
        toU32,
        toI32,
        toU16,
        toI16,
        toU8,
        toI8,
        toScalars,
        replaceEach,
        replaceFirst,
        replaceLast,
        splitFirst,
        splitLast,
        walkUtf8WithIndex,
        reserve,
        appendScalar,
        walkScalars,
        walkScalarsUntil,
        withCapacity,
        withPrefix,
        graphemes,
    ]
    imports [
        Bool.{ Bool, Eq },
        Result.{ Result },
        List,
        Num.{ Nat, Num, U8, U16, U32, U64, U128, I8, I16, I32, I64, I128, F32, F64, Dec },
    ]

## # Types
##
## Dealing with text is a deep topic, so by design, Roc's `Str` module sticks
## to the basics.
##
## ### Unicode
##
## Unicode can represent text values which span multiple languages, symbols, and emoji.
## Here are some valid Roc strings:
##
## "Roc!"
## "鹏"
## "🕊"
##
## Every Unicode string is a sequence of [extended grapheme clusters](http://www.unicode.org/glossary/#extended_grapheme_cluster).
## An extended grapheme cluster represents what a person reading a string might
## call a "character" - like "A" or "ö" or "👩‍👩‍👦‍👦".
## Because the term "character" means different things in different areas of
## programming, and "extended grapheme cluster" is a mouthful, in Roc we use the
## term "grapheme" as a shorthand for the more precise "extended grapheme cluster."
##
## You can get the number of graphemes in a string by calling `Str.countGraphemes` on it:
##
##     Str.countGraphemes "Roc!"
##     Str.countGraphemes "折り紙"
##     Str.countGraphemes "🕊"
##
## > The `countGraphemes` function walks through the entire string to get its answer,
## > so if you want to check whether a string is empty, you'll get much better performance
## > by calling `Str.isEmpty myStr` instead of `Str.countGraphemes myStr == 0`.
##
## ### Escape sequences
##
## If you put a `\` in a Roc string literal, it begins an *escape sequence*.
## An escape sequence is a convenient way to insert certain strings into other strings.
## For example, suppose you write this Roc string:
##
##     "I took the one less traveled by,\nAnd that has made all the difference."
##
## The `"\n"` in the middle will insert a line break into this string. There are
## other ways of getting a line break in there, but `"\n"` is the most common.
##
## Another way you could insert a newlines is by writing `\u{0x0A}` instead of `\n`.
## That would result in the same string, because the `\u` escape sequence inserts
## [Unicode code points](https://unicode.org/glossary/#code_point) directly into
## the string. The Unicode code point 10 is a newline, and 10 is `0A` in hexadecimal.
## `0x0A` is a Roc hexadecimal literal, and `\u` escape sequences are always
## followed by a hexadecimal literal inside `{` and `}` like this.
##
## As another example, `"R\u{0x6F}c"` is the same string as `"Roc"`, because
## `"\u{0x6F}"` corresponds to the Unicode code point for lowercase `o`. If you
## want to [spice things up a bit](https://en.wikipedia.org/wiki/Metal_umlaut),
## you can write `"R\u{0xF6}c"` as an alternative way to get the string `"Röc"\.
##
## Roc strings also support these escape sequences:
##
## * `\\` - an actual backslash (writing a single `\` always begins an escape sequence!)
## * `\"` - an actual quotation mark (writing a `"` without a `\` ends the string)
## * `\r` - [carriage return](https://en.wikipedia.org/wiki/Carriage_Return)
## * `\t` - [horizontal tab](https://en.wikipedia.org/wiki/Tab_key#Tab_characters)
## * `\v` - [vertical tab](https://en.wikipedia.org/wiki/Tab_key#Tab_characters)
##
## You can also use escape sequences to insert named strings into other strings, like so:
##
##     name = "Lee"
##     city = "Roctown"
##
##     greeting = "Hello there, \(name)! Welcome to \(city)."
##
## Here, `greeting` will become the string `"Hello there, Lee! Welcome to Roctown."`.
## This is known as [string interpolation](https://en.wikipedia.org/wiki/String_interpolation),
## and you can use it as many times as you like inside a string. The name
## between the parentheses must refer to a `Str` value that is currently in
## scope, and it must be a name - it can't be an arbitrary expression like a function call.
Utf8ByteProblem : [
    InvalidStartByte,
    UnexpectedEndOfSequence,
    ExpectedContinuation,
    OverlongEncoding,
    CodepointTooLarge,
    EncodesSurrogateHalf,
]

Utf8Problem : { byteIndex : Nat, problem : Utf8ByteProblem }

## Returns [Bool.true] if the string is empty, and [Bool.false] otherwise.
##
##     expect Str.isEmpty "hi!" == Bool.false
##     expect Str.isEmpty "" == Bool.true
isEmpty : Str -> Bool

## Concatenates two strings together.
##
##     expect Str.concat "ab" "cd" == "abcd"
##     expect Str.concat "hello" "" == "hello"
##     expect Str.concat "" "" == ""
concat : Str, Str -> Str

## Returns a string of the specified capacity without any content.
withCapacity : Nat -> Str

## Combines a [List] of strings into a single string, with a separator
## string in between each.
##
##     expect Str.joinWith ["one", "two", "three"] ", " == "one, two, three"
##     expect Str.joinWith ["1", "2", "3", "4"] "." == "1.2.3.4"
joinWith : List Str, Str -> Str

## Split a string around a separator.
##
## Passing `""` for the separator is not useful;
## it returns the original string wrapped in a [List]. To split a string
## into its individual [graphemes](https://stackoverflow.com/a/27331885/4200103), use `Str.graphemes`
##
##     expect Str.split "1,2,3" "," == ["1","2","3"]
##     expect Str.split "1,2,3" "" == ["1,2,3"]
split : Str, Str -> List Str

## Repeats a string the given number of times.
##
##     expect Str.repeat "z" 3 == "zzz"
##     expect Str.repeat "na" 8 == "nananananananana"
##
## Returns `""` when given `""` for the string or `0` for the count.
##
##     expect Str.repeat "" 10 == ""
##     expect Str.repeat "anything" 0 == ""
repeat : Str, Nat -> Str

## Counts the number of [extended grapheme clusters](http://www.unicode.org/glossary/#extended_grapheme_cluster)
## in the string.
##
## Note that the number of extended grapheme clusters can be different from the number
## of visual glyphs rendered! Consider the following examples:
##
##     expect Str.countGraphemes "Roc" == 3
##     expect Str.countGraphemes "👩‍👩‍👦‍👦"  == 4
##     expect Str.countGraphemes "🕊"  == 1
##
## Note that "👩‍👩‍👦‍👦" takes up 4 graphemes (even though visually it appears as a single
## glyph) because under the hood it's represented using an emoji modifier sequence.
## In contrast, "🕊" only takes up 1 grapheme because under the hood it's represented
## using a single Unicode code point.
countGraphemes : Str -> Nat

## Split a string into its constituent grapheme clusters
graphemes : Str -> List Str

## If the string begins with a [Unicode code point](http://www.unicode.org/glossary/#code_point)
## equal to the given [U32], returns [Bool.true]. Otherwise returns [Bool.false].
##
## If the given string is empty, or if the given [U32] is not a valid
## code point, returns [Bool.false].
##
##     expect Str.startsWithScalar "鹏 means 'roc'" 40527 # "鹏" is Unicode scalar 40527
##     expect !Str.startsWithScalar "9" 9 # the Unicode scalar for "9" is 57, not 9
##     expect !Str.startsWithScalar "" 40527
##
## **Performance Note:** This runs slightly faster than [Str.startsWith], so
## if you want to check whether a string begins with something that's representable
## in a single code point, you can use (for example) `Str.startsWithScalar '鹏'`
## instead of `Str.startsWith "鹏"`. ('鹏' evaluates to the [U32] value `40527`.)
## This will not work for graphemes which take up multiple code points, however;
## `Str.startsWithScalar '👩‍👩‍👦‍👦'` would be a compiler error because 👩‍👩‍👦‍👦 takes up
## multiple code points and cannot be represented as a single [U32].
## You'd need to use `Str.startsWithScalar "🕊"` instead.
startsWithScalar : Str, U32 -> Bool

## Returns a [List] of the [Unicode scalar values](https://unicode.org/glossary/#unicode_scalar_value)
## in the given string.
##
## (Roc strings contain only scalar values, not [surrogate code points](https://unicode.org/glossary/#surrogate_code_point),
## so this is equivalent to returning a list of the string's [code points](https://unicode.org/glossary/#code_point).)
##
##     expect Str.toScalars "Roc" == [82, 111, 99]
##     expect Str.toScalars "鹏" == [40527]
##     expect Str.toScalars "சி" == [2970, 3007]
##     expect Str.toScalars "🐦" == [128038]
##     expect Str.toScalars "👩‍👩‍👦‍👦" == [128105, 8205, 128105, 8205, 128102, 8205, 128102]
##     expect Str.toScalars "I ♥ Roc" == [73, 32, 9829, 32, 82, 111, 99]
##     expect Str.toScalars "" == []
toScalars : Str -> List U32

## Returns a [List] of the string's [U8] UTF-8 [code units](https://unicode.org/glossary/#code_unit).
## (To split the string into a [List] of smaller [Str] values instead of [U8] values,
## see [Str.split].)
##
##     expect Str.toUtf8 "Roc" == [82, 111, 99]
##     expect Str.toUtf8 "鹏" == [233, 185, 143]
##     expect Str.toUtf8 "சி" == [224, 174, 154, 224, 174, 191]
##     expect Str.toUtf8 "🐦" == [240, 159, 144, 166]
toUtf8 : Str -> List U8

## Converts a [List] of [U8] UTF-8 [code units](https://unicode.org/glossary/#code_unit) to a string.
##
## Returns `Err` if the given bytes are invalid UTF-8, and returns `Ok ""` when given `[]`.
##
##     expect Str.fromUtf8 [82, 111, 99] == Ok "Roc"
##     expect Str.fromUtf8 [233, 185, 143] == Ok "鹏"
##     expect Str.fromUtf8 [224, 174, 154, 224, 174, 191] == Ok "சி"
##     expect Str.fromUtf8 [240, 159, 144, 166] == Ok "🐦"
##     expect Str.fromUtf8 [] == Ok ""
##     expect Str.fromUtf8 [255] |> Result.isErr
fromUtf8 : List U8 -> Result Str [BadUtf8 Utf8ByteProblem Nat]
fromUtf8 = \bytes ->
    result = fromUtf8RangeLowlevel bytes 0 (List.len bytes)

    if result.cIsOk then
        Ok result.bString
    else
        Err (BadUtf8 result.dProblemCode result.aByteIndex)

## Encode part of a [List] of [U8] UTF-8 [code units](https://unicode.org/glossary/#code_unit)
## into a [Str]
##
##     expect Str.fromUtf8Range [72, 105, 80, 103] { start : 0, count : 2 } == Ok "Hi"
fromUtf8Range : List U8, { start : Nat, count : Nat } -> Result Str [BadUtf8 Utf8ByteProblem Nat, OutOfBounds]
fromUtf8Range = \bytes, config ->
    if config.start + config.count <= List.len bytes then
        result = fromUtf8RangeLowlevel bytes config.start config.count

        if result.cIsOk then
            Ok result.bString
        else
            Err (BadUtf8 result.dProblemCode result.aByteIndex)
    else
        Err OutOfBounds

FromUtf8Result : {
    aByteIndex : Nat,
    bString : Str,
    cIsOk : Bool,
    dProblemCode : Utf8ByteProblem,
}

fromUtf8RangeLowlevel : List U8, Nat, Nat -> FromUtf8Result

## Check if the given [Str] starts with a value.
##
##     expect Str.startsWith "ABC" "A" == Bool.true
##     expect Str.startsWith "ABC" "X" == Bool.false
startsWith : Str, Str -> Bool

## Check if the given [Str] ends with a value.
##
##     expect Str.endsWith "ABC" "C" == Bool.true
##     expect Str.endsWith "ABC" "X" == Bool.false
endsWith : Str, Str -> Bool

## Return the [Str] with all whitespace removed from both the beginning
## as well as the end.
##
##     expect Str.trim "   Hello      \n\n" == "Hello"
trim : Str -> Str

## Return the [Str] with all whitespace removed from the beginning.
##
##     expect Str.trimLeft "   Hello      \n\n" == "Hello      \n\n"
trimLeft : Str -> Str

## Return the [Str] with all whitespace removed from the end.
##
##      expect Str.trimRight "   Hello      \n\n" == "   Hello"
trimRight : Str -> Str

## Encode a [Str] to a [Dec]. A [Dec] value is a 128-bit decimal
## [fixed-point number](https://en.wikipedia.org/wiki/Fixed-point_arithmetic).
##
##     expect Str.toDec "10" == Ok 10dec
##     expect Str.toDec "-0.25" == Ok -0.25dec
##     expect Str.toDec "not a number" == Err InvalidNumStr
toDec : Str -> Result Dec [InvalidNumStr]
toDec = \string -> strToNumHelp string

## Encode a [Str] to a [F64]. A [F64] value is a 64-bit
## [floating-point number](https://en.wikipedia.org/wiki/IEEE_754) and can be
## specified with a `f64` suffix.
##
##     expect Str.toF64 "0.10" == Ok 0.10f64
##     expect Str.toF64 "not a number" == Err InvalidNumStr
toF64 : Str -> Result F64 [InvalidNumStr]
toF64 = \string -> strToNumHelp string

## Encode a [Str] to a [F32].A [F32] value is a 32-bit
## [floating-point number](https://en.wikipedia.org/wiki/IEEE_754) and can be
## specified with a `f32` suffix.
##
##     expect Str.toF32 "0.10" == Ok 0.10f32
##     expect Str.toF32 "not a number" == Err InvalidNumStr
toF32 : Str -> Result F32 [InvalidNumStr]
toF32 = \string -> strToNumHelp string

## Convert a [Str] to a [Nat]. If the given number doesn't fit in [Nat], it will be [truncated](https://www.ualberta.ca/computing-science/media-library/teaching-resources/java/truncation-rounding.html).
## [Nat] has a different maximum number depending on the system you're building
## for, so this may give a different answer on different systems.
##
## For example, on a 32-bit system, `Num.maxNat` will return the same answer as
## `Num.maxU32`. This means that calling `Str.toNat "9_000_000_000"` on a 32-bit
## system will return `Num.maxU32` instead of 9 billion, because 9 billion is
## larger than `Num.maxU32` and will not fit in a [Nat] on a 32-bit system.
##
## Calling `Str.toNat "9_000_000_000"` on a 64-bit system will return
## the [Nat] value of 9_000_000_000. This is because on a 64-bit system, [Nat] can
## hold up to `Num.maxU64`, and 9_000_000_000 is smaller than `Num.maxU64`.
##
##     expect Str.toNat "9_000_000_000" == Ok 9000000000
##     expect Str.toNat "not a number" == Err InvalidNumStr
toNat : Str -> Result Nat [InvalidNumStr]
toNat = \string -> strToNumHelp string

## Encode a [Str] to an unsigned [U128] integer. A [U128] value can hold numbers
## from `0` to `340_282_366_920_938_463_463_374_607_431_768_211_455` (over
## 340 undecillion). It can be specified with a u128 suffix.
##
##     expect Str.toU128 "1500" == Ok 1500u128
##     expect Str.toU128 "0.1" == Err InvalidNumStr
##     expect Str.toU128 "-1" == Err InvalidNumStr
##     expect Str.toU128 "not a number" == Err InvalidNumStr
toU128 : Str -> Result U128 [InvalidNumStr]
toU128 = \string -> strToNumHelp string

## Encode a [Str] to a signed [I128] integer. A [I128] value can hold numbers
## from `-170_141_183_460_469_231_731_687_303_715_884_105_728` to
## `170_141_183_460_469_231_731_687_303_715_884_105_727`. It can be specified
## with a i128 suffix.
##
##     expect Str.toI128 "1500" == Ok 1500i128
##     expect Str.toI128 "-1" == Ok -1i128
##     expect Str.toI128 "0.1" == Err InvalidNumStr
##     expect Str.toI128 "not a number" == Err InvalidNumStr
toI128 : Str -> Result I128 [InvalidNumStr]
toI128 = \string -> strToNumHelp string

## Encode a [Str] to an unsigned [U64] integer. A [U64] value can hold numbers
## from `0` to `18_446_744_073_709_551_615` (over 18 quintillion). It
## can be specified with a u64 suffix.
##
##     expect Str.toU64 "1500" == Ok 1500u64
##     expect Str.toU64 "0.1" == Err InvalidNumStr
##     expect Str.toU64 "-1" == Err InvalidNumStr
##     expect Str.toU64 "not a number" == Err InvalidNumStr
toU64 : Str -> Result U64 [InvalidNumStr]
toU64 = \string -> strToNumHelp string

## Encode a [Str] to a signed [I64] integer. A [I64] value can hold numbers
## from `-9_223_372_036_854_775_808` to `9_223_372_036_854_775_807`. It can be
## specified with a i64 suffix.
##
##     expect Str.toI64 "1500" == Ok 1500i64
##     expect Str.toI64 "-1" == Ok -1i64
##     expect Str.toI64 "0.1" == Err InvalidNumStr
##     expect Str.toI64 "not a number" == Err InvalidNumStr
toI64 : Str -> Result I64 [InvalidNumStr]
toI64 = \string -> strToNumHelp string

## Encode a [Str] to an unsigned [U32] integer. A [U32] value can hold numbers
## from `0` to `4_294_967_295` (over 4 billion). It can be specified with
## a u32 suffix.
##
##     expect Str.toU32 "1500" == Ok 1500u32
##     expect Str.toU32 "0.1" == Err InvalidNumStr
##     expect Str.toU32 "-1" == Err InvalidNumStr
##     expect Str.toU32 "not a number" == Err InvalidNumStr
toU32 : Str -> Result U32 [InvalidNumStr]
toU32 = \string -> strToNumHelp string

## Encode a [Str] to a signed [I32] integer. A [I32] value can hold numbers
## from `-2_147_483_648` to `2_147_483_647`. It can be
## specified with a i32 suffix.
##
##     expect Str.toI32 "1500" == Ok 1500i32
##     expect Str.toI32 "-1" == Ok -1i32
##     expect Str.toI32 "0.1" == Err InvalidNumStr
##     expect Str.toI32 "not a number" == Err InvalidNumStr
toI32 : Str -> Result I32 [InvalidNumStr]
toI32 = \string -> strToNumHelp string

## Encode a [Str] to an unsigned [U16] integer. A [U16] value can hold numbers
## from `0` to `65_535`. It can be specified with a u16 suffix.
##
##     expect Str.toU16 "1500" == Ok 1500u16
##     expect Str.toU16 "0.1" == Err InvalidNumStr
##     expect Str.toU16 "-1" == Err InvalidNumStr
##     expect Str.toU16 "not a number" == Err InvalidNumStr
toU16 : Str -> Result U16 [InvalidNumStr]
toU16 = \string -> strToNumHelp string

## Encode a [Str] to a signed [I16] integer. A [I16] value can hold numbers
## from `-32_768` to `32_767`. It can be
## specified with a i16 suffix.
##
##     expect Str.toI16 "1500" == Ok 1500i16
##     expect Str.toI16 "-1" == Ok -1i16
##     expect Str.toI16 "0.1" == Err InvalidNumStr
##     expect Str.toI16 "not a number" == Err InvalidNumStr
toI16 : Str -> Result I16 [InvalidNumStr]
toI16 = \string -> strToNumHelp string

## Encode a [Str] to an unsigned [U8] integer. A [U8] value can hold numbers
## from `0` to `255`. It can be specified with a u8 suffix.
##
##     expect Str.toU8 "250" == Ok 250u8
##     expect Str.toU8 "-0.1" == Err InvalidNumStr
##     expect Str.toU8 "not a number" == Err InvalidNumStr
##     expect Str.toU8 "1500" == Err InvalidNumStr
toU8 : Str -> Result U8 [InvalidNumStr]
toU8 = \string -> strToNumHelp string

## Encode a [Str] to a signed [I8] integer. A [I8] value can hold numbers
## from `-128` to `127`. It can be
## specified with a i8 suffix.
##
##     expect Str.toI8 "-15" == Ok -15i8
##     expect Str.toI8 "150.00" == Err InvalidNumStr
##     expect Str.toI8 "not a number" == Err InvalidNumStr
toI8 : Str -> Result I8 [InvalidNumStr]
toI8 = \string -> strToNumHelp string

## Get the byte at the given index, without performing a bounds check.
getUnsafe : Str, Nat -> U8

## Gives the number of bytes in a [Str] value.
##
##     expect Str.countUtf8Bytes "Hello World" == 11
countUtf8Bytes : Str -> Nat

## string slice that does not do bounds checking or utf-8 verification
substringUnsafe : Str, Nat, Nat -> Str

## Returns the given [Str] with each occurrence of a substring replaced.
## Returns [Err NotFound] if the substring is not found.
##
##     expect Str.replaceEach "foo/bar/baz" "/" "_" == Ok "foo_bar_baz"
##     expect Str.replaceEach "not here" "/" "_" == Err NotFound
replaceEach : Str, Str, Str -> Result Str [NotFound]
replaceEach = \haystack, needle, flower ->
    when splitFirst haystack needle is
        Ok { before, after } ->
            # We found at least one needle, so start the buffer off with
            # `before` followed by the first replacement flower.
            Str.withCapacity (Str.countUtf8Bytes haystack)
            |> Str.concat before
            |> Str.concat flower
            |> replaceEachHelp after needle flower
            |> Ok

        Err err -> Err err

replaceEachHelp : Str, Str, Str, Str -> Str
replaceEachHelp = \buf, haystack, needle, flower ->
    when splitFirst haystack needle is
        Ok { before, after } ->
            buf
            |> Str.concat before
            |> Str.concat flower
            |> replaceEachHelp after needle flower

        Err NotFound -> Str.concat buf haystack

expect Str.replaceEach "abXdeXghi" "X" "_" == Ok "ab_de_ghi"

## Returns the given [Str] with the first occurrence of a substring replaced.
## Returns [Err NotFound] if the substring is not found.
##
##     expect Str.replaceFirst "foo/bar/baz" "/" "_" == Ok "foo_bar/baz"
##     expect Str.replaceFirst "no slashes here" "/" "_" == Err NotFound
replaceFirst : Str, Str, Str -> Result Str [NotFound]
replaceFirst = \haystack, needle, flower ->
    when splitFirst haystack needle is
        Ok { before, after } ->
            Ok "\(before)\(flower)\(after)"

        Err err -> Err err

expect Str.replaceFirst "abXdeXghi" "X" "_" == Ok "ab_deXghi"

## Returns the given [Str] with the last occurrence of a substring replaced.
## Returns [Err NotFound] if the substring is not found.
##
##     expect Str.replaceLast "foo/bar/baz" "/" "_" == Ok "foo/bar_baz"
##     expect Str.replaceLast "no slashes here" "/" "_" == Err NotFound
replaceLast : Str, Str, Str -> Result Str [NotFound]
replaceLast = \haystack, needle, flower ->
    when splitLast haystack needle is
        Ok { before, after } ->
            Ok "\(before)\(flower)\(after)"

        Err err -> Err err

expect Str.replaceLast "abXdeXghi" "X" "_" == Ok "abXde_ghi"

## Returns the given [Str] before the first occurrence of a [delimiter](https://www.computerhope.com/jargon/d/delimite.htm), as well
## as the rest of the string after that occurrence.
## Returns [ Err NotFound] if the delimiter is not found.
##
##     expect Str.splitFirst "foo/bar/baz" "/" == Ok { before: "foo", after: "bar/baz" }
##     expect Str.splitFirst "no slashes here" "/" == Err NotFound
splitFirst : Str, Str -> Result { before : Str, after : Str } [NotFound]
splitFirst = \haystack, needle ->
    when firstMatch haystack needle is
        Some index ->
            remaining = Str.countUtf8Bytes haystack - Str.countUtf8Bytes needle - index

            before = Str.substringUnsafe haystack 0 index
            after = Str.substringUnsafe haystack (index + Str.countUtf8Bytes needle) remaining

            Ok { before, after }

        None ->
            Err NotFound

# splitFirst when needle isn't in haystack
expect splitFirst "foo" "z" == Err NotFound

# splitFirst when needle isn't in haystack, and haystack is empty
expect splitFirst "" "z" == Err NotFound

# splitFirst when haystack ends with needle repeated
expect splitFirst "foo" "o" == Ok { before: "f", after: "o" }

# splitFirst with multi-byte needle
expect splitFirst "hullabaloo" "ab" == Ok { before: "hull", after: "aloo" }

# splitFirst when needle is haystack
expect splitFirst "foo" "foo" == Ok { before: "", after: "" }

firstMatch : Str, Str -> [Some Nat, None]
firstMatch = \haystack, needle ->
    haystackLength = Str.countUtf8Bytes haystack
    needleLength = Str.countUtf8Bytes needle
    lastPossible = Num.subSaturated haystackLength needleLength

    firstMatchHelp haystack needle 0 lastPossible

firstMatchHelp : Str, Str, Nat, Nat -> [Some Nat, None]
firstMatchHelp = \haystack, needle, index, lastPossible ->
    if index <= lastPossible then
        if matchesAt haystack index needle then
            Some index
        else
            firstMatchHelp haystack needle (index + 1) lastPossible
    else
        None

## Returns the given [Str] before the last occurrence of a delimiter, as well as
## the rest of the string after that occurrence.
## Returns [Err NotFound] if the delimiter is not found.
##
##     expect Str.splitLast "foo/bar/baz" "/" == Ok { before: "foo/bar", after: "baz" }
##     expect Str.splitLast "no slashes here" "/" == Err NotFound
splitLast : Str, Str -> Result { before : Str, after : Str } [NotFound]
splitLast = \haystack, needle ->
    when lastMatch haystack needle is
        Some index ->
            remaining = Str.countUtf8Bytes haystack - Str.countUtf8Bytes needle - index

            before = Str.substringUnsafe haystack 0 index
            after = Str.substringUnsafe haystack (index + Str.countUtf8Bytes needle) remaining

            Ok { before, after }

        None ->
            Err NotFound

# splitLast when needle isn't in haystack
expect Str.splitLast "foo" "z" == Err NotFound

# splitLast when haystack ends with needle repeated
expect Str.splitLast "foo" "o" == Ok { before: "fo", after: "" }

# splitLast with multi-byte needle
expect Str.splitLast "hullabaloo" "ab" == Ok { before: "hull", after: "aloo" }

# splitLast when needle is haystack
expect Str.splitLast "foo" "foo" == Ok { before: "", after: "" }

lastMatch : Str, Str -> [Some Nat, None]
lastMatch = \haystack, needle ->
    haystackLength = Str.countUtf8Bytes haystack
    needleLength = Str.countUtf8Bytes needle
    lastPossibleIndex = Num.subSaturated haystackLength needleLength

    lastMatchHelp haystack needle lastPossibleIndex

lastMatchHelp : Str, Str, Nat -> [Some Nat, None]
lastMatchHelp = \haystack, needle, index ->
    if matchesAt haystack index needle then
        Some index
    else
        when Num.subChecked index 1 is
            Ok nextIndex ->
                lastMatchHelp haystack needle nextIndex

            Err _ ->
                None

min = \x, y -> if x < y then x else y

matchesAt : Str, Nat, Str -> Bool
matchesAt = \haystack, haystackIndex, needle ->
    haystackLength = Str.countUtf8Bytes haystack
    needleLength = Str.countUtf8Bytes needle
    endIndex = min (haystackIndex + needleLength) haystackLength

    matchesAtHelp {
        haystack,
        haystackIndex,
        needle,
        needleIndex: 0,
        needleLength,
        endIndex,
    }

matchesAtHelp = \state ->
    { haystack, haystackIndex, needle, needleIndex, needleLength, endIndex } = state
    isAtEndOfHaystack = haystackIndex >= endIndex

    if isAtEndOfHaystack then
        didWalkEntireNeedle = needleIndex == needleLength

        didWalkEntireNeedle
    else
        doesThisMatch =
            Str.getUnsafe haystack haystackIndex
            ==
            Str.getUnsafe needle needleIndex
        doesRestMatch =
            matchesAtHelp
                { state &
                    haystackIndex: haystackIndex + 1,
                    needleIndex: needleIndex + 1,
                }

        doesThisMatch && doesRestMatch

## Walks over the `UTF-8` bytes of the given [Str] and calls a function to update
## state for each byte. The index for that byte in the string is provided
## to the update function.
##
##     f : List U8, U8, Nat -> List U8
##     f = \state, byte, _ -> List.append state byte
##     expect Str.walkUtf8WithIndex "ABC" [] f == [65, 66, 67]
walkUtf8WithIndex : Str, state, (state, U8, Nat -> state) -> state
walkUtf8WithIndex = \string, state, step ->
    walkUtf8WithIndexHelp string state step 0 (Str.countUtf8Bytes string)

walkUtf8WithIndexHelp : Str, state, (state, U8, Nat -> state), Nat, Nat -> state
walkUtf8WithIndexHelp = \string, state, step, index, length ->
    if index < length then
        byte = Str.getUnsafe string index
        newState = step state byte index

        walkUtf8WithIndexHelp string newState step (index + 1) length
    else
        state

## Enlarge a string for at least the given number additional bytes.
reserve : Str, Nat -> Str

## is UB when the scalar is invalid
appendScalarUnsafe : Str, U32 -> Str

## Append a [U32] scalar to the given string. If the given scalar is not a valid
## unicode value, it returns [Err InvalidScalar].
##
##     expect Str.appendScalar "H" 105 == Ok "Hi"
##     expect Str.appendScalar "😢" 0xabcdef == Err InvalidScalar
appendScalar : Str, U32 -> Result Str [InvalidScalar]
appendScalar = \string, scalar ->
    if isValidScalar scalar then
        Ok (appendScalarUnsafe string scalar)
    else
        Err InvalidScalar

isValidScalar : U32 -> Bool
isValidScalar = \scalar ->
    scalar <= 0xD7FF || (scalar >= 0xE000 && scalar <= 0x10FFFF)

getScalarUnsafe : Str, Nat -> { scalar : U32, bytesParsed : Nat }

## Walks over the unicode [U32] values for the given [Str] and calls a function
## to update state for each.
##
##     f : List U32, U32 -> List U32
##     f = \state, scalar -> List.append state scalar
##     expect Str.walkScalars "ABC" [] f == [65, 66, 67]
walkScalars : Str, state, (state, U32 -> state) -> state
walkScalars = \string, init, step ->
    walkScalarsHelp string init step 0 (Str.countUtf8Bytes string)

walkScalarsHelp : Str, state, (state, U32 -> state), Nat, Nat -> state
walkScalarsHelp = \string, state, step, index, length ->
    if index < length then
        { scalar, bytesParsed } = getScalarUnsafe string index
        newState = step state scalar

        walkScalarsHelp string newState step (index + bytesParsed) length
    else
        state

## Walks over the unicode [U32] values for the given [Str] and calls a function
## to update state for each.
##
##     f : List U32, U32 -> [Break (List U32), Continue (List U32)]
##     f = \state, scalar ->
##         check = 66
##         if scalar == check then
##             Break [check]
##         else
##             Continue (List.append state scalar)
##     expect Str.walkScalarsUntil "ABC" [] f == [66]
##     expect Str.walkScalarsUntil "AxC" [] f == [65, 120, 67]
walkScalarsUntil : Str, state, (state, U32 -> [Break state, Continue state]) -> state
walkScalarsUntil = \string, init, step ->
    walkScalarsUntilHelp string init step 0 (Str.countUtf8Bytes string)

walkScalarsUntilHelp : Str, state, (state, U32 -> [Break state, Continue state]), Nat, Nat -> state
walkScalarsUntilHelp = \string, state, step, index, length ->
    if index < length then
        { scalar, bytesParsed } = getScalarUnsafe string index

        when step state scalar is
            Continue newState ->
                walkScalarsUntilHelp string newState step (index + bytesParsed) length

            Break newState ->
                newState
    else
        state

strToNum : Str -> { berrorcode : U8, aresult : Num * }

strToNumHelp : Str -> Result (Num a) [InvalidNumStr]
strToNumHelp = \string ->
    result : { berrorcode : U8, aresult : Num a }
    result = strToNum string

    if result.berrorcode == 0 then
        Ok result.aresult
    else
        Err InvalidNumStr

## Adds a prefix to the given [Str].
##
##     expect Str.withPrefix "Awesome" "Roc" == "RocAwesome"
withPrefix : Str, Str -> Str
withPrefix = \str, prefix -> Str.concat prefix str
