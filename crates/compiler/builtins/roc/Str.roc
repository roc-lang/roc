interface Str
    exposes
        [
            Utf8Problem,
            Utf8ByteProblem,
            concat,
            isEmpty,
            joinWith,
            split,
            repeat,
            countGraphemes,
            startsWithCodePt,
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
        ]
    imports [Bool.{ Bool }, Result.{ Result }]

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
## You can get the number of graphemes in a string by calling [Str.countGraphemes] on it:
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
Utf8ByteProblem :
    [
        InvalidStartByte,
        UnexpectedEndOfSequence,
        ExpectedContinuation,
        OverlongEncoding,
        CodepointTooLarge,
        EncodesSurrogateHalf,
    ]

Utf8Problem : { byteIndex : Nat, problem : Utf8ByteProblem }

## Returns `True` if the string is empty, and `False` otherwise.
##
## >>> Str.isEmpty "hi!"
##
## >>> Str.isEmpty ""
isEmpty : Str -> Bool
concat : Str, Str -> Str

## Combine a list of strings into a single string, with a separator
## string in between each.
##
## >>> Str.joinWith ["one", "two", "three"] ", "
joinWith : List Str, Str -> Str

## Split a string around a separator.
##
## >>> Str.split "1,2,3" ","
##
## Passing `""` for the separator is not useful; it returns the original string
## wrapped in a list.
##
## >>> Str.split "1,2,3" ""
##
## To split a string into its individual graphemes, use `Str.graphemes`
split : Str, Str -> List Str
repeat : Str, Nat -> Str

## Count the number of [extended grapheme clusters](http://www.unicode.org/glossary/#extended_grapheme_cluster)
## in the string.
##
##     Str.countGraphemes "Roc!"   # 4
##     Str.countGraphemes "‰∏ÉÂ∑ßÊùø" # 3
##     Str.countGraphemes "üïä"     # 1
countGraphemes : Str -> Nat

## If the string begins with a [Unicode code point](http://www.unicode.org/glossary/#code_point)
## equal to the given [U32], return `True`. Otherwise return `False`.
##
## If the given [Str] is empty, or if the given [U32] is not a valid
## code point, this will return `False`.
##
## **Performance Note:** This runs slightly faster than [Str.startsWith], so
## if you want to check whether a string begins with something that's representable
## in a single code point, you can use (for example) `Str.startsWithCodePt '鹏'`
## instead of `Str.startsWithCodePt "鹏"`. ('鹏' evaluates to the [U32]
## value `40527`.) This will not work for graphemes which take up multiple code
## points, however; `Str.startsWithCodePt '👩‍👩‍👦‍👦'` would be a compiler error
## because 👩‍👩‍👦‍👦 takes up multiple code points and cannot be represented as a
## single [U32]. You'd need to use `Str.startsWithCodePt "🕊"` instead.
startsWithCodePt : Str, U32 -> Bool

## Return a [List] of the [unicode scalar values](https://unicode.org/glossary/#unicode_scalar_value)
## in the given string.
##
## (Strings contain only scalar values, not [surrogate code points](https://unicode.org/glossary/#surrogate_code_point),
## so this is equivalent to returning a list of the string's [code points](https://unicode.org/glossary/#code_point).)
toScalars : Str -> List U32

## Return a [List] of the string's [U8] UTF-8 [code units](https://unicode.org/glossary/#code_unit).
## (To split the string into a [List] of smaller [Str] values instead of [U8] values,
## see [Str.split].)
##
## >>> Str.toUtf8 "👩‍👩‍👦‍👦"
##
## >>> Str.toUtf8 "Roc"
##
## >>> Str.toUtf8 "鹏"
##
## >>> Str.toUtf8 "🐦"
toUtf8 : Str -> List U8

# fromUtf8 : List U8 -> Result Str [BadUtf8 Utf8Problem]*
# fromUtf8Range : List U8 -> Result Str [BadUtf8 Utf8Problem Nat, OutOfBounds]*
fromUtf8 : List U8 -> Result Str [BadUtf8 Utf8ByteProblem Nat]*
fromUtf8Range : List U8, { start : Nat, count : Nat } -> Result Str [BadUtf8 Utf8ByteProblem Nat, OutOfBounds]*

startsWith : Str, Str -> Bool
endsWith : Str, Str -> Bool

## Return the string with any blank spaces removed from both the beginning
## as well as the end.
trim : Str -> Str
trimLeft : Str -> Str
trimRight : Str -> Str

toDec : Str -> Result Dec [InvalidNumStr]*
toF64 : Str -> Result F64 [InvalidNumStr]*
toF32 : Str -> Result F32 [InvalidNumStr]*
toNat : Str -> Result Nat [InvalidNumStr]*
toU128 : Str -> Result U128 [InvalidNumStr]*
toI128 : Str -> Result I128 [InvalidNumStr]*
toU64 : Str -> Result U64 [InvalidNumStr]*
toI64 : Str -> Result I64 [InvalidNumStr]*
toU32 : Str -> Result U32 [InvalidNumStr]*
toI32 : Str -> Result I32 [InvalidNumStr]*
toU16 : Str -> Result U16 [InvalidNumStr]*
toI16 : Str -> Result I16 [InvalidNumStr]*
toU8 : Str -> Result U8 [InvalidNumStr]*
toI8 : Str -> Result I8 [InvalidNumStr]*
