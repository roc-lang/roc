interface Str
    exposes
        [
            Str,
            append,
            concat,
            countGraphemes,
            endsWith,
            fromUtf8,
            isEmpty,
            joinWith,
            split,
            startsWith,
            startsWithCodePt,
            toUtf8,
            Utf8Problem,
            Utf8ByteProblem
        ]
    imports []

## # Types
##
## Dealing with text is a deep topic, so by design, Roc's `Str` module sticks
## to the basics.
##
## _For more advanced use cases like working with raw [code points](https://unicode.org/glossary/#code_point),
## see the [roc/unicode](roc/unicode) package. For locale-specific text
## functions (including uppercasing strings, as capitalization rules vary by locale;
## in English, `"i"` capitalizes to `"I"`, but [in Turkish](https://en.wikipedia.org/wiki/Dotted_and_dotless_I#In_computing),
## the same `"i"` capitalizes to `"İ"` - as well as sorting strings, which also varies
## by locale; `"ö"` is sorted differently in German and Swedish) see the [roc/locale](roc/locale) package._
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
##
## ### Encoding
##
## Roc strings are not coupled to any particular
## [encoding](https://en.wikipedia.org/wiki/Character_encoding). As it happens,
## they are currently encoded in UTF-8, but this module is intentionally designed
## not to rely on that implementation detail so that a future release of Roc can
## potentially change it without breaking existing Roc applications. (UTF-8
## seems pretty great today, but so did UTF-16 at an earlier point in history.)
##
## This module has functions to can convert a [Str] to a [List] of raw [code unit](https://unicode.org/glossary/#code_unit)
## integers (not to be confused with the [code points](https://unicode.org/glossary/#code_point)
## mentioned earlier) in a particular encoding. If you need encoding-specific functions,
## you should take a look at the [roc/unicode](roc/unicode) package.
## It has many more tools than this module does!

## A [Unicode](https://unicode.org) text value.
Str : [ @Str ]

## Convert

## Convert a [Float] to a decimal string, rounding off to the given number of decimal places.
##
## If you want to keep all the digits, use [Str.num] instead.
decimal : Float *, Nat -> Str


## Convert a [Num] to a string.
num : Float *, Nat -> Str

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

## Split a string around newlines.
##
## On strings that use `"\n"` for their line endings, this gives the same answer
## as passing `"\n"` to [Str.split]. However, on strings that use `"\n\r"` (such
## as [in Windows files](https://en.wikipedia.org/wiki/Newline#History)), this
## will consume the entire `"\n\r"` instead of just the `"\n"`.
##
## >>> Str.lines "Hello, World!\nNice to meet you!"
##
## >>> Str.lines "Hello, World!\n\rNice to meet you!"
##
## To split a string using a custom separator, use [Str.split]. For more advanced
## string splitting, use a #Parser.
lines : Str, Str -> List Str

## Check

## Returns `True` if the string is empty, and `False` otherwise.
##
## >>> Str.isEmpty "hi!"
##
## >>> Str.isEmpty ""
isEmpty : Str -> Bool

startsWith : Str, Str -> Bool

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

endsWith : Str, Str -> Bool

contains : Str, Str -> Bool

anyGraphemes : Str, (Str -> Bool) -> Bool

allGraphemes : Str, (Str -> Bool) -> Bool

## Combine

## Combine a list of strings into a single string.
##
## >>> Str.join [ "a", "bc", "def" ]
join : List Str -> Str

## Combine a list of strings into a single string, with a separator
## string in between each.
##
## >>> Str.joinWith [ "one", "two", "three" ] ", "
joinWith : List Str, Str -> Str

## Add to the start of a string until it has at least the given number of
## graphemes.
##
## >>> Str.padGraphemesStart "0" 5 "36"
##
## >>> Str.padGraphemesStart "0" 1 "36"
##
## >>> Str.padGraphemesStart "0" 5 "12345"
##
## >>> Str.padGraphemesStart "✈️"" 5 "👩‍👩‍👦‍👦👩‍👩‍👦‍👦👩‍👩‍👦‍👦"
padGraphemesStart : Str, Nat, Str -> Str

## Add to the end of a string until it has at least the given number of
## graphemes.
##
## >>> Str.padGraphemesStart "0" 5 "36"
##
## >>> Str.padGraphemesStart "0" 1 "36"
##
## >>> Str.padGraphemesStart "0" 5 "12345"
##
## >>> Str.padGraphemesStart "✈️"" 5 "👩‍👩‍👦‍👦👩‍👩‍👦‍👦👩‍👩‍👦‍👦"
padGraphemesEnd : Str, Nat, Str -> Str

## Graphemes

## Split a string into its individual graphemes.
##
## >>> Str.graphemes "1,2,3"
##
## >>> Str.graphemes  "👍👍👍"
##
graphemes : Str -> List Str

## Count the number of [extended grapheme clusters](http://www.unicode.org/glossary/#extended_grapheme_cluster)
## in the string.
##
##     Str.countGraphemes "Roc!"   # 4
##     Str.countGraphemes "七巧板" # 3
##     Str.countGraphemes "🕊"     # 1
countGraphemes : Str -> Nat

## Reverse the order of the string's individual graphemes.
##
## >>> Str.reverseGraphemes "1-2-3"
##
## >>> Str.reverseGraphemes  "🐦✈️"👩‍👩‍👦‍👦"
##
## >>> Str.reversegraphemes "Crème Brûlée"
reverseGraphemes : Str -> Str

## Returns `True` if the two strings are equal when ignoring case.
##
## >>> Str.caseInsensitiveEq "hi" "Hi"
isCaseInsensitiveEq : Str, Str -> Bool

isCaseInsensitiveNeq : Str, Str -> Bool

walkGraphemes : Str, { start: state, step: (state, Str -> state) } -> state
walkGraphemesUntil : Str, { start: state, step: (state, Str -> [ Continue state, Done state ]) } -> state
walkGraphemesBackwards : Str, { start: state, step: (state, Str -> state) } -> state
walkGraphemesBackwardsUntil : Str, { start: state, step: (state, Str -> [ Continue state, Done state ]) } -> state

## Returns `True` if the string begins with an uppercase letter.
##
## >>> Str.isCapitalized "Hi"
##
## >>> Str.isCapitalized " Hi"
##
## >>> Str.isCapitalized "hi"
##
## >>> Str.isCapitalized "Česká"
##
## >>> Str.isCapitalized "Э"
##
## >>> Str.isCapitalized "東京"
##
## >>> Str.isCapitalized "🐦"
##
## >>> Str.isCapitalized ""
##
## Since the rules for how to capitalize a string vary by locale,
## (for example, in English, `"i"` capitalizes to `"I"`, but
## [in Turkish](https://en.wikipedia.org/wiki/Dotted_and_dotless_I#In_computing),
## the same `"i"` capitalizes to `"İ"`) see the [roc/locale](roc/locale) package
## package for functions which capitalize strings.
isCapitalized : Str -> Bool

## Returns `True` if the string consists entirely of uppercase letters.
##
## >>> Str.isAllUppercase "hi"
##
## >>> Str.isAllUppercase "Hi"
##
## >>> Str.isAllUppercase "HI"
##
## >>> Str.isAllUppercase " Hi"
##
## >>> Str.isAllUppercase "Česká"
##
## >>> Str.isAllUppercase "Э"
##
## >>> Str.isAllUppercase "東京"
##
## >>> Str.isAllUppercase "🐦"
##
## >>> Str.isAllUppercase ""
isAllUppercase : Str -> Bool

## Returns `True` if the string consists entirely of lowercase letters.
##
## >>> Str.isAllLowercase "hi"
##
## >>> Str.isAllLowercase "Hi"
##
## >>> Str.isAllLowercase "HI"
##
## >>> Str.isAllLowercase " Hi"
##
## >>> Str.isAllLowercase "Česká"
##
## >>> Str.isAllLowercase "Э"
##
## >>> Str.isAllLowercase "東京"
##
## >>> Str.isAllLowercase "🐦"
##
## >>> Str.isAllLowercase ""
isAllLowercase : Str -> Bool

## Return the string with any blank spaces removed from both the beginning
## as well as the end.
trim : Str -> Str

## If the given [U32] is a valid [Unicode Scalar Value](http://www.unicode.org/glossary/#unicode_scalar_value),
## return a [Str] containing only that scalar.
fromScalar : U32 -> Result Str [ BadScalar ]*
fromCodePts : List U32 -> Result Str [ BadCodePt U32 ]*
fromUtf8 : List U8 -> Result Str [ BadUtf8 ]*

## Create a [Str] from bytes encoded as [UTF-16LE](https://en.wikipedia.org/wiki/UTF-16#Byte-order_encoding_schemes).
# fromUtf16Le : List U8 -> Result Str [ BadUtf16Le Endi ]*

# ## Create a [Str] from bytes encoded as [UTF-16BE](https://en.wikipedia.org/wiki/UTF-16#Byte-order_encoding_schemes).
# fromUtf16Be : List U8 -> Result Str [ BadUtf16Be Endi ]*

# ## Create a [Str] from bytes encoded as UTF-16 with a [Byte Order Mark](https://en.wikipedia.org/wiki/Byte_order_mark).
# fromUtf16Bom : List U8 -> Result Str [ BadUtf16 Endi, NoBom ]*

# ## Create a [Str] from bytes encoded as [UTF-32LE](https://web.archive.org/web/20120322145307/http://mail.apps.ietf.org/ietf/charsets/msg01095.html)
# fromUtf32Le : List U8 -> Result Str [ BadUtf32Le Endi ]*

# ## Create a [Str] from bytes encoded as [UTF-32BE](https://web.archive.org/web/20120322145307/http://mail.apps.ietf.org/ietf/charsets/msg01095.html)
# fromUtf32Be : List U8 -> Result Str [ BadUtf32Be Endi ]*

# ## Create a [Str] from bytes encoded as UTF-32 with a [Byte Order Mark](https://en.wikipedia.org/wiki/Byte_order_mark).
# fromUtf32Bom : List U8 -> Result Str [ BadUtf32 Endi, NoBom ]*

# ## Convert from UTF-8, substituting the replacement character ("�") for any
# ## invalid sequences encountered.
# fromUtf8Sub : List U8 -> Str
# fromUtf16Sub : List U8, Endi -> Str
# fromUtf16BomSub : List U8 -> Result Str [ NoBom ]*

## Return a [List] of the string's [U8] UTF-8 [code units](https://unicode.org/glossary/#code_unit).
## (To split the string into a [List] of smaller [Str] values instead of [U8] values,
## see [Str.split] and `Str.graphemes`.)
##
## >>> Str.toUtf8 "👩‍👩‍👦‍👦"
##
## >>> Str.toUtf8 "Roc"
##
## >>> Str.toUtf8 "鹏"
##
## >>> Str.toUtf8 "🐦"
##
## For a more flexible function that walks through each of these [U8] code units
## without creating a [List], see `Str.walkUtf8` and `Str.walkRevUtf8`.
toUtf8 : Str -> List U8
toUtf16Be : Str -> List U8
toUtf16Le : Str -> List U8
# toUtf16Bom : Str, Endi -> List U8
toUtf32Be : Str -> List U8
toUtf32Le : Str -> List U8
# toUtf32Bom : Str, Endi -> List U8

# Parsing

## If the string begins with a valid [extended grapheme cluster](http://www.unicode.org/glossary/#extended_grapheme_cluster),
## return it along with the rest of the string after that grapheme.
##
## If the string does not begin with a full grapheme, for example because it was
## empty, return `Err`.
parseGrapheme : Str -> Result { val : Str, rest : Str } [ Expected [ Grapheme ]* Str ]*

## If the string begins with a valid [Unicode code point](http://www.unicode.org/glossary/#code_point),
## return it along with the rest of the string after that code point.
##
## If the string does not begin with a valid code point, for example because it was
## empty, return `Err`.
parseCodePt : Str -> Result { val : U32, rest : Str } [ Expected [ CodePt ]* Str ]*

## If the first string begins with the second, return whatever comes
## after the second.
chomp : Str, Str -> Result Str [ Expected [ ExactStr Str ]* Str ]*

## If the string begins with a [Unicode code point](http://www.unicode.org/glossary/#code_point)
## equal to the given [U32], return whatever comes after that code point.
chompCodePt : Str, U32 -> Result Str [ Expected [ ExactCodePt U32 ]* Str ]*

## If the string represents a valid [U8] number, return that number.
##
## For more advanced options, see [parseU8].
toU8 : Str -> Result U8 [ InvalidU8 ]*
toI8 : Str -> Result I8 [ InvalidI8 ]*
toU16 : Str -> Result U16 [ InvalidU16 ]*
toI16 : Str -> Result I16 [ InvalidI16 ]*
toU32 : Str -> Result U32 [ InvalidU32 ]*
toI32 : Str -> Result I32 [ InvalidI32 ]*
toU64 : Str -> Result U64 [ InvalidU64 ]*
toI64 : Str -> Result I64 [ InvalidI64 ]*
toU128 : Str -> Result U128 [ InvalidU128 ]*
toI128 : Str -> Result I128 [ InvalidI128 ]*
toF64 : Str -> Result U128 [ InvalidF64 ]*
toF32 : Str -> Result I128 [ InvalidF32 ]*
toDec : Str -> Result Dec [ InvalidDec ]*

## If the string represents a valid number, return that number.
##
## The exact number type to look for will be inferred from usage.
## In the example below, the usage of I64 in the type signature will require that type instead of (Num *).
##
## >>> strToI64 : Str -> Result I64 [ InvalidNumStr ]*
## >>> strToI64 = \inputStr ->
## >>>     Str.toNum inputStr
##
## If the string is exactly `"NaN"`, `"∞"`, or `"-∞"`, they will be accepted
## only when converting to [F64] or [F32] numbers, and will be translated accordingly.
##
## This never accepts numbers with underscores or commas in them. For more
## advanced options, see [parseNum].
toNum : Str -> Result (Num *) [ InvalidNumStr ]*

## If the string begins with an [Int] or a [finite](Num.isFinite) [Frac], return
## that number along with the rest of the string after it.
##
## The exact number type to look for will be inferred from usage.
## In the example below, the usage of Float64 in the type signature will require that type instead of (Num *).
##
## >>> parseFloat64 : Str -> Result { val: Float64, rest: Str } [ InvalidNumStr ]*
## >>>     Str.parseNum input {}
##
## If the string begins with `"NaN"`, `"∞"`, and `"-∞"` (which do not represent
## [finite](Num.isFinite) numbers), they will be accepted only when parsing
## [F64] or [F32] numbers, and translated accordingly.
# parseNum : Str, NumParseConfig -> Result { val : Num *, rest : Str } [ InvalidNumStr ]*

## Notes:
## * You can allow a decimal mark for integers; they'll only parse if the numbers after it are all 0.
## * For `wholeSep`, `Required` has a payload for how many digits (e.g. "required every 3 digits")
## * For `wholeSep`, `Allowed` allows the separator to appear anywhere.
# NumParseConfig :
#     {
#         base ? [ Decimal, Hexadecimal, Octal, Binary ],
#         notation ? [ Standard, Scientific, Any ],
#         decimalMark ? [ Allowed Str, Required Str, Disallowed ],
#         decimalDigits ? [ Any, AtLeast U16, Exactly U16 ],
#         wholeDigits ? [ Any, AtLeast U16, Exactly U16 ],
#         leadingZeroes ? [ Allowed, Disallowed ],
#         trailingZeroes ? [ Allowed, Disallowed ],
#         wholeSep ? { mark : Str, policy : [ Allowed, Required U64 ] }
#     }
