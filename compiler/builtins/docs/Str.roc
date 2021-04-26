interface Str
    exposes [ Str, decimal, split, isEmpty, startsWith, endsWith, contains, anyGraphemes, allGraphemes, join, joinWith, padGraphemesStart, padGraphemesEnd, graphemes, reverseGraphemes, isCaseInsensitiveEq, isCaseInsensitiveNeq, walkGraphemes, isCapitalized, isAllUppercase, isAllLowercase, toUtf8, toUtf16, toUtf32, trim, walkUtf8, walkUtf16, walkUtf32, walkRevUtf8, walkRevUtf16, walkRevUtf32 ]
    imports []
## # Types

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
## You can get the number of graphemes in a string by calling #Str.countGraphemes on it:
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
## This module has functions to can convert a #Str to a #List of raw [code unit](https://unicode.org/glossary/#code_unit)
## integers (not to be confused with the [code points](https://unicode.org/glossary/#code_point)
## mentioned earlier) in a particular encoding. If you need encoding-specific functions,
## you should take a look at the [roc/unicode](roc/unicode) package.
## It has many more tools than this module does!

## A [Unicode](https://unicode.org) text value.
##
## The type parameter represents the string's encoding.
Str a : [ @Str a ]

## Convert

## Convert a #Float to a decimal string, rounding off to the given number of decimal places.
##
## Since #Float values are imprecise, it's usually best to limit this to the lowest
## number you can choose that will make sense for what you want to display.
##
## If you want to keep all the digits, passing the same float to #Str.num
## will do that.
decimal : Float *, Nat -> Str *

## Split a string around a separator.
##
## >>> Str.split "1,2,3" ","
##
## Passing `""` for the separator is not useful; it returns the original string
## wrapped in a list.
##
## >>> Str.split "1,2,3" ""
##
## To split a string into its individual graphemes, use #Str.graphemes
split : Str a, Str a -> List (Str a)

## Split a string around newlines.
##
## On strings that use `"\n"` for their line endings, this gives the same answer
## as passing `"\n"` to #Str.split. However, on strings that use `"\n\r"` (such
## as [in Windows files](https://en.wikipedia.org/wiki/Newline#History)), this
## will consume the entire `"\n\r"` instead of just the `"\n"`.
##
## >>> Str.lines "Hello, World!\nNice to meet you!"
##
## >>> Str.lines "Hello, World!\n\rNice to meet you!"
##
## To split a string using a custom separator, use #Str.split. For more advanced
## string splitting, use a #Parser.
lines : Str a, Str a -> List (Str a)

## Check

## Returns #True if the string is empty, and #False otherwise.
##
## >>> Str.isEmpty "hi!"
##
## >>> Str.isEmpty ""
isEmpty : Str * -> Bool

startsWith : Str a, Str a -> Bool

endsWith : Str a, Str a -> Bool

contains : Str a, Str a -> Bool

anyGraphemes : Str a, (Str a -> Bool) -> Bool

allGraphemes : Str a, (Str a -> Bool) -> Bool

## Combine

## Combine a list of strings into a single string.
##
## >>> Str.join [ "a", "bc", "def" ]
join : List (Str a) -> Str a

## Combine a list of strings into a single string, with a separator
## string in between each.
##
## >>> Str.joinWith [ "one", "two", "three" ] ", "
joinWith : List (Str a), Str a -> Str a

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
padGraphemesStart : Str a, Nat, Str a -> Str a

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
padGraphemesEnd : Str a, Nat, Str a -> Str a

## Graphemes

## Split a string into its individual graphemes.
##
## >>> Str.graphemes "1,2,3"
##
## >>> Str.graphemes  "👍👍👍"
##
graphemes : Str a -> List (Str a)

##     Str.countGraphemes "Roc!"   # 4
##     Str.countGraphemes "七巧板" # 3
##     Str.countGraphemes "🕊"     # 1

## Reverse the order of the string's individual graphemes.
##
## >>> Str.reverseGraphemes "1-2-3"
##
## >>> Str.reverseGraphemes  "🐦✈️"👩‍👩‍👦‍👦"
##
## >>> Str.reversegraphemes "Crème Brûlée"
reverseGraphemes : Str a -> Str a

## Returns #True if the two strings are equal when ignoring case.
##
## >>> Str.caseInsensitiveEq "hi" "Hi"
isCaseInsensitiveEq : Str a, Str a -> Bool

isCaseInsensitiveNeq : Str a, Str a -> Bool

walkGraphemes : Str a, { start: state, step: (state, Str a -> state) } -> state

## Returns #True if the string begins with an uppercase letter.
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
isCapitalized : Str * -> Bool

## Returns #True if the string consists entirely of uppercase letters.
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
isAllUppercase : Str * -> Bool

## Returns #True if the string consists entirely of lowercase letters.
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
isAllLowercase : Str * -> Bool

## Return the string with any blank spaces removed from both the beginning
## as well as the end.
trim : Str a -> Str a

## Unicode Scalar Values

## Besides graphemes, another way to break down strings is into
## Unicode Scalar Values.
##
## USVs are no substitute for graphemes!
## These functions exist to support advanced use cases like those found in
## [roc/unicode](roc/unicode), and using USVs when graphemes would
## be more appropriate can very easily lead to bugs.
##
## For example, `Str.countGraphemes "👩‍👩‍👦‍👦"` returns `1`,
## whereas `Str.toUtf8 "👩‍👩‍👦‍👦"` returns a list with a length of 25,
## `Str.toUtf16 "👩‍👩‍👦‍👦"` returns a list with a length of 11.
## and `Str.toUtf32 "👩‍👩‍👦‍👦"` returns a list with a length of 7.


## Walk through the string's [Unicode Scalar Values](http://www.unicode.org/glossary/#unicode_scalar_value)
## (USVs) to build up a state.
## (If you want a `step` function which receives a #Str instead of an #U32, see #Str.walkGraphemes.)
##
## Here are the #U32 values that will be passed to `step` when this function is
## called on various strings:
##
## * `"👩‍👩‍👦‍👦"` passes 128105, 8205, 128105, 8205, 128102, 8205, 128102
## * `"Roc"` passes 82, 111, 99
## * `"鹏"` passes 40527
## * `"🐦"` passes 128038
walkUsv : Str, { start: state, step: (state, U32 -> state) } -> state

## Walk backwards through the string's [Unicode Scalar Values](http://www.unicode.org/glossary/#unicode_scalar_value)
## (USVs) to build up a state.
## (If you want a `step` function which receives a #Str instead of an #U32, see #Str.walkGraphemes.)
##
## Here are the #U32 values that will be passed to `step` when this function is
## called on various strings:
##
## * `"👩‍👩‍👦‍👦"` passes 128102, 8205, 128102, 8205, 128105, 8205, 128105
## * `"Roc"` passes 99, 111, 82
## * `"鹏"` passes 40527
## * `"🐦"` passes 128038
##
## To convert a #Str into a plain `List U32` of UTF-32 code units, see #Str.toUtf32.
walkBackwardsUsv : Str, { start: state, step: (state, U32 -> state) } -> state

# Parsing

## Parse a [Unicode Scalar Value](http://www.unicode.org/glossary/#unicode_scalar_value)
## (USV).
parseUsv : Str a -> Result { answer : U32, rest : Str a } [ Expected [ Usv ]* (Str a) ]*
parseGrapheme : Str a -> Result { answer : Str a, rest : Str a } [ Expected [ Grapheme ]* (Str a) ]*
parseU8 : Str a -> Result { answer : U8, rest : Str a } [ Expected [ U8 ]* (Str a) ]*
parseI8 : Str a -> Result { answer : I8, rest : Str a } [ Expected [ I8 ]* (Str a) ]*
parseU16 : Str a -> Result { answer : U16, rest : Str a } [ Expected [ U16 ]* (Str a) ]*
parseI16 : Str a -> Result { answer : I16, rest : Str a } [ Expected [ I16 ]* (Str a) ]*
parseU32 : Str a -> Result { answer : U32, rest : Str a } [ Expected [ U32 ]* (Str a) ]*
parseI32 : Str a -> Result { answer : I32, rest : Str a } [ Expected [ I32 ]* (Str a) ]*
parseU64 : Str a -> Result { answer : U64, rest : Str a } [ Expected [ U64 ]* (Str a) ]*
parseI64 : Str a -> Result { answer : I64, rest : Str a } [ Expected [ I64 ]* (Str a) ]*
parseU128 : Str a -> Result { answer : U128, rest : Str a } [ Expected [ U128 ]* (Str a) ]*
parseI128 : Str a -> Result { answer : I128, rest : Str a } [ Expected [ I128 ]* (Str a) ]*
