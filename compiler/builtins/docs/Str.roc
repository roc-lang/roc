api Str provides Str, isEmpty, join

## Types

## A [Unicode](https://unicode.org) text value.
##
## Dealing with text is deep topic, so by design, Roc's `Str` module sticks
## to the basics. For more advanced uses such as working with raw [code points](https://en.wikipedia.org/wiki/Code_point),
## see the [roc/unicode](roc/unicode) package, and for locale-specific text
## functions (including capitalization, as capitalization rules vary by locale)
## see the [roc/locale](roc/locale) package.
##
## ### Unicode
##
## Unicode can represent text values which span multiple languages, symbols, and emoji.
## Here are some valid Roc strings:
##
## * "Roc"
## * "鹏"
## * "🐦"
##
## Every Unicode string is a sequence of [grapheme clusters](https://unicode.org/glossary/#grapheme_cluster).
## A grapheme cluster corresponds to what a person reading a string might call
## a "character", but because the term "character" is used to mean many different
## concepts across different programming languages, we intentionally avoid it in Roc.
## Instead, we use the term "clusters" as a shorthand for "grapheme clusters."
##
## You can get the number of grapheme clusters in a string by calling `Str.countClusters` on it:
##
## >>> Str.countClusters "Roc"
##
## >>> Str.countClusters "音乐"
##
## >>> Str.countClusters "👍"
##
## > The `countClusters` function traverses the entire string to calculate its answer,
## > so it's much better for performance to use `Str.isEmpty` instead of
## > calling `Str.countClusters` and checking whether the count was `0`.
##
## ### Escape characters
##
## ### String interpolation
##
## ### Encoding
##
## Whenever any Roc string is created, its [encoding](https://en.wikipedia.org/wiki/Character_encoding)
## comes from a configuration option chosen by [the host](guide|hosts).
## Because of this, None of the functions in this module
## make assumptions about the underlying encoding. After all, different hosts
## may choose different encodings! Here are some factors hosts may consider
## when deciding which encoding to choose:
##
## * Linux APIs typically use [UTF-8](https://en.wikipedia.org/wiki/UTF-8) encoding
## * Windows APIs and Apple [Objective-C](https://en.wikipedia.org/wiki/Objective-C) APIs typically use [UTF-16](https://en.wikipedia.org/wiki/UTF-16) encoding
## * Hosts which call [C](https://en.wikipedia.org/wiki/C_%28programming_language%29) functions may choose [MUTF-8](https://en.wikipedia.org/wiki/UTF-8#Modified_UTF-8) to disallow a valid UTF-8 character which can prematurely terminate C strings
##
## > Roc strings only support Unicode, so they do not support non-Unicode character
## > encodings like [ASCII](https://en.wikipedia.org/wiki/ASCII).
##
## To write code which behaves differently depending on which encoding the host chose,
## the #Str.codeUnits function will do that. However, if you are doing encoding-specific work,
## you should take a look at the [roc/unicode](roc/unicode) pacakge;
## it has many more tools than this module does.
##
Str : [ @Str ]

## Convert

## Convert a #Float to a decimal string, rounding off to the given number of decimal places.
##
## Since #Float values are imprecise, it's usually best to limit this to the lowest
## number you can choose that will make sense for what you want to display.
##
## If you want to kep all the digits, passing #Int.highestSupported will accomplish this,
## but it's recommended to pass much smaller numbers instead.
##
## Passing a negative number for decimal places is equivalent to passing 0.
decimal : Int, Float -> Str

## Convert an #Int to a string.
int : Float -> Str

## Check

isEmpty : Str -> Bool

startsWith : Str, Str -> Bool

endsWith : Str, Str -> Bool

contains : Str, Str -> Bool

any : Str, Str -> Bool

all : Str, Str -> Bool

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


padStart : Str, Int, Str -> Str

padEnd : Str, Int, Str -> Str


foldClusters : Str, { start: state, step: (state, Str -> state) } -> state

## Returns #True if the string begins with a capital letter, and #False otherwise.
##
## >>> Str.isCapitalized "hi"
##
## >>> Str.isCapitalized "Hi"
##
## >>> Str.isCapitalized " Hi"
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
## Since the rules for how to capitalize an uncapitalized string vary by locale,
## see the [roc/locale](roc/locale) package for functions which do that.
isCapitalized : Str -> Bool


## Deconstruct the string into raw code unit integers. (Note that code units
## are not the same as code points; to work with code points, see [roc/unicode](roc/unicode)).
##
## This returns a different tag depending on the string encoding chosen by the host.
##
## The size of an individual code unit depends on the encoding. For example,
## in UTF-8 and MUTF-8, a code unit is 8 bits, so those encodings
## are returned as `List U8`. In contrast, UTF-16 encoding uses 16-bit code units,
## so the `Utf16` tag holds a `List U16` instead.
##
## > Code units are no substitute for grapheme clusters!
## >
## > For example, `Str.countGraphemes "👍"` always returns `1` no matter what,
## > whereas `Str.codeUnits "👍"` could give you back a `List U8` with a length
## > of 4, or a `List U16` with a length of 2, neither of which is equal to
## > the correct number of grapheme clusters in that string.
## >
## > This function exists for more advanced use cases like those found in
## > [roc/unicode](roc/unicode), and using code points when grapheme clusters would
## > be more appropriate can very easily lead to bugs.
codeUnits : Str -> [ Utf8 (List U8), Mutf8 (List U8), Ucs2 (List U16), Utf16 (List U16), Utf32 (List U32) ]
