api Str provides Str, isEmpty, join

## Types

## A [Unicode](https://unicode.org) text value.
##
## Dealing with text is deep topic, so by design, Roc's `Str` module sticks
## to the basics. For more advanced use cases like working with raw [code points](https://en.wikipedia.org/wiki/Code_point),
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
## Roc strings are not coupled to any particular
## [encoding](https://en.wikipedia.org/wiki/Character_encoding). As it happens,
## they are currently encoded in UTF-8, but this module is intentionally designed
## not to rely on that implementation detail so that a future release of Roc can
## potentially change it without breaking existing Roc applications.
##
## This module has functions to can convert a #Str to a #List of raw code unit integers
## in a particular encoding, but if you are doing encoding-specific work,
## you should take a look at the [roc/unicode](roc/unicode) pacakge.
## It has many more tools than this module does!
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
decimal : Float *, Int * -> Str

## Convert an #Int to a string.
int : Int * -> Str

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

reverseClusters : Str -> Str

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

## ## Code Units
##
## Besides grapheme clusters, another way to break down strings is into
## raw code unit integers.
##
## Code units are no substitute for grapheme clusters!
## These functions exist to support advanced use cases like those found in
## [roc/unicode](roc/unicode), and using code units when grapheme clusters would
## be more appropriate can very easily lead to bugs.
##
## For example, `Str.countGraphemes "👍"` returns `1`,
## whereas `Str.toUtf8 "👍"` returns a list with a length of 4,
## and `Str.toUtf16 "👍"` returns a list with a length of 2.

toUtf8 : Str -> List U8

toUtf16 : Str -> List U16

toUtf32 : Str -> List U32

