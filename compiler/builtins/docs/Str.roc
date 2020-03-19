api Str provides Str, isEmpty, join

## Types

## A [Unicode](https://unicode.org) text value.
##
## Dealing with text is deep topic, so by design, Roc's `Str` module sticks
## to the basics. For more advanced use cases like working with raw [code points](https://unicode.org/glossary/#code_point),
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
## This module has functions to can convert a #Str to a #List of raw [code unit](https://unicode.org/glossary/#code_unit)
## integers (not to be confused with the [code points](https://unicode.org/glossary/#code_point)
## mentioned earlier) in a particular encoding. If you need encoding-specific functions,
## you should take a look at the [roc/unicode](roc/unicode) package.
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
decimal : Float *, ULen -> Str

## Convert an #Int to a string.
int : Int * -> Str

## Split a string around a separator.
##
## >>> Str.splitClusters "1,2,3" ","
##
## Passing `""` for the separator is not useful; it returns the original string
## wrapped in a list.
##
## >>> Str.splitClusters "1,2,3" ""
##
## To split a string into its grapheme clusters, use #Str.clusters
split : Str, Str -> List Str

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

## Grapheme Clusters

## Split a string into its grapheme clusters.
##
## >>> Str.clusters "1,2,3"
##
## >>> Str.clusters  "👍👍👍"
##
clusters : Str -> List Str

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
## For example, `Str.countGraphemes "👩‍👩‍👦‍👦"` returns `1`,
## whereas `Str.toUtf8 "👩‍👩‍👦‍👦"` returns a list with a length of 25,
## `Str.toUtf16 "👩‍👩‍👦‍👦"` returns a list with a length of 11.
## and `Str.toUtf32 "👩‍👩‍👦‍👦"` returns a list with a length of 7.

## Return a #List of the string's #U8 UTF-8 [code units](https://unicode.org/glossary/#code_unit).
## (To split the string into a #List of smaller #Str values instead of #U8 values,
## see #Str.split and #Str.clusters.)
##
## >>> Str.toUtf8 "👩‍👩‍👦‍👦"
##
## >>> Str.toUtf8 "Roc"
##
## >>> Str.toUtf8 "鹏"
##
## >>> Str.toUtf8 "🐦"
##
## For a more flexible function that walks through each of these #U8 code units
## without creating a #List, see #Str.foldUtf8 and #Str.foldRevUtf8.
toUtf8 : Str -> List U8

## Return a #List of the string's #U16 UTF-16 [code units](https://unicode.org/glossary/#code_unit).
## (To split the string into a #List of smaller #Str values instead of #U16 values,
## see #Str.split and #Str.clusters.)
##
## >>> Str.toUtf16 "👩‍👩‍👦‍👦"
##
## >>> Str.toUtf16 "Roc"
##
## >>> Str.toUtf16 "鹏"
##
## >>> Str.toUtf16 "🐦"
##
## For a more flexible function that walks through each of these #U16 code units
## without creating a #List, see #Str.foldUtf16 and #Str.foldRevUtf16.
toUtf16 : Str -> List U16

## Return a #List of the string's #U32 UTF-32 [code units](https://unicode.org/glossary/#code_unit).
## (To split the string into a #List of smaller #Str values instead of #U32 values,
## see #Str.split and #Str.clusters.)
##
## >>> Str.toUtf32 "👩‍👩‍👦‍👦"
##
## >>> Str.toUtf32 "Roc"
##
## >>> Str.toUtf32 "鹏"
##
## >>> Str.toUtf32 "🐦"
##
## For a more flexible function that walks through each of these #U32 code units
## without creating a #List, see #Str.foldUtf32 and #Str.foldRevUtf32.
toUtf32 : Str -> List U32


## Walk through the string's #U8 UTF-8 [code units](https://unicode.org/glossary/#code_unit)
## to build up a state.
## (If you want a `step` function which receives a #Str instead of an #U8, see #Str.foldClusters.)
##
## Here are the #U8 values that will be passed to `step` when this function is
## called on various strings:
##
## * `"👩‍👩‍👦‍👦"` passes 240, 159, 145, 169, 226, 128, 141, 240, 159, 145, 169, 226, 128, 141, 240, 159, 145, 166, 226, 128, 141, 240, 159, 145, 166
## * `"Roc"` passes 82, 111, 99
## * `"鹏"` passes 233, 185, 143
## * `"🐦"` passes 240, 159, 144, 166
##
## To convert a #Str into a plain `List U8` of UTF-8 code units, see #Str.toUtf8.
foldUtf8 : Str, { start: state, step: (state, U8 -> state) } -> state

## Walk through the string's #U16 UTF-16 [code units](https://unicode.org/glossary/#code_unit)
## to build up a state.
## (If you want a `step` function which receives a #Str instead of an #U16, see #Str.foldClusters.)
##
## Here are the #U16 values that will be passed to `step` when this function is
## called on various strings:
##
## * `"👩‍👩‍👦‍👦"` passes 55357, 56425, 8205, 55357, 56425, 8205, 55357, 56422, 8205, 55357, 56422
## * `"Roc"` passes 82, 111, 99
## * `"鹏"` passes 40527
## * `"🐦"` passes 55357, 56358
##
## To convert a #Str into a plain `List U16` of UTF-16 code units, see #Str.toUtf16.
foldUtf16 : Str, { start: state, step: (state, U16 -> state) } -> state

## Walk through the string's #U32 UTF-32 [code units](https://unicode.org/glossary/#code_unit)
## to build up a state.
## (If you want a `step` function which receives a #Str instead of an #U32, see #Str.foldClusters.)
##
## Here are the #U32 values that will be passed to `step` when this function is
## called on various strings:
##
## * `"👩‍👩‍👦‍👦"` passes 128105, 8205, 128105, 8205, 128102, 8205, 128102
## * `"Roc"` passes 82, 111, 99
## * `"鹏"` passes 40527
## * `"🐦"` passes 128038
##
## To convert a #Str into a plain `List U32` of UTF-32 code units, see #Str.toUtf32.
foldUtf32 : Str, { start: state, step: (state, U32 -> state) } -> state


## Walk backwards through the string's #U8 UTF-8 [code units](https://unicode.org/glossary/#code_unit)
## to build up a state.
## (If you want a `step` function which receives a #Str instead of an #U8, see #Str.foldClusters.)
##
## Here are the #U8 values that will be passed to `step` when this function is
## called on various strings:
##
## * `"👩‍👩‍👦‍👦"` passes 166, 145, 159, 240, 141, 128, 226, 166, 145, 159, 240, 141, 128, 226, 169, 145, 159, 240, 141, 128, 226, 169, 145, 159, 240
## * `"Roc"` passes 99, 111, 82
## * `"鹏"` passes 143, 185, 233
## * `"🐦"` passes 166, 144, 159, 240
##
## To convert a #Str into a plain `List U8` of UTF-8 code units, see #Str.toUtf8.
foldRevUtf8 : Str, { start: state, step: (state, U8 -> state) } -> state

## Walk backwards through the string's #U16 UTF-16 [code units](https://unicode.org/glossary/#code_unit)
## to build up a state.
## (If you want a `step` function which receives a #Str instead of an #U16, see #Str.foldClusters.)
##
## Here are the #U16 values that will be passed to `step` when this function is
## called on various strings:
##
## * `"👩‍👩‍👦‍👦"` passes 56422, 55357, 8205, 56422, 55357, 8205, 56425, 55357, 8205, 56425, 55357
## * `"Roc"` passes 99, 111, 82
## * `"鹏"` passes 40527
## * `"🐦"` passes 56358, 55357
##
## To convert a #Str into a plain `List U16` of UTF-16 code units, see #Str.toUtf16.
foldRevUtf16 : Str, { start: state, step: (state, U16 -> state) } -> state

## Walk backwards through the string's #U32 UTF-32 [code units](https://unicode.org/glossary/#code_unit)
## to build up a state.
## (If you want a `step` function which receives a #Str instead of an #U32, see #Str.foldClusters.)
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
foldRevUtf32 : Str, { start: state, step: (state, U32 -> state) } -> state



