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

decimal : Float *, Nat -> Str


num : Float *, Nat -> Str

split : Str, Str -> List Str

lines : Str, Str -> List Str

## Check

isEmpty : Str -> Bool

startsWith : Str, Str -> Bool

startsWithCodePt : Str, U32 -> Bool

endsWith : Str, Str -> Bool

contains : Str, Str -> Bool

anyGraphemes : Str, (Str -> Bool) -> Bool

allGraphemes : Str, (Str -> Bool) -> Bool

## Combine

join : List Str -> Str

joinWith : List Str, Str -> Str

padGraphemesStart : Str, Nat, Str -> Str

padGraphemesEnd : Str, Nat, Str -> Str

## Graphemes

graphemes : Str -> List Str

countGraphemes : Str -> Nat

reverseGraphemes : Str -> Str

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

trim : Str -> Str

fromScalar : U32 -> Result Str [ BadScalar ]*
fromCodePts : List U32 -> Result Str [ BadCodePt U32 ]*
fromUtf8 : List U8 -> Result Str [ BadUtf8 ]*




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

## If the bytes begin with a valid [extended grapheme cluster](http://www.unicode.org/glossary/#extended_grapheme_cluster)
## encoded as [UTF-8](https://en.wikipedia.org/wiki/UTF-8), return it along with the number of bytes it took up.
##
## If the bytes do not begin with a valid grapheme, for example because the list was
## empty or began with an invalid grapheme, return `Err`.
parseUtf8Grapheme : List U8 -> Result { grapheme : Str, bytesParsed: Nat } [ InvalidGrapheme ]*

## If the bytes begin with a valid [Unicode code point](http://www.unicode.org/glossary/#code_point)
## encoded as [UTF-8](https://en.wikipedia.org/wiki/UTF-8), return it along with the number of bytes it took up.
##
## If the string does not begin with a valid code point, for example because the list was
## empty or began with an invalid code point, return an `Err`.
parseUtf8CodePt : List U8 -> Result { codePt : U32, bytesParsed: Nat } [ InvalidCodePt ]*

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
