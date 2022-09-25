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
    ]
    imports [Bool.{ Bool }, Result.{ Result }, List]

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
Utf8ByteProblem : [
    InvalidStartByte,
    UnexpectedEndOfSequence,
    ExpectedContinuation,
    OverlongEncoding,
    CodepointTooLarge,
    EncodesSurrogateHalf,
]

Utf8Problem : { byteIndex : Nat, problem : Utf8ByteProblem }

## Returns `Bool.true` if the string is empty, and `Bool.false` otherwise.
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
## equal to the given [U32], return `Bool.true`. Otherwise return `Bool.false`.
##
## If the given [Str] is empty, or if the given [U32] is not a valid
## code point, this will return `Bool.false`.
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

fromUtf8 : List U8 -> Result Str [BadUtf8 Utf8ByteProblem Nat]*
fromUtf8 = \bytes ->
    result = fromUtf8RangeLowlevel bytes 0 (List.len bytes)

    if result.cIsOk then
        Ok result.bString
    else
        Err (BadUtf8 result.dProblemCode result.aByteIndex)

fromUtf8Range : List U8, { start : Nat, count : Nat } -> Result Str [BadUtf8 Utf8ByteProblem Nat, OutOfBounds]*
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

startsWith : Str, Str -> Bool
endsWith : Str, Str -> Bool

## Return the string with any blank spaces removed from both the beginning
## as well as the end.
trim : Str -> Str
trimLeft : Str -> Str
trimRight : Str -> Str

toDec : Str -> Result Dec [InvalidNumStr]*
toDec = \string -> strToNumHelp string
toF64 : Str -> Result F64 [InvalidNumStr]*
toF64 = \string -> strToNumHelp string
toF32 : Str -> Result F32 [InvalidNumStr]*
toF32 = \string -> strToNumHelp string
toNat : Str -> Result Nat [InvalidNumStr]*
toNat = \string -> strToNumHelp string
toU128 : Str -> Result U128 [InvalidNumStr]*
toU128 = \string -> strToNumHelp string
toI128 : Str -> Result I128 [InvalidNumStr]*
toI128 = \string -> strToNumHelp string
toU64 : Str -> Result U64 [InvalidNumStr]*
toU64 = \string -> strToNumHelp string
toI64 : Str -> Result I64 [InvalidNumStr]*
toI64 = \string -> strToNumHelp string
toU32 : Str -> Result U32 [InvalidNumStr]*
toU32 = \string -> strToNumHelp string
toI32 : Str -> Result I32 [InvalidNumStr]*
toI32 = \string -> strToNumHelp string
toU16 : Str -> Result U16 [InvalidNumStr]*
toU16 = \string -> strToNumHelp string
toI16 : Str -> Result I16 [InvalidNumStr]*
toI16 = \string -> strToNumHelp string
toU8 : Str -> Result U8 [InvalidNumStr]*
toU8 = \string -> strToNumHelp string
toI8 : Str -> Result I8 [InvalidNumStr]*
toI8 = \string -> strToNumHelp string

## Gets the byte at the given index, without performing a bounds check
getUnsafe : Str, Nat -> U8

## gives the number of string bytes
countUtf8Bytes : Str -> Nat

## string slice that does not do bounds checking or utf-8 verification
substringUnsafe : Str, Nat, Nat -> Str

## Returns the string with each occurrence of a substring replaced with a replacement.
## If the substring is not found, returns `Err NotFound`.
##
##     Str.replaceEach "foo/bar/baz" "/" "_" == Ok "foo_bar_baz"
replaceEach : Str, Str, Str -> Result Str [NotFound]*
replaceEach = \haystack, needle, flower ->
    when splitFirst haystack needle is
        Ok { before, after } ->
            # We found at least one needle, so start the buffer off with
            # `before` followed by the first replacement flower.
            Str.reserve "" (Str.countUtf8Bytes haystack)
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

## Returns the string with the first occurrence of a substring replaced with a replacement.
## If the substring is not found, returns `Err NotFound`.
##
##     Str.replaceFirst "foo/bar/baz" "/" "_" == Ok "foo_bar/baz"
replaceFirst : Str, Str, Str -> Result Str [NotFound]*
replaceFirst = \haystack, needle, flower ->
    when splitFirst haystack needle is
        Ok { before, after } ->
            Ok "\(before)\(flower)\(after)"

        Err err -> Err err

expect Str.replaceFirst "abXdeXghi" "X" "_" == Ok "ab_deXghi"

## Returns the string with the last occurrence of a substring replaced with a replacement.
## If the substring is not found, returns `Err NotFound`.
##
##     Str.replaceLast "foo/bar/baz" "/" "_" == Ok "foo/bar_baz"
replaceLast : Str, Str, Str -> Result Str [NotFound]*
replaceLast = \haystack, needle, flower ->
    when splitLast haystack needle is
        Ok { before, after } ->
            Ok "\(before)\(flower)\(after)"

        Err err -> Err err

expect Str.replaceLast "abXdeXghi" "X" "_" == Ok "abXde_ghi"

## Returns the string before the first occurrence of a delimiter, as well as the
## rest of the string after that occurrence. If the delimiter is not found, returns `Err`.
##
##     Str.splitFirst "foo/bar/baz" "/" == Ok { before: "foo", after: "bar/baz" }
splitFirst : Str, Str -> Result { before : Str, after : Str } [NotFound]*
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

## Returns the string before the last occurrence of a delimiter, as well as the
## rest of the string after that occurrence. If the delimiter is not found, returns `Err`.
##
##     Str.splitLast "foo/bar/baz" "/" == Ok { before: "foo/bar", after: "baz" }
splitLast : Str, Str -> Result { before : Str, after : Str } [NotFound]*
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

    matchesAtHelp haystack haystackIndex needle 0 endIndex

matchesAtHelp : Str, Nat, Str, Nat, Nat -> Bool
matchesAtHelp = \haystack, haystackIndex, needle, needleIndex, endIndex ->
    if haystackIndex < endIndex then
        if Str.getUnsafe haystack haystackIndex == Str.getUnsafe needle needleIndex then
            matchesAtHelp haystack (haystackIndex + 1) needle (needleIndex + 1) endIndex
        else
            Bool.false
    else
        Bool.true

## Walks over the string's UTF-8 bytes, calling a function which updates a state using each
## UTF-8 `U8` byte as well as the index of that byte within the string.
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

## Make sure at least some number of bytes fit in this string without reallocating
reserve : Str, Nat -> Str

## is UB when the scalar is invalid
appendScalarUnsafe : Str, U32 -> Str

appendScalar : Str, U32 -> Result Str [InvalidScalar]*
appendScalar = \string, scalar ->
    if isValidScalar scalar then
        Ok (appendScalarUnsafe string scalar)
    else
        Err InvalidScalar

isValidScalar : U32 -> Bool
isValidScalar = \scalar ->
    scalar <= 0xD7FF || (scalar >= 0xE000 && scalar <= 0x10FFFF)

getScalarUnsafe : Str, Nat -> { scalar : U32, bytesParsed : Nat }

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

strToNumHelp : Str -> Result (Num a) [InvalidNumStr]*
strToNumHelp = \string ->
    result : { berrorcode : U8, aresult : Num a }
    result = strToNum string

    if result.berrorcode == 0 then
        Ok result.aresult
    else
        Err InvalidNumStr
