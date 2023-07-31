interface Base64
    exposes [toBytes, toStr]
    imports []

## Base64-encodes a [Str] into another [Str].
toStr : Str -> Str
toStr = \input ->
    result = toBytes input |> Str.fromUtf8

    ## toBytes should always return valid UTF-8!
    # expect Result.isOk result # TODO put this back once https://github.com/roc-lang/roc/issues/5680 is fixed

    Result.withDefault result ""

## Base64-encodes a [Str] into a [List] `U8`.
toBytes : Str -> List U8
toBytes = \input ->
    len = Str.countUtf8Bytes input

    List.withCapacity (4 * ((2 + len) // 3))
    |> toBytesHelp (Str.toUtf8 input) len 0

toBytesHelp : List U8, List U8, Nat, Nat -> List U8
toBytesHelp = \answer, input, len, index ->
    if index < len then
        octetA = getU32 input (index + 0)
        octetB = getU32 input (index + 1)
        octetC = getU32 input (index + 2)
        triple =
            (octetA |> Num.shiftLeftBy 0x10)
            |> Num.bitwiseOr (octetB |> Num.shiftLeftBy 0x08)
            |> Num.bitwiseOr octetC

        answer
        |> update triple 0 Bool.false
        |> update triple 1 (index + 1 > len)
        |> update triple 2 (index + 2 > len)
        |> update triple 3 (index + 3 > len)
        |> toBytesHelp input len (index + 3)
    else
        answer

getU32 : List U8, Nat -> U32
getU32 = \input, index ->
    input
    |> List.get index
    |> Result.withDefault 0
    |> Num.toU32

update : List U8, U32, Nat, Bool -> List U8
update = \list, triple, index, usePlaceholder ->
    shiftAmount = 6 * (3 - index) |> Num.toU8
    byte = base64Char (triple |> Num.shiftRightBy shiftAmount |> Num.bitwiseAnd 0x3F)
    byteOrPlaceholder = if usePlaceholder then '=' else byte
    List.append list byteOrPlaceholder

base64Char : U32 -> U8 # TODO type checking bug if this takes `Int *`
base64Char = \index ->
    # This should all be branchless!
    isPlusOrSlash = index >= 62
    isUppercase = index < 26
    isLowercase = index >= 26 && index < 52
    isDigit = index >= 52 && index < 62

    offset =
        0
        |> Num.bitwiseOr (if isUppercase then 'A' else 0)
        |> Num.bitwiseOr (if isLowercase then Num.subWrap 'a' 26 else 0)
        |> Num.bitwiseOr (if isDigit then Num.subWrap '0' 52 else 0)
        |> Num.bitwiseOr (if isPlusOrSlash then Num.subWrap '+' index else 0)

    index
    |> Num.addWrap offset
    |> Num.toU8
