interface Base64.Decode exposes [fromBytes] imports [Bytes.Decode.{ ByteDecoder, DecodeProblem }]

fromBytes : List U8 -> Result Str DecodeProblem
fromBytes = \bytes ->
    Bytes.Decode.decode bytes (decodeBase64 (List.len bytes))

decodeBase64 : Nat -> ByteDecoder Str
decodeBase64 = \width -> Bytes.Decode.loop loopHelp { remaining: width, string: "" }

loopHelp : { remaining : Nat, string : Str } -> ByteDecoder (Bytes.Decode.Step { remaining : Nat, string : Str } Str)
loopHelp = \{ remaining, string } ->
    if remaining >= 3 then
        x, y, z <- Bytes.Decode.map3 Bytes.Decode.u8 Bytes.Decode.u8 Bytes.Decode.u8

        a : U32
        a = Num.intCast x
        b : U32
        b = Num.intCast y
        c : U32
        c = Num.intCast z
        combined = Num.bitwiseOr (Num.bitwiseOr (Num.shiftLeftBy a 16) (Num.shiftLeftBy b 8)) c

        Loop {
            remaining: remaining - 3,
            string: Str.concat string (bitsToChars combined 0),
        }
    else if remaining == 0 then
        Bytes.Decode.succeed (Done string)
    else if remaining == 2 then
        x, y <- Bytes.Decode.map2 Bytes.Decode.u8 Bytes.Decode.u8

        a : U32
        a = Num.intCast x
        b : U32
        b = Num.intCast y
        combined = Num.bitwiseOr (Num.shiftLeftBy a 16) (Num.shiftLeftBy b 8)

        Done (Str.concat string (bitsToChars combined 1))
    else
        # remaining = 1
        x <- Bytes.Decode.map Bytes.Decode.u8

        a : U32
        a = Num.intCast x

        Done (Str.concat string (bitsToChars (Num.shiftLeftBy a 16) 2))

bitsToChars : U32, Int * -> Str
bitsToChars = \bits, missing ->
    when Str.fromUtf8 (bitsToCharsHelp bits missing) is
        Ok str -> str
        Err _ -> ""

# Mask that can be used to get the lowest 6 bits of a binary number
lowest6BitsMask : Int *
lowest6BitsMask = 63

bitsToCharsHelp : U32, Int * -> List U8
bitsToCharsHelp = \bits, missing ->
    # The input is 24 bits, which we have to partition into 4 6-bit segments. We achieve this by
    # shifting to the right by (a multiple of) 6 to remove unwanted bits on the right, then `Num.bitwiseAnd`
    # with `0b111111` (which is 2^6 - 1 or 63) (so, 6 1s) to remove unwanted bits on the left.
    # any 6-bit number is a valid base64 digit, so this is actually safe
    p =
        Num.shiftRightZfBy bits 18
        |> Num.intCast
        |> unsafeToChar

    q =
        Num.bitwiseAnd (Num.shiftRightZfBy bits 12) lowest6BitsMask
        |> Num.intCast
        |> unsafeToChar

    r =
        Num.bitwiseAnd (Num.shiftRightZfBy bits 6) lowest6BitsMask
        |> Num.intCast
        |> unsafeToChar

    s =
        Num.bitwiseAnd bits lowest6BitsMask
        |> Num.intCast
        |> unsafeToChar

    equals : U8
    equals = 61

    when missing is
        0 -> [p, q, r, s]
        1 -> [p, q, r, equals]
        2 -> [p, q, equals, equals]
        _ ->
            # unreachable
            []

# Base64 index to character/digit
unsafeToChar : U8 -> U8
unsafeToChar = \n ->
    if n <= 25 then
        # uppercase characters
        65 + n
    else if n <= 51 then
        # lowercase characters
        97 + (n - 26)
    else if n <= 61 then
        # digit characters
        48 + (n - 52)
    else
        # special cases
        when n is
            62 ->
                # '+'
                43

            63 ->
                # '/'
                47

            _ ->
                # anything else is invalid '\u{0000}'
                0
