interface Base64.Encode
    exposes [toBytes]
    imports [Bytes.Encode.{ ByteEncoder }]

InvalidChar : U8

# State : [None, One U8, Two U8, Three U8]
toBytes : Str -> List U8
toBytes = \str ->
    str
    |> Str.toUtf8
    |> encodeChunks
    |> Bytes.Encode.sequence
    |> Bytes.Encode.encode

encodeChunks : List U8 -> List ByteEncoder
encodeChunks = \bytes ->
    List.walk bytes { output: [], accum: None } folder
    |> encodeResidual

coerce : U64, a -> a
coerce = \_, x -> x

# folder : { output : List ByteEncoder, accum : State }, U8 -> { output : List ByteEncoder, accum : State }
folder = \{ output, accum }, char ->
    when accum is
        Unreachable n -> coerce n { output, accum: Unreachable n }
        None -> { output, accum: One char }
        One a -> { output, accum: Two a char }
        Two a b -> { output, accum: Three a b char }
        Three a b c ->
            when encodeCharacters a b c char is
                Ok encoder ->
                    {
                        output: List.append output encoder,
                        accum: None,
                    }

                Err _ ->
                    { output, accum: None }

#  SGVs bG8g V29y bGQ=
# encodeResidual : { output : List ByteEncoder, accum : State } -> List ByteEncoder
encodeResidual = \{ output, accum } ->
    when accum is
        Unreachable _ -> output
        None -> output
        One _ -> output
        Two a b ->
            when encodeCharacters a b equals equals is
                Ok encoder -> List.append output encoder
                Err _ -> output

        Three a b c ->
            when encodeCharacters a b c equals is
                Ok encoder -> List.append output encoder
                Err _ -> output

equals : U8
equals = 61

# Convert 4 characters to 24 bits (as an ByteEncoder)
encodeCharacters : U8, U8, U8, U8 -> Result ByteEncoder InvalidChar
encodeCharacters = \a, b, c, d ->
    if !(isValidChar a) then
        Err a
    else if !(isValidChar b) then
        Err b
    else
        # `=` is the padding character, and must be special-cased
        # only the `c` and `d` char are allowed to be padding
        n1 = unsafeConvertChar a
        n2 = unsafeConvertChar b

        x : U32
        x = Num.intCast n1

        y : U32
        y = Num.intCast n2

        if d == equals then
            if c == equals then
                n = Num.bitwiseOr (Num.shiftLeftBy x 18) (Num.shiftLeftBy y 12)

                # masking higher bits is not needed, Encode.unsignedInt8 ignores higher bits
                b1 : U8
                b1 = Num.intCast (Num.shiftRightBy n 16)

                Ok (Bytes.Encode.u8 b1)
            else if !(isValidChar c) then
                Err c
            else
                n3 = unsafeConvertChar c

                z : U32
                z = Num.intCast n3

                n = Num.bitwiseOr (Num.bitwiseOr (Num.shiftLeftBy x 18) (Num.shiftLeftBy y 12)) (Num.shiftLeftBy z 6)

                combined : U16
                combined = Num.intCast (Num.shiftRightBy n 8)

                Ok (Bytes.Encode.u16 BE combined)
        else if !(isValidChar d) then
            Err d
        else
            n3 = unsafeConvertChar c
            n4 = unsafeConvertChar d

            z : U32
            z = Num.intCast n3

            w : U32
            w = Num.intCast n4

            n =
                Num.bitwiseOr
                    (Num.bitwiseOr (Num.shiftLeftBy x 18) (Num.shiftLeftBy y 12))
                    (Num.bitwiseOr (Num.shiftLeftBy z 6) w)

            b3 : U8
            b3 = Num.intCast n

            combined : U16
            combined = Num.intCast (Num.shiftRightBy n 8)

            Ok (Bytes.Encode.sequence [Bytes.Encode.u16 BE combined, Bytes.Encode.u8 b3])

# is the character a base64 digit?
# The base16 digits are: A-Z, a-z, 0-1, '+' and '/'
isValidChar : U8 -> Bool
isValidChar = \c ->
    if isAlphaNum c then
        Bool.true
    else
        when c is
            43 ->
                # '+'
                Bool.true

            47 ->
                # '/'
                Bool.true

            _ ->
                Bool.false

isAlphaNum : U8 -> Bool
isAlphaNum = \key ->
    (key >= 48 && key <= 57) || (key >= 64 && key <= 90) || (key >= 97 && key <= 122)

# Convert a base64 character/digit to its index
# See also [Wikipedia](https://en.wikipedia.org/wiki/Base64#Base64_table)
unsafeConvertChar : U8 -> U8
unsafeConvertChar = \key ->
    if key >= 65 && key <= 90 then
        # A-Z
        key - 65
    else if key >= 97 && key <= 122 then
        # a-z
        (key - 97) + 26
    else if key >= 48 && key <= 57 then
        # 0-9
        (key - 48) + 26 + 26
    else
        when key is
            43 ->
                # '+'
                62

            47 ->
                # '/'
                63

            _ ->
                0
