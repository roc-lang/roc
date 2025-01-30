module [to_bytes]

import Bytes.Encode exposing [ByteEncoder]

InvalidChar : U8

# State : [None, One U8, Two U8, Three U8]
to_bytes : Str -> List U8
to_bytes = \str ->
    str
    |> Str.to_utf8
    |> encode_chunks
    |> Bytes.Encode.sequence
    |> Bytes.Encode.encode

encode_chunks : List U8 -> List ByteEncoder
encode_chunks = \bytes ->
    List.walk(bytes, { output: [], accum: None }, folder)
    |> encode_residual

coerce : U64, a -> a
coerce = \_, x -> x

# folder : { output : List ByteEncoder, accum : State }, U8 -> { output : List ByteEncoder, accum : State }
folder = \{ output, accum }, char ->
    when accum is
        Unreachable(n) -> coerce(n, { output, accum: Unreachable(n) })
        None -> { output, accum: One(char) }
        One(a) -> { output, accum: Two(a, char) }
        Two(a, b) -> { output, accum: Three(a, b, char) }
        Three(a, b, c) ->
            when encode_characters(a, b, c, char) is
                Ok(encoder) ->
                    {
                        output: List.append(output, encoder),
                        accum: None,
                    }

                Err(_) ->
                    { output, accum: None }

#  SGVs bG8g V29y bGQ=
# encodeResidual : { output : List ByteEncoder, accum : State } -> List ByteEncoder
encode_residual = \{ output, accum } ->
    when accum is
        Unreachable(_) -> output
        None -> output
        One(_) -> output
        Two(a, b) ->
            when encode_characters(a, b, equals, equals) is
                Ok(encoder) -> List.append(output, encoder)
                Err(_) -> output

        Three(a, b, c) ->
            when encode_characters(a, b, c, equals) is
                Ok(encoder) -> List.append(output, encoder)
                Err(_) -> output

equals : U8
equals = 61

# Convert 4 characters to 24 bits (as an ByteEncoder)
encode_characters : U8, U8, U8, U8 -> Result ByteEncoder InvalidChar
encode_characters = \a, b, c, d ->
    if !(is_valid_char(a)) then
        Err(a)
    else if !(is_valid_char(b)) then
        Err(b)
    else
        # `=` is the padding character, and must be special-cased
        # only the `c` and `d` char are allowed to be padding
        n1 = unsafe_convert_char(a)
        n2 = unsafe_convert_char(b)

        x : U32
        x = Num.int_cast(n1)

        y : U32
        y = Num.int_cast(n2)

        if d == equals then
            if c == equals then
                n = Num.bitwise_or(Num.shift_left_by(x, 18), Num.shift_left_by(y, 12))

                # masking higher bits is not needed; U8 ignores higher bits
                b1 : U8
                b1 = Num.int_cast(Num.shift_right_by(n, 16))

                Ok(Bytes.Encode.u8(b1))
            else if !(is_valid_char(c)) then
                Err(c)
            else
                n3 = unsafe_convert_char(c)

                z : U32
                z = Num.int_cast(n3)

                n = Num.bitwise_or(Num.bitwise_or(Num.shift_left_by(x, 18), Num.shift_left_by(y, 12)), Num.shift_left_by(z, 6))

                combined : U16
                combined = Num.int_cast(Num.shift_right_by(n, 8))

                Ok(Bytes.Encode.u16(BE, combined))
        else if !(is_valid_char(d)) then
            Err(d)
        else
            n3 = unsafe_convert_char(c)
            n4 = unsafe_convert_char(d)

            z : U32
            z = Num.int_cast(n3)

            w : U32
            w = Num.int_cast(n4)

            n =
                Num.bitwise_or(
                    Num.bitwise_or(Num.shift_left_by(x, 18), Num.shift_left_by(y, 12)),
                    Num.bitwise_or(Num.shift_left_by(z, 6), w),
                )

            b3 : U8
            b3 = Num.int_cast(n)

            combined : U16
            combined = Num.int_cast(Num.shift_right_by(n, 8))

            Ok(Bytes.Encode.sequence([Bytes.Encode.u16(BE, combined), Bytes.Encode.u8(b3)]))

# is the character a base64 digit?
# The base16 digits are: A-Z, a-z, 0-1, '+' and '/'
is_valid_char : U8 -> Bool
is_valid_char = \c ->
    if is_alpha_num(c) then
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

is_alpha_num : U8 -> Bool
is_alpha_num = \key ->
    (key >= 48 and key <= 57) or (key >= 64 and key <= 90) or (key >= 97 and key <= 122)

# Convert a base64 character/digit to its index
# See also [Wikipedia](https://en.wikipedia.org/wiki/Base64#Base64_table)
unsafe_convert_char : U8 -> U8
unsafe_convert_char = \key ->
    if key >= 65 and key <= 90 then
        # A-Z
        key - 65
    else if key >= 97 and key <= 122 then
        # a-z
        (key - 97) + 26
    else if key >= 48 and key <= 57 then
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
