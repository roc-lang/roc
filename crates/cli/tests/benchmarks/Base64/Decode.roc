module [from_bytes]

import Bytes.Decode exposing [ByteDecoder, DecodeProblem]

from_bytes : List U8 -> Result Str DecodeProblem
from_bytes = \bytes ->
    Bytes.Decode.decode(bytes, decode_base64(List.len(bytes)))

decode_base64 : U64 -> ByteDecoder Str
decode_base64 = \width -> Bytes.Decode.loop(loop_help, { remaining: width, string: "" })

loop_help : { remaining : U64, string : Str } -> ByteDecoder (Bytes.Decode.Step { remaining : U64, string : Str } Str)
loop_help = \{ remaining, string } ->
    if remaining >= 3 then
        Bytes.Decode.map3(Bytes.Decode.u8, Bytes.Decode.u8, Bytes.Decode.u8, \x, y, z ->
            a : U32
            a = Num.intCast(x)
            b : U32
            b = Num.intCast(y)
            c : U32
            c = Num.intCast(z)
            combined = Num.bitwiseOr(Num.bitwiseOr(Num.shiftLeftBy(a, 16), Num.shiftLeftBy(b, 8)), c)

            Loop({
                remaining: remaining - 3,
                string: Str.concat(string, bits_to_chars(combined, 0)),
            }))
    else if remaining == 0 then
        Bytes.Decode.succeed(Done(string))
    else if remaining == 2 then
        Bytes.Decode.map2(Bytes.Decode.u8, Bytes.Decode.u8, \x, y ->

            a : U32
            a = Num.intCast(x)
            b : U32
            b = Num.intCast(y)
            combined = Num.bitwiseOr(Num.shiftLeftBy(a, 16), Num.shiftLeftBy(b, 8))

            Done(Str.concat(string, bits_to_chars(combined, 1))))
    else
        # remaining = 1
        Bytes.Decode.map(Bytes.Decode.u8, \x ->

            a : U32
            a = Num.intCast(x)

            Done(Str.concat(string, bits_to_chars(Num.shiftLeftBy(a, 16), 2))))

bits_to_chars : U32, Int * -> Str
bits_to_chars = \bits, missing ->
    when Str.fromUtf8(bits_to_chars_help(bits, missing)) is
        Ok(str) -> str
        Err(_) -> ""

# Mask that can be used to get the lowest 6 bits of a binary number
lowest6_bits_mask : Int *
lowest6_bits_mask = 63

bits_to_chars_help : U32, Int * -> List U8
bits_to_chars_help = \bits, missing ->
    # The input is 24 bits, which we have to partition into 4 6-bit segments. We achieve this by
    # shifting to the right by (a multiple of) 6 to remove unwanted bits on the right, then `Num.bitwiseAnd`
    # with `0b111111` (which is 2^6 - 1 or 63) (so, 6 1s) to remove unwanted bits on the left.
    # any 6-bit number is a valid base64 digit, so this is actually safe
    p =
        Num.shiftRightZfBy(bits, 18)
        |> Num.intCast
        |> unsafe_to_char

    q =
        Num.bitwiseAnd(Num.shiftRightZfBy(bits, 12), lowest6_bits_mask)
        |> Num.intCast
        |> unsafe_to_char

    r =
        Num.bitwiseAnd(Num.shiftRightZfBy(bits, 6), lowest6_bits_mask)
        |> Num.intCast
        |> unsafe_to_char

    s =
        Num.bitwiseAnd(bits, lowest6_bits_mask)
        |> Num.intCast
        |> unsafe_to_char

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
unsafe_to_char : U8 -> U8
unsafe_to_char = \n ->
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
