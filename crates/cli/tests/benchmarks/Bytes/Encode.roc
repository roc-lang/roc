module [ByteEncoder, sequence, u8, u16, bytes, empty, encode]

Endianness : [BE, LE]

ByteEncoder : [Signed8 I8, Unsigned8 U8, Signed16 Endianness I16, Unsigned16 Endianness U16, Sequence U64 (List ByteEncoder), Bytes (List U8)]

u8 : U8 -> ByteEncoder
u8 = \value -> Unsigned8(value)

empty : ByteEncoder
empty =
    foo : List ByteEncoder
    foo = []

    Sequence(0, foo)

u16 : Endianness, U16 -> ByteEncoder
u16 = \endianness, value -> Unsigned16(endianness, value)

bytes : List U8 -> ByteEncoder
bytes = \bs -> Bytes(bs)

sequence : List ByteEncoder -> ByteEncoder
sequence = \encoders ->
    Sequence(get_widths(encoders, 0), encoders)

get_width : ByteEncoder -> U64
get_width = \encoder ->
    when encoder is
        Signed8(_) -> 1
        Unsigned8(_) -> 1
        Signed16(_, _) -> 2
        Unsigned16(_, _) -> 2
        # Signed32 _ -> 4
        # Unsigned32 _ -> 4
        # Signed64 _ -> 8
        # Unsigned64 _ -> 8
        # Signed128 _ -> 16
        # Unsigned128 _ -> 16
        Sequence(w, _) -> w
        Bytes(bs) -> List.len(bs)

get_widths : List ByteEncoder, U64 -> U64
get_widths = \encoders, initial ->
    List.walk(encoders, initial, \accum, encoder -> accum + get_width(encoder))

encode : ByteEncoder -> List U8
encode = \encoder ->
    output = List.repeat(0, get_width(encoder))

    encode_help(encoder, 0, output)
    |> .output

encode_help : ByteEncoder, U64, List U8 -> { output : List U8, offset : U64 }
encode_help = \encoder, offset, output ->
    when encoder is
        Unsigned8(value) ->
            {
                output: List.set(output, offset, value),
                offset: offset + 1,
            }

        Signed8(value) ->
            cast : U8
            cast = Num.int_cast(value)

            {
                output: List.set(output, offset, cast),
                offset: offset + 1,
            }

        Unsigned16(endianness, value) ->
            a : U8
            a = Num.int_cast(Num.shift_right_by(value, 8))

            b : U8
            b = Num.int_cast(value)

            new_output =
                when endianness is
                    BE ->
                        output
                        |> List.set((offset + 0), a)
                        |> List.set((offset + 1), b)

                    LE ->
                        output
                        |> List.set((offset + 0), b)
                        |> List.set((offset + 1), a)

            {
                output: new_output,
                offset: offset + 2,
            }

        Signed16(endianness, value) ->
            a : U8
            a = Num.int_cast(Num.shift_right_by(value, 8))

            b : U8
            b = Num.int_cast(value)

            new_output =
                when endianness is
                    BE ->
                        output
                        |> List.set((offset + 0), a)
                        |> List.set((offset + 1), b)

                    LE ->
                        output
                        |> List.set((offset + 0), b)
                        |> List.set((offset + 1), a)

            {
                output: new_output,
                offset: offset + 1,
            }

        Bytes(bs) ->
            List.walk(
                bs,
                { output, offset },
                \accum, byte -> {
                    offset: accum.offset + 1,
                    output: List.set(accum.output, offset, byte),
                },
            )

        Sequence(_, encoders) ->
            List.walk(
                encoders,
                { output, offset },
                \accum, single ->
                    encode_help(single, accum.offset, accum.output),
            )
