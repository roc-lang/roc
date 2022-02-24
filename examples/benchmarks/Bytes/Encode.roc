interface Bytes.Encode exposes [ Encoder, sequence, u8, u16, bytes, empty, encode ] imports []

Endianness : [ BE, LE ]

Encoder : [ Signed8 I8, Unsigned8 U8, Signed16 Endianness I16, Unsigned16 Endianness U16, Sequence Nat (List Encoder), Bytes (List U8) ]

u8 : U8 -> Encoder
u8 = \value -> Unsigned8 value

empty : Encoder
empty =
    foo : List Encoder
    foo = []

    Sequence 0 foo

u16 : Endianness, U16 -> Encoder
u16 = \endianness, value -> Unsigned16 endianness value

bytes : List U8 -> Encoder
bytes = \bs -> Bytes bs

sequence : List Encoder -> Encoder
sequence = \encoders ->
    Sequence (getWidths encoders 0) encoders

getWidth : Encoder -> Nat
getWidth = \encoder ->
    when encoder is
        Signed8 _ ->
            1

        Unsigned8 _ ->
            1

        Signed16 _ _ ->
            2

        Unsigned16 _ _ ->
            2

        # Signed32 _ -> 4
        # Unsigned32 _ -> 4
        # Signed64 _ -> 8
        # Unsigned64 _ -> 8
        # Signed128 _ -> 16
        # Unsigned128 _ -> 16
        Sequence w _ ->
            w

        Bytes bs ->
            List.len bs

getWidths : List Encoder, Nat -> Nat
getWidths = \encoders, initial ->
    List.walk encoders initial \accum, encoder -> accum + getWidth encoder

encode : Encoder -> List U8
encode = \encoder ->
    output = List.repeat 0 (getWidth encoder)

    encodeHelp encoder 0 output
        |> .output

encodeHelp : Encoder, Nat, List U8 -> { output : List U8, offset : Nat }
encodeHelp = \encoder, offset, output ->
    when encoder is
        Unsigned8 value ->
            {
                output: List.set output offset value,
                offset: offset + 1,
            }

        Signed8 value ->
            cast : U8
            cast = Num.intCast value

            {
                output: List.set output offset cast,
                offset: offset + 1,
            }

        Unsigned16 endianness value ->
            a : U8
            a = Num.intCast (Num.shiftRightBy 8 value)

            b : U8
            b = Num.intCast value

            newOutput =
                when endianness is
                    BE ->
                        output
                            |> List.set (offset + 0) a
                            |> List.set (offset + 1) b

                    LE ->
                        output
                            |> List.set (offset + 0) b
                            |> List.set (offset + 1) a

            {
                output: newOutput,
                offset: offset + 2,
            }

        Signed16 endianness value ->
            a : U8
            a = Num.intCast (Num.shiftRightBy 8 value)

            b : U8
            b = Num.intCast value

            newOutput =
                when endianness is
                    BE ->
                        output
                            |> List.set (offset + 0) a
                            |> List.set (offset + 1) b

                    LE ->
                        output
                            |> List.set (offset + 0) b
                            |> List.set (offset + 1) a

            {
                output: newOutput,
                offset: offset + 1,
            }

        Bytes bs ->
            List.walk
                bs
                { output, offset }
                \accum, byte ->
                    {
                        offset: accum.offset + 1,
                        output: List.set accum.output offset byte,
                    }

        Sequence _ encoders ->
            List.walk
                encoders
                { output, offset }
                \accum, single ->
                    encodeHelp single accum.offset accum.output
