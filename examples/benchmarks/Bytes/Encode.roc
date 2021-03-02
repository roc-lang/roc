interface Bytes.Encode exposes [ Encoder, empty, sequence, u8, bytes, encode ] imports []

Encoder : [ Signed8 I8, Unsigned8 U8, Sequence Nat (List Encoder), Bytes (List U8) ]

empty : Encoder
empty = 
    Sequence 0 []

u8 : U8 -> Encoder
u8 = \value -> Unsigned8 value

sequence : List Encoder -> Encoder
sequence = \encoders ->
    Sequence (getWidths encoders 0) encoders

getWidth : Encoder -> Nat
getWidth = \encoder ->
    when encoder is
        Signed8 _ -> 1
        Unsigned8 _ -> 1
        # Signed16 _ -> 2
        # Unsigned16 _ -> 2
        # Signed32 _ -> 4
        # Unsigned32 _ -> 4
        # Signed64 _ -> 8
        # Unsigned64 _ -> 8
        # Signed128 _ -> 16
        # Unsigned128 _ -> 16
        Sequence w _ -> w
        Bytes bs -> List.len bs 

getWidths = \encoders, initial -> List.walk encoders (\encoder, accum -> accum + getWidth encoder) initial

bytes : List U8 -> Encoder
bytes = \bs -> Bytes bs


encode : Encoder -> List U8
encode = \encoder ->
    output = List.repeat (getWidth encoder) 0

    encodeHelp encoder 0 output
        |> .output

encodeHelp = \encoder, offset, output ->
    when encoder is
        Unsigned8 value ->
            {
                output: List.set output offset value,
                offset: offset + 1
            }

        Signed8 value ->
            cast : U8
            cast = Num.intCast value

            {
                output: List.set output offset cast,
                offset: offset + 1
            }

        Bytes bs ->
            List.walk bs (\byte, accum -> { offset: accum.offset + 1, output : List.set accum.output offset byte }) { output, offset }

        Sequence _ encoders ->
            List.walk encoders (\single, accum -> encodeHelp single accum.offset accum.output) { output, offset }
