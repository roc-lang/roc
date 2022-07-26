interface Json
    exposes [
        Json,
        toUtf8,
    ]
    imports [
        List,
        Str,
        Encode,
        Encode.{
            Encoder,
            EncoderFormatting,
            appendWith,
        },
    ]

Json := {} has [
         EncoderFormatting {
             u8: encodeU8,
             u16: encodeU16,
             u32: encodeU32,
             u64: encodeU64,
             u128: encodeU128,
             i8: encodeI8,
             i16: encodeI16,
             i32: encodeI32,
             i64: encodeI64,
             i128: encodeI128,
             f32: encodeF32,
             f64: encodeF64,
             dec: encodeDec,
             bool: encodeBool,
             string: encodeString,
             list: encodeList,
             record: encodeRecord,
             tag: encodeTag,
         },
     ]

toUtf8 = @Json {}

numToBytes = \n ->
    n |> Num.toStr |> Str.toUtf8

encodeU8 = \n -> Encode.custom \bytes, @Json {} -> List.concat bytes (numToBytes n)

encodeU16 = \n -> Encode.custom \bytes, @Json {} -> List.concat bytes (numToBytes n)

encodeU32 = \n -> Encode.custom \bytes, @Json {} -> List.concat bytes (numToBytes n)

encodeU64 = \n -> Encode.custom \bytes, @Json {} -> List.concat bytes (numToBytes n)

encodeU128 = \n -> Encode.custom \bytes, @Json {} -> List.concat bytes (numToBytes n)

encodeI8 = \n -> Encode.custom \bytes, @Json {} -> List.concat bytes (numToBytes n)

encodeI16 = \n -> Encode.custom \bytes, @Json {} -> List.concat bytes (numToBytes n)

encodeI32 = \n -> Encode.custom \bytes, @Json {} -> List.concat bytes (numToBytes n)

encodeI64 = \n -> Encode.custom \bytes, @Json {} -> List.concat bytes (numToBytes n)

encodeI128 = \n -> Encode.custom \bytes, @Json {} -> List.concat bytes (numToBytes n)

encodeF32 = \n -> Encode.custom \bytes, @Json {} -> List.concat bytes (numToBytes n)

encodeF64 = \n -> Encode.custom \bytes, @Json {} -> List.concat bytes (numToBytes n)

encodeDec = \n -> Encode.custom \bytes, @Json {} -> List.concat bytes (numToBytes n)

encodeBool = \b -> Encode.custom \bytes, @Json {} ->
        if
            b
        then
            List.concat bytes (Str.toUtf8 "true")
        else
            List.concat bytes (Str.toUtf8 "false")

encodeString = \s -> Encode.custom \bytes, @Json {} ->
        List.append bytes (Num.toU8 '"')
        |> List.concat (Str.toUtf8 s)
        |> List.append (Num.toU8 '"')

encodeList = \lst, encodeElem ->
    Encode.custom \bytes, @Json {} ->
        writeList = \{ buffer, elemsLeft }, elem ->
            bufferWithElem = appendWith buffer (encodeElem elem) (@Json {})
            bufferWithSuffix =
                if elemsLeft > 1 then
                    List.append bufferWithElem (Num.toU8 ',')
                else
                    bufferWithElem

            { buffer: bufferWithSuffix, elemsLeft: elemsLeft - 1 }

        head = List.append bytes (Num.toU8 '[')
        { buffer: withList } = List.walk lst { buffer: head, elemsLeft: List.len lst } writeList

        List.append withList (Num.toU8 ']')

encodeRecord = \fields ->
    Encode.custom \bytes, @Json {} ->
        writeRecord = \{ buffer, fieldsLeft }, { key, value } ->
            bufferWithKeyValue =
                List.append buffer (Num.toU8 '"')
                |> List.concat (Str.toUtf8 key)
                |> List.append (Num.toU8 '"')
                |> List.append (Num.toU8 ':')
                |> appendWith value (@Json {})

            bufferWithSuffix =
                if fieldsLeft > 1 then
                    List.append bufferWithKeyValue (Num.toU8 ',')
                else
                    bufferWithKeyValue

            { buffer: bufferWithSuffix, fieldsLeft: fieldsLeft - 1 }

        bytesHead = List.append bytes (Num.toU8 '{')
        { buffer: bytesWithRecord } = List.walk fields { buffer: bytesHead, fieldsLeft: List.len fields } writeRecord

        List.append bytesWithRecord (Num.toU8 '}')

encodeTag = \name, payload ->
    Encode.custom \bytes, @Json {} ->
        # Idea: encode `A v1 v2` as `{"A": [v1, v2]}`
        writePayload = \{ buffer, itemsLeft }, encoder ->
            bufferWithValue = appendWith buffer encoder (@Json {})
            bufferWithSuffix =
                if itemsLeft > 1 then
                    List.append bufferWithValue (Num.toU8 ',')
                else
                    bufferWithValue

            { buffer: bufferWithSuffix, itemsLeft: itemsLeft - 1 }

        bytesHead =
            List.append bytes (Num.toU8 '{')
            |> List.append (Num.toU8 '"')
            |> List.concat (Str.toUtf8 name)
            |> List.append (Num.toU8 '"')
            |> List.append (Num.toU8 ':')
            |> List.append (Num.toU8 '[')

        { buffer: bytesWithPayload } = List.walk payload { buffer: bytesHead, itemsLeft: List.len payload } writePayload

        List.append bytesWithPayload (Num.toU8 ']')
        |> List.append (Num.toU8 '}')
