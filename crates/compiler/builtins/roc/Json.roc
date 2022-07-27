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
        Decode,
        Decode.{
            DecoderFormatting,
        }
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
         DecoderFormatting {
             u8: decodeU8,
             u16: decodeU16,
             u32: decodeU32,
             u64: decodeU64,
             u128: decodeU128,
             i8: decodeI8,
             i16: decodeI16,
             i32: decodeI32,
             i64: decodeI64,
             i128: decodeI128,
             f32: decodeF32,
             f64: decodeF64,
             dec: decodeDec,
             bool: decodeBool,
             string: decodeString,
             list: decodeList,
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

takeWhile = \list, predicate ->
    helper = \{taken, rest} ->
        when List.first rest is
            Ok elem ->
                if predicate elem then
                    helper {taken: List.append taken elem, rest: List.split rest 1 |> .others}
                else
                    {taken, rest}
            Err _ -> {taken, rest}
    helper {taken: [], rest: list}

digits = List.range 48u8 58u8

takeDigits = \bytes ->
    takeWhile bytes \n -> List.contains digits n

decodeU8 = Decode.custom \bytes, @Json {} ->
    {taken, rest} = takeDigits bytes
    when Str.fromUtf8 taken |> Result.try Str.toU8 is
        Ok n -> {result: Ok n, rest}
        Err _ -> {result: Err TooShort, rest}

decodeU16 = Decode.custom \bytes, @Json {} ->
    {taken, rest} = takeDigits bytes
    when Str.fromUtf8 taken |> Result.try Str.toU16 is
        Ok n -> {result: Ok n, rest}
        Err _ -> {result: Err TooShort, rest}

decodeU32 = Decode.custom \bytes, @Json {} ->
    {taken, rest} = takeDigits bytes
    when Str.fromUtf8 taken |> Result.try Str.toU32 is
        Ok n -> {result: Ok n, rest}
        Err _ -> {result: Err TooShort, rest}

decodeU64 = Decode.custom \bytes, @Json {} ->
    {taken, rest} = takeDigits bytes
    when Str.fromUtf8 taken |> Result.try Str.toU64 is
        Ok n -> {result: Ok n, rest}
        Err _ -> {result: Err TooShort, rest}

decodeU128 = Decode.custom \bytes, @Json {} ->
    {taken, rest} = takeDigits bytes
    when Str.fromUtf8 taken |> Result.try Str.toU128 is
        Ok n -> {result: Ok n, rest}
        Err _ -> {result: Err TooShort, rest}

decodeI8 = Decode.custom \bytes, @Json {} ->
    {taken, rest} = takeDigits bytes
    when Str.fromUtf8 taken |> Result.try Str.toI8 is
        Ok n -> {result: Ok n, rest}
        Err _ -> {result: Err TooShort, rest}

decodeI16 = Decode.custom \bytes, @Json {} ->
    {taken, rest} = takeDigits bytes
    when Str.fromUtf8 taken |> Result.try Str.toI16 is
        Ok n -> {result: Ok n, rest}
        Err _ -> {result: Err TooShort, rest}

decodeI32 = Decode.custom \bytes, @Json {} ->
    {taken, rest} = takeDigits bytes
    when Str.fromUtf8 taken |> Result.try Str.toI32 is
        Ok n -> {result: Ok n, rest}
        Err _ -> {result: Err TooShort, rest}

decodeI64 = Decode.custom \bytes, @Json {} ->
    {taken, rest} = takeDigits bytes
    when Str.fromUtf8 taken |> Result.try Str.toI64 is
        Ok n -> {result: Ok n, rest}
        Err _ -> {result: Err TooShort, rest}

decodeI128 = Decode.custom \bytes, @Json {} ->
    {taken, rest} = takeDigits bytes
    when Str.fromUtf8 taken |> Result.try Str.toI128 is
        Ok n -> {result: Ok n, rest}
        Err _ -> {result: Err TooShort, rest}

decodeF32 = Decode.custom \bytes, @Json {} ->
    {taken, rest} = takeDigits bytes
    when Str.fromUtf8 taken |> Result.try Str.toF32 is
        Ok n -> {result: Ok n, rest}
        Err _ -> {result: Err TooShort, rest}

decodeF64 = Decode.custom \bytes, @Json {} ->
    {taken, rest} = takeDigits bytes
    when Str.fromUtf8 taken |> Result.try Str.toF64 is
        Ok n -> {result: Ok n, rest}
        Err _ -> {result: Err TooShort, rest}

decodeDec = Decode.custom \bytes, @Json {} ->
    {taken, rest} = takeDigits bytes
    when Str.fromUtf8 taken |> Result.try Str.toDec is
        Ok n -> {result: Ok n, rest}
        Err _ -> {result: Err TooShort, rest}

decodeBool = Decode.custom \bytes, @Json {} -> {result: Err TooShort, rest: bytes}
decodeString = Decode.custom \bytes, @Json {} -> {result: Err TooShort, rest: bytes}
decodeList = \decodeElem -> Decode.custom \bytes, @Json {} ->
    when Decode.decodeWith bytes decodeElem (@Json {}) is
        _ -> {result: Err TooShort, rest: bytes}
