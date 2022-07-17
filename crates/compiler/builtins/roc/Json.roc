interface Json
    exposes [
        Json,
        format,
    ]
    imports [
        List,
        Str,
        Encode.{
            Encoder,
            custom,
            appendWith,
            u8,
            u16,
            u32,
            u64,
            u128,
            i8,
            i16,
            i32,
            i64,
            i128,
            f32,
            f64,
            dec,
            bool,
            string,
            list,
            record,
            tag,
        },
    ]

Json := {}

format = @Json {}

numToBytes = \n ->
    n |> Num.toStr |> Str.toUtf8

# impl EncoderFormatting for Json
u8 = \n -> custom \bytes, @Json {} -> List.concat bytes (numToBytes n)

u16 = \n -> custom \bytes, @Json {} -> List.concat bytes (numToBytes n)

u32 = \n -> custom \bytes, @Json {} -> List.concat bytes (numToBytes n)

u64 = \n -> custom \bytes, @Json {} -> List.concat bytes (numToBytes n)

u128 = \n -> custom \bytes, @Json {} -> List.concat bytes (numToBytes n)

i8 = \n -> custom \bytes, @Json {} -> List.concat bytes (numToBytes n)

i16 = \n -> custom \bytes, @Json {} -> List.concat bytes (numToBytes n)

i32 = \n -> custom \bytes, @Json {} -> List.concat bytes (numToBytes n)

i64 = \n -> custom \bytes, @Json {} -> List.concat bytes (numToBytes n)

i128 = \n -> custom \bytes, @Json {} -> List.concat bytes (numToBytes n)

f32 = \n -> custom \bytes, @Json {} -> List.concat bytes (numToBytes n)

f64 = \n -> custom \bytes, @Json {} -> List.concat bytes (numToBytes n)

dec = \n -> custom \bytes, @Json {} -> List.concat bytes (numToBytes n)

bool = \b -> custom \bytes, @Json {} ->
        if
            b
        then
            List.concat bytes (Str.toUtf8 "true")
        else
            List.concat bytes (Str.toUtf8 "false")

string = \s -> custom \bytes, @Json {} ->
        List.append bytes (Num.toU8 '"')
        |> List.concat (Str.toUtf8 s)
        |> List.append (Num.toU8 '"')

list = \lst, encodeElem ->
    custom \bytes, @Json {} ->
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

record = \fields ->
    custom \bytes, @Json {} ->
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

tag = \name, payload ->
    custom \bytes, @Json {} ->
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
