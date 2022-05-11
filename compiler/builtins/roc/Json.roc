interface Json
    exposes
        [
            Json, format
        ]
    imports
        [
            Encode.{
                custom,
                appendWith,
                u8, u16, u32, u64, u128,
                i8, i16, i32, i64, i128,
                f32, f64, dec,
                bool,
                string,
                list,
            }
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
                if b
                then List.concat bytes (Str.toUtf8 "true")
                else List.concat bytes (Str.toUtf8 "false")

string = \s -> custom \bytes, @Json {} ->
                    List.append bytes (Num.toU8 '"')
                    |> List.concat (Str.toUtf8 s)
                    |> List.append (Num.toU8 '"')

list = \lst, encodeElem ->
    custom \bytes, @Json {} ->
        head = List.append bytes (Num.toU8 '[')
        withList = List.walk lst head (\bytes1, elem -> appendWith bytes1 (encodeElem elem) (@Json {}))
        List.append withList (Num.toU8 ']')
