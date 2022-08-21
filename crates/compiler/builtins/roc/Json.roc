interface Json
    exposes [
        Json,
        toUtf8,
        fromUtf8,
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
            DecodeResult,
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
             record: decodeRecord,
         },
     ]

toUtf8 = @Json {}

fromUtf8 = @Json {}

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
    helper = \{ taken, rest } ->
        when List.first rest is
            Ok elem ->
                if predicate elem then
                    helper { taken: List.append taken elem, rest: List.split rest 1 |> .others }
                else
                    { taken, rest }

            Err _ -> { taken, rest }

    helper { taken: [], rest: list }

asciiByte = \b -> Num.toU8 b

digits = List.range (asciiByte '0') (asciiByte '9' + 1)

takeDigits = \bytes ->
    takeWhile bytes \n -> List.contains digits n

takeFloat = \bytes ->
    { taken: intPart, rest } = takeDigits bytes

    when List.get rest 0 is
        Ok 46 -> # 46 = .
            { taken: floatPart, rest: afterAll } = takeDigits (List.split rest 1).others
            builtFloat =
                List.concat (List.append intPart (asciiByte '.')) floatPart

            { taken: builtFloat, rest: afterAll }

        _ ->
            { taken: intPart, rest }

decodeU8 = Decode.custom \bytes, @Json {} ->
    { taken, rest } = takeDigits bytes

    when Str.fromUtf8 taken |> Result.try Str.toU8 is
        Ok n -> { result: Ok n, rest }
        Err _ -> { result: Err TooShort, rest }

decodeU16 = Decode.custom \bytes, @Json {} ->
    { taken, rest } = takeDigits bytes

    when Str.fromUtf8 taken |> Result.try Str.toU16 is
        Ok n -> { result: Ok n, rest }
        Err _ -> { result: Err TooShort, rest }

decodeU32 = Decode.custom \bytes, @Json {} ->
    { taken, rest } = takeDigits bytes

    when Str.fromUtf8 taken |> Result.try Str.toU32 is
        Ok n -> { result: Ok n, rest }
        Err _ -> { result: Err TooShort, rest }

decodeU64 = Decode.custom \bytes, @Json {} ->
    { taken, rest } = takeDigits bytes

    when Str.fromUtf8 taken |> Result.try Str.toU64 is
        Ok n -> { result: Ok n, rest }
        Err _ -> { result: Err TooShort, rest }

decodeU128 = Decode.custom \bytes, @Json {} ->
    { taken, rest } = takeDigits bytes

    when Str.fromUtf8 taken |> Result.try Str.toU128 is
        Ok n -> { result: Ok n, rest }
        Err _ -> { result: Err TooShort, rest }

decodeI8 = Decode.custom \bytes, @Json {} ->
    { taken, rest } = takeDigits bytes

    when Str.fromUtf8 taken |> Result.try Str.toI8 is
        Ok n -> { result: Ok n, rest }
        Err _ -> { result: Err TooShort, rest }

decodeI16 = Decode.custom \bytes, @Json {} ->
    { taken, rest } = takeDigits bytes

    when Str.fromUtf8 taken |> Result.try Str.toI16 is
        Ok n -> { result: Ok n, rest }
        Err _ -> { result: Err TooShort, rest }

decodeI32 = Decode.custom \bytes, @Json {} ->
    { taken, rest } = takeDigits bytes

    when Str.fromUtf8 taken |> Result.try Str.toI32 is
        Ok n -> { result: Ok n, rest }
        Err _ -> { result: Err TooShort, rest }

decodeI64 = Decode.custom \bytes, @Json {} ->
    { taken, rest } = takeDigits bytes

    when Str.fromUtf8 taken |> Result.try Str.toI64 is
        Ok n -> { result: Ok n, rest }
        Err _ -> { result: Err TooShort, rest }

decodeI128 = Decode.custom \bytes, @Json {} ->
    { taken, rest } = takeDigits bytes

    when Str.fromUtf8 taken |> Result.try Str.toI128 is
        Ok n -> { result: Ok n, rest }
        Err _ -> { result: Err TooShort, rest }

decodeF32 = Decode.custom \bytes, @Json {} ->
    { taken, rest } = takeFloat bytes

    when Str.fromUtf8 taken |> Result.try Str.toF32 is
        Ok n -> { result: Ok n, rest }
        Err _ -> { result: Err TooShort, rest }

decodeF64 = Decode.custom \bytes, @Json {} ->
    { taken, rest } = takeFloat bytes

    when Str.fromUtf8 taken |> Result.try Str.toF64 is
        Ok n -> { result: Ok n, rest }
        Err _ -> { result: Err TooShort, rest }

decodeDec = Decode.custom \bytes, @Json {} ->
    { taken, rest } = takeFloat bytes

    when Str.fromUtf8 taken |> Result.try Str.toDec is
        Ok n -> { result: Ok n, rest }
        Err _ -> { result: Err TooShort, rest }

decodeBool = Decode.custom \bytes, @Json {} ->
    { before: maybeFalse, others: afterFalse } = List.split bytes 5

    # Note: this could be more performant by traversing both branches char-by-char.
    # Doing that would also make `rest` more correct in the erroring case.
    if
        maybeFalse == [asciiByte 'f', asciiByte 'a', asciiByte 'l', asciiByte 's', asciiByte 'e']
    then
        { result: Ok False, rest: afterFalse }
    else
        { before: maybeTrue, others: afterTrue } = List.split bytes 4

        if
            maybeTrue == [asciiByte 't', asciiByte 'r', asciiByte 'u', asciiByte 'e']
        then
            { result: Ok True, rest: afterTrue }
        else
            { result: Err TooShort, rest: bytes }

jsonString : List U8 -> DecodeResult Str
jsonString = \bytes ->
    { before, others: afterStartingQuote } = List.split bytes 1

    if
        before == [asciiByte '"']
    then
        # TODO: handle escape sequences
        { taken: strSequence, rest } = takeWhile afterStartingQuote \n -> n != asciiByte '"'

        when Str.fromUtf8 strSequence is
            Ok s ->
                { others: afterEndingQuote } = List.split rest 1

                { result: Ok s, rest: afterEndingQuote }

            Err _ -> { result: Err TooShort, rest }
    else
        { result: Err TooShort, rest: bytes }

decodeString = Decode.custom \bytes, @Json {} ->
    jsonString bytes

decodeList = \decodeElem -> Decode.custom \bytes, @Json {} ->
        decodeElems = \chunk, accum ->
            when Decode.decodeWith chunk decodeElem (@Json {}) is
                { result, rest } ->
                    when result is
                        Ok val ->
                            # TODO: handle spaces before ','
                            { before: afterElem, others } = List.split rest 1

                            if
                                afterElem == [asciiByte ',']
                            then
                                decodeElems others (List.append accum val)
                            else
                                Done (List.append accum val) rest

                        Err e -> Errored e rest

        { before, others: afterStartingBrace } = List.split bytes 1

        if
            before == [asciiByte '[']
        then
            # TODO: empty lists
            when decodeElems afterStartingBrace [] is
                Errored e rest -> { result: Err e, rest }
                Done vals rest ->
                    { before: maybeEndingBrace, others: afterEndingBrace } = List.split rest 1

                    if
                        maybeEndingBrace == [asciiByte ']']
                    then
                        { result: Ok vals, rest: afterEndingBrace }
                    else
                        { result: Err TooShort, rest }
        else
            { result: Err TooShort, rest: bytes }

parseExactChar : List U8, U8 -> DecodeResult {}
parseExactChar = \bytes, char ->
    when List.get bytes 0 is
        Ok c ->
            if
                c == char
            then
                { result: Ok {}, rest: (List.split bytes 1).others }
            else
                { result: Err TooShort, rest: bytes }

        Err _ -> { result: Err TooShort, rest: bytes }

openBrace : List U8 -> DecodeResult {}
openBrace = \bytes -> parseExactChar bytes (asciiByte '{')

closingBrace : List U8 -> DecodeResult {}
closingBrace = \bytes -> parseExactChar bytes (asciiByte '}')

recordKey : List U8 -> DecodeResult Str
recordKey = \bytes -> jsonString bytes

anything : List U8 -> DecodeResult {}
anything = \bytes -> { result: Err TooShort, rest: bytes }

colon : List U8 -> DecodeResult {}
colon = \bytes -> parseExactChar bytes (asciiByte ':')

comma : List U8 -> DecodeResult {}
comma = \bytes -> parseExactChar bytes (asciiByte ',')

tryDecode : DecodeResult a, ({ val : a, rest : List U8 } -> DecodeResult b) -> DecodeResult b
tryDecode = \{ result, rest }, mapper ->
    when result is
        Ok val -> mapper { val, rest }
        Err e -> { result: Err e, rest }

decodeRecord = \initialState, stepField, finalizer -> Decode.custom \bytes, @Json {} ->
        # NB: the stepper function must be passed explicitly until #2894 is resolved.
        decodeFields = \stepper, state, kvBytes ->
            { val: key, rest } <- recordKey kvBytes |> tryDecode
            { rest: afterColonBytes } <- colon rest |> tryDecode
            { val: newState, rest: beforeCommaOrBreak } <- tryDecode
                    (
                        when stepper state key is
                            Skip ->
                                { rest: beforeCommaOrBreak } <- afterColonBytes |> anything |> tryDecode
                                { result: Ok state, rest: beforeCommaOrBreak }

                            Keep decoder ->
                                Decode.decodeWith afterColonBytes decoder (@Json {})
                    )

            { result: commaResult, rest: nextBytes } = comma beforeCommaOrBreak

            when commaResult is
                Ok {} -> decodeFields stepField newState nextBytes
                Err _ -> { result: Ok newState, rest: nextBytes }

        { rest: afterBraceBytes } <- bytes |> openBrace |> tryDecode

        { val: endStateResult, rest: beforeClosingBraceBytes } <- decodeFields stepField initialState afterBraceBytes |> tryDecode

        { rest: afterRecordBytes } <- beforeClosingBraceBytes |> closingBrace |> tryDecode

        when finalizer endStateResult is
            Ok val -> { result: Ok val, rest: afterRecordBytes }
            Err e -> { result: Err e, rest: afterRecordBytes }
