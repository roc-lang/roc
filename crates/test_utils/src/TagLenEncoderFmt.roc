# a simple encoder/decoder format
#
# HACK: since this file is inlined into test code, it can't be a proper module
#
# the original author found it useful to leave this header here as a comment, to
# make it easy to switch back and forth between editing it as a proper roc module
# (so things like language server and `roc test` work), and inlining it into test
# code.
#
# module [
#     TagLenFmt,
#     tagLenFmt,
# ]

TagLenFmt := {}
    implements [
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
            tuple: encodeTuple,
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
            tuple: decodeTuple,
        },
    ]

tagLenFmt = @TagLenFmt {}

# ENCODE

appendPreLen = \bytes, pre, len ->
    List.append bytes (Num.toU8 pre)
    |> List.concat (Num.toStr len |> Str.toUtf8)
    |> List.append ' '
encodeNum = \n -> Encode.custom \bytes, @TagLenFmt {} -> appendPreLen bytes 'n' n

encodeU8 = encodeNum
encodeU16 = encodeNum
encodeU32 = encodeNum
encodeU64 = encodeNum
encodeU128 = encodeNum
encodeI8 = encodeNum
encodeI16 = encodeNum
encodeI32 = encodeNum
encodeI64 = encodeNum
encodeI128 = encodeNum
encodeF32 = encodeNum
encodeF64 = encodeNum
encodeDec = encodeNum
encodeBool = \b -> encodeU8 (if b then 1 else 0)

expect
    actual = Encode.toBytes 1 tagLenFmt
    actual == (Str.toUtf8 "n1 ")
expect
    actual = Encode.toBytes 1.3dec tagLenFmt
    actual == (Str.toUtf8 "n1.3 ")
expect
    actual = Encode.toBytes Bool.true tagLenFmt
    actual == (Str.toUtf8 "n1 ")

encodeString = \str -> Encode.custom \bytes, @TagLenFmt {} ->
        appendPreLen bytes 's' (Str.countUtf8Bytes str)
        |> List.concat (Str.toUtf8 str)
        |> List.append ' '

expect
    actual = Encode.toBytes "hey" tagLenFmt
    actual == (Str.toUtf8 "s3 hey ")

encodeList = \lst, encodeElem -> Encode.custom \bytes, @TagLenFmt {} ->
        bytesPre = appendPreLen bytes 'l' (List.len lst)
        List.walk lst bytesPre \buf, elem ->
            Encode.appendWith buf (encodeElem elem) (@TagLenFmt {})

expect
    actual = Encode.toBytes [1, 2, 3] tagLenFmt
    actual == (Str.toUtf8 "l3 n1 n2 n3 ")

encodeRecord = \fields -> Encode.custom \bytes, @TagLenFmt {} ->
        bytesPre =
            appendPreLen bytes 'r' (List.len fields)
        List.walk fields bytesPre \buf, { key, value } ->
            Encode.appendWith buf (encodeString key) (@TagLenFmt {})
            |> Encode.appendWith value (@TagLenFmt {})

expect
    actual = Encode.toBytes { foo: "foo", bar: Bool.true } tagLenFmt
    actual == Str.toUtf8 "r2 s3 bar n1 s3 foo s3 foo "

encodeTuple = \elems -> encodeList elems (\e -> e)
encodeTag = \name, payload -> encodeTuple (List.prepend payload (encodeString name))

expect
    actual = Encode.toBytes (1, "foo", {}) tagLenFmt
    actual == (Str.toUtf8 "l3 n1 s3 foo r0 ")

# DECODE

splitAtSpace = \bytes ->
    when List.splitFirst bytes ' ' is
        Ok { before, after } -> { taken: before, rest: after }
        Err _ -> { taken: [], rest: bytes }

decodeNumPre = \bytes, pre, toNum ->
    when List.splitAt bytes 1 is
        { before: [b], others } if b == pre ->
            { taken, rest } = splitAtSpace others
            str = taken |> Str.fromUtf8 |> Result.mapErr \_ -> TooShort
            result = Result.try str \s -> (toNum s |> Result.mapErr \_ -> TooShort)
            when result is
                Ok _ -> { result, rest }
                Err _ -> { result, rest: others }

        _ -> { result: Err TooShort, rest: bytes }

decodeNum = \toNum -> Decode.custom \bytes, @TagLenFmt {} -> decodeNumPre bytes 'n' toNum

decodeU8 = decodeNum Str.toU8
decodeU16 = decodeNum Str.toU16
decodeU32 = decodeNum Str.toU32
decodeU64 = decodeNum Str.toU64
decodeU128 = decodeNum Str.toU128
decodeI8 = decodeNum Str.toI8
decodeI16 = decodeNum Str.toI16
decodeI32 = decodeNum Str.toI32
decodeI64 = decodeNum Str.toI64
decodeI128 = decodeNum Str.toI128
decodeF32 = decodeNum Str.toF32
decodeF64 = decodeNum Str.toF64
decodeDec = decodeNum Str.toDec
decodeBool = Decode.custom \bytes, @TagLenFmt {} ->
    { result: numResult, rest } = Decode.decodeWith bytes decodeU8 (@TagLenFmt {})
    when numResult is
        Ok 1 -> { result: Ok Bool.true, rest }
        Ok 0 -> { result: Ok Bool.false, rest }
        _ -> { result: Err TooShort, rest: bytes }

expect
    actual = Decode.fromBytes (Str.toUtf8 "n1 ") tagLenFmt
    actual == Ok (Num.toU8 1)
expect
    actual = Decode.fromBytes (Str.toUtf8 "n1 ") tagLenFmt
    actual == Ok Bool.true

decodeLenPre = \bytes, pre -> decodeNumPre bytes pre Str.toU64

decodeTry = \{ result, rest }, map ->
    when result is
        Ok a -> map a rest
        Err e -> { result: Err e, rest }

decodeString = Decode.custom \bytes, @TagLenFmt {} ->
    decodeLenPre bytes 's'
    |> decodeTry \len, lenRest ->
        { before, others } = List.splitAt lenRest len
        result = Str.fromUtf8 before |> Result.mapErr \_ -> TooShort
        when List.splitAt others 1 is
            { before: [' '], others: rest } -> { result, rest }
            _ -> { result: Err TooShort, rest: others }

expect
    actual = Decode.fromBytes (Str.toUtf8 "s3 foo ") tagLenFmt
    actual == Ok "foo"

repeatDecode : U8, List U8, state, (state -> Decode.Decoder state TagLenFmt) -> DecodeResult state
repeatDecode = \pre, bytes, state, stepState ->
    run = \end, bs ->
        List.range { start: At 0, end: Before end }
        |> List.walk { result: Ok state, rest: bs } \res, _i ->
            decodeTry res \s, rest ->
                Decode.decodeWith rest (stepState s) (@TagLenFmt {})

    decodeLenPre bytes pre |> decodeTry run

decodeList = \elemDecoder -> Decode.custom \bytes, @TagLenFmt {} ->
        step = \lst -> Decode.custom \sbytes, @TagLenFmt {} ->
                Decode.decodeWith sbytes elemDecoder (@TagLenFmt {})
                |> Decode.mapResult \elem -> List.append lst elem
        repeatDecode 'l' bytes [] step

expect
    actual = Decode.fromBytes (Str.toUtf8 "l3 n1 n2 n3 ") tagLenFmt
    actual == Ok [1, 2, 3]

decodeRecord = \initState, stepField, finalizer -> Decode.custom \bytes, @TagLenFmt {} ->
        flattenFieldRes = \next, rest ->
            when next is
                Keep valueDecoder -> { result: Ok valueDecoder, rest }
                Skip -> { result: Err TooShort, rest }

        step = \state -> Decode.custom \sbytes, @TagLenFmt {} ->
                Decode.decodeWith sbytes decodeString (@TagLenFmt {})
                |> decodeTry \key, bs ->
                    flattenFieldRes (stepField state key) bs
                |> decodeTry \valueDecoder, bs ->
                    Decode.decodeWith bs valueDecoder (@TagLenFmt {})

        repeatDecode 'r' bytes initState step
        |> decodeTry \state, rest -> { result: finalizer state (@TagLenFmt {}), rest }

expect
    actual = Decode.fromBytes (Str.toUtf8 "r2 s3 bar n1 s3 foo s3 foo ") tagLenFmt
    actual == Ok ({ foo: "foo", bar: Bool.true })

decodeTuple = \initialState, stepElem, finalizer -> Decode.custom \bytes, @TagLenFmt {} ->
        flattenFieldRes = \next, rest ->
            when next is
                Next dec -> { result: Ok dec, rest }
                TooLong -> { result: Err TooShort, rest }
        step = \{ state, i } -> Decode.custom \sbytes, @TagLenFmt {} ->
                flattenFieldRes (stepElem state i) sbytes
                |> decodeTry \dec, rest -> Decode.decodeWith rest dec (@TagLenFmt {})
                |> Decode.mapResult \s -> { state: s, i: i + 1 }

        repeatDecode 'l' bytes { state: initialState, i: 0 } step
        |> decodeTry \s, rest -> { result: finalizer s.state, rest }

expect
    actual = Decode.fromBytes (Str.toUtf8 "l3 n1 s3 abc l1 n0 ") tagLenFmt
    actual == Ok (1, "abc", [Bool.false])

expect
    input = { foo: (1, "abc", [Bool.false, Bool.true]), bar: { baz: 0.32 } }
    encoded = Encode.toBytes input tagLenFmt
    decoded = Decode.fromBytes encoded tagLenFmt
    decoded == Ok input
