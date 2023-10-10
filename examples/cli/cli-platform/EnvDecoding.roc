interface EnvDecoding exposes [EnvFormat, format] imports []

EnvFormat := {} implements [
        DecoderFormatting {
            u8: envU8,
            u16: envU16,
            u32: envU32,
            u64: envU64,
            u128: envU128,
            i8: envI8,
            i16: envI16,
            i32: envI32,
            i64: envI64,
            i128: envI128,
            f32: envF32,
            f64: envF64,
            dec: envDec,
            bool: envBool,
            string: envString,
            list: envList,
            record: envRecord,
            tuple: envTuple,
        },
    ]

format : {} -> EnvFormat
format = \{} -> @EnvFormat {}

decodeBytesToNum = \bytes, transformer ->
    when Str.fromUtf8 bytes is
        Ok s ->
            when transformer s is
                Ok n -> { result: Ok n, rest: [] }
                Err _ -> { result: Err TooShort, rest: bytes }

        Err _ -> { result: Err TooShort, rest: bytes }

envU8 = Decode.custom \bytes, @EnvFormat {} -> decodeBytesToNum bytes Str.toU8
envU16 = Decode.custom \bytes, @EnvFormat {} -> decodeBytesToNum bytes Str.toU16
envU32 = Decode.custom \bytes, @EnvFormat {} -> decodeBytesToNum bytes Str.toU32
envU64 = Decode.custom \bytes, @EnvFormat {} -> decodeBytesToNum bytes Str.toU64
envU128 = Decode.custom \bytes, @EnvFormat {} -> decodeBytesToNum bytes Str.toU128
envI8 = Decode.custom \bytes, @EnvFormat {} -> decodeBytesToNum bytes Str.toI8
envI16 = Decode.custom \bytes, @EnvFormat {} -> decodeBytesToNum bytes Str.toI16
envI32 = Decode.custom \bytes, @EnvFormat {} -> decodeBytesToNum bytes Str.toI32
envI64 = Decode.custom \bytes, @EnvFormat {} -> decodeBytesToNum bytes Str.toI64
envI128 = Decode.custom \bytes, @EnvFormat {} -> decodeBytesToNum bytes Str.toI128
envF32 = Decode.custom \bytes, @EnvFormat {} -> decodeBytesToNum bytes Str.toF32
envF64 = Decode.custom \bytes, @EnvFormat {} -> decodeBytesToNum bytes Str.toF64
envDec = Decode.custom \bytes, @EnvFormat {} -> decodeBytesToNum bytes Str.toDec
envBool = Decode.custom \bytes, @EnvFormat {} ->
    when Str.fromUtf8 bytes is
        Ok "true" -> { result: Ok Bool.true, rest: [] }
        Ok "false" -> { result: Ok Bool.false, rest: [] }
        _ -> { result: Err TooShort, rest: bytes }
envString = Decode.custom \bytes, @EnvFormat {} ->
    when Str.fromUtf8 bytes is
        Ok s -> { result: Ok s, rest: [] }
        Err _ -> { result: Err TooShort, rest: bytes }

envList = \decodeElem -> Decode.custom \bytes, @EnvFormat {} ->
        # Per our supported methods of decoding, this is either a list of strings or
        # a list of numbers; in either case, the list of bytes must be Utf-8
        # decodable. So just parse it as a list of strings and pass each chunk to
        # the element decoder. By construction, our element decoders expect to parse
        # a whole list of bytes anyway.
        decodeElems = \allBytes, accum ->
            { toParse, remainder } =
                when List.splitFirst allBytes (Num.toU8 ',') is
                    Ok { before, after } ->
                        { toParse: before, remainder: Some after }

                    Err NotFound ->
                        { toParse: allBytes, remainder: None }

            when Decode.decodeWith toParse decodeElem (@EnvFormat {}) is
                { result, rest } ->
                    when result is
                        Ok val ->
                            when remainder is
                                Some restBytes -> decodeElems restBytes (List.append accum val)
                                None -> Done (List.append accum val)

                        Err e -> Errored e rest

        when decodeElems bytes [] is
            Errored e rest -> { result: Err e, rest }
            Done vals ->
                { result: Ok vals, rest: [] }

# TODO: we must currently annotate the arrows here so that the lambda sets are
# exercised, and the solver can find an ambient lambda set for the
# specialization.
envRecord : _, (_, _ -> [Keep (Decoder _ _), Skip]), (_ -> _) -> Decoder _ _
envRecord = \_initialState, _stepField, _finalizer -> Decode.custom \bytes, @EnvFormat {} ->
        { result: Err TooShort, rest: bytes }

# TODO: we must currently annotate the arrows here so that the lambda sets are
# exercised, and the solver can find an ambient lambda set for the
# specialization.
envTuple : _, (_, _ -> [Next (Decoder _ _), TooLong]), (_ -> _) -> Decoder _ _
envTuple = \_initialState, _stepElem, _finalizer -> Decode.custom \bytes, @EnvFormat {} ->
        { result: Err TooShort, rest: bytes }
