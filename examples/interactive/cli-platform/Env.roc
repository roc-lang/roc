interface Env
    exposes [cwd, dict, var, decode]
    imports [Task.{ Task }, Path.{ Path }, InternalPath, Effect, InternalTask,
        Decode.{ Decoder, DecodeError, Decoding, DecoderFormatting }
    ]

## Reads the [current working directory](https://en.wikipedia.org/wiki/Working_directory)
## from the environment. File operations on relative [Path]s are relative to this directory.
cwd : Task Path [CwdUnavailable]* [Env]*
cwd =
    effect = Effect.map Effect.cwd \bytes ->
        if List.isEmpty bytes then
            Err CwdUnavailable
        else
            Ok (InternalPath.fromArbitraryBytes bytes)

    InternalTask.fromEffect effect

# ## Sets the [current working directory](https://en.wikipedia.org/wiki/Working_directory)
# ## in the environment. After changing it, file operations on relative [Path]s will be relative
# ## to this directory.
# setCwd : Path -> Task {} [InvalidCwd]* [Env]*
# setCwd = InternalTask.fromEffect Effect.setCwd
# ## Gets the path to the currently-running executable.
# exePath : Task Path [ExePathUnavailable]* [Env]*
# exePath = InternalTask.fromEffect Effect.setCwd
## Reads the given environment variable.
##
## If the value is invalid Unicode, the invalid parts will be replaced with the
## [Unicode replacement character](https://unicode.org/glossary/#replacement_character)
## (`�`).
var : Str -> Task Str [VarNotFound]* [Env]*
var = \name ->
    Effect.envVar name
    |> Effect.map (\result -> Result.mapErr result \{} -> VarNotFound)
    |> InternalTask.fromEffect

## Reads the given environment variable and attempts to decode it.
##
## The type being decoded into will be determined by type inference. For example,
## if this ends up being used like a `Task U16 …` then the environment variable
## will be decoded as a string representation of a `U16`.
##
##     getU16Var : Str -> Task U16 [VarNotFound, DecodeErr DecodeError]* [Env]*
##     getU16Var = \var -> Env.decode var
##     # If the environment contains a variable NUM_THINGS=123, then calling
##     # (getU16Var "NUM_THINGS") would return a task which succeeds with the U16 number 123.
##     #
##     # However, if the NUM_THINGS environment variable was set to 1234567, then
##     # (getU16Var "NUM_THINGS") would fail because that number is too big to fit in a U16.
##
## Supported types:
## - strings
## - numbers, as long as they contain only numeric digits, up to one `.`, and an optional `-` at the front for negative numbers
## - comma-separated lists (of either strings or numbers), as long as there are no spaces after the commas
##
## Trying to decode into any other types will always fail with a `DecodeErr`.
decode : Str -> Task val [VarNotFound, DecodeErr DecodeError]* [Env]*
         | val has Decoding
decode = \name ->
    Effect.envVar name
    |> Effect.map (
        \result ->
            when result is
                Err {} -> Err VarNotFound
                Ok varStr ->
                    when Decode.fromBytes (Str.toUtf8 varStr) (@EnvFormatting {}) is
                        Ok output -> Ok output
                        Err _ -> Err (DecodeErr TooShort)
    )
    |> InternalTask.fromEffect

EnvFormatting := {} has [DecoderFormatting { 
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
}]

decodeBytesToNum = \bytes, transformer ->
    when Str.fromUtf8 bytes is
        Ok s ->
            when transformer s is
                Ok n -> {result: Ok n, rest: []}
                Err _ -> {result: Err TooShort, rest: bytes}
        Err _ -> {result: Err TooShort, rest: bytes}

envU8 = Decode.custom \bytes, @EnvFormatting {} -> decodeBytesToNum bytes Str.toU8
envU16 = Decode.custom \bytes, @EnvFormatting {} -> decodeBytesToNum bytes Str.toU16
envU32 = Decode.custom \bytes, @EnvFormatting {} -> decodeBytesToNum bytes Str.toU32
envU64 = Decode.custom \bytes, @EnvFormatting {} -> decodeBytesToNum bytes Str.toU64
envU128 = Decode.custom \bytes, @EnvFormatting {} -> decodeBytesToNum bytes Str.toU128
envI8 = Decode.custom \bytes, @EnvFormatting {} -> decodeBytesToNum bytes Str.toI8
envI16 = Decode.custom \bytes, @EnvFormatting {} -> decodeBytesToNum bytes Str.toI16
envI32 = Decode.custom \bytes, @EnvFormatting {} -> decodeBytesToNum bytes Str.toI32
envI64 = Decode.custom \bytes, @EnvFormatting {} -> decodeBytesToNum bytes Str.toI64
envI128 = Decode.custom \bytes, @EnvFormatting {} -> decodeBytesToNum bytes Str.toI128
envF32 = Decode.custom \bytes, @EnvFormatting {} -> decodeBytesToNum bytes Str.toF32
envF64 = Decode.custom \bytes, @EnvFormatting {} -> decodeBytesToNum bytes Str.toF64
envDec = Decode.custom \bytes, @EnvFormatting {} -> decodeBytesToNum bytes Str.toDec
envBool = Decode.custom \bytes, @EnvFormatting {} ->
    when Str.fromUtf8 bytes is
        Ok "true" -> {result: Ok Bool.true, rest: []}
        Ok "false" -> {result: Ok Bool.false, rest: []}
        _ -> {result: Err TooShort, rest: bytes}
envString = Decode.custom \bytes, @EnvFormatting {} ->
    when Str.fromUtf8 bytes is
        Ok s -> {result: Ok s, rest: []}
        Err _ -> {result: Err TooShort, rest: bytes}

envList : Decoder _ _ -> _
envList = \_decodeElem -> Decode.custom \bytes, @EnvFormatting {} ->
        {result: Err TooShort, rest: bytes}

envRecord : _, (_, _ -> [Keep (Decoder _ _), Skip]), (_ -> _) -> Decoder _ _
envRecord = \_initialState, _stepField, _finalizer -> Decode.custom \bytes, @EnvFormatting {} ->
        {result: Err TooShort, rest: bytes}

## Reads all the process's environment variables into a [Dict].
##
## If any key or value contains invalid Unicode, the [Unicode replacement character](https://unicode.org/glossary/#replacement_character)
## (`�`) will be used in place of any parts of keys or values that are invalid Unicode.
dict : Task (Dict Str Str) * [Env]*
dict =
    Effect.envDict
    |> Effect.map Ok
    |> InternalTask.fromEffect

# ## Walks over the process's environment variables as key-value arguments to the walking function.
# ##
# ##     Env.walk "Vars:\n" \state, key, value ->
# ##         "- \(key): \(value)\n"
# ##     # This might produce a string such as:
# ##     #
# ##     #     """
# ##     #     Vars:
# ##     #     - FIRST_VAR: first value
# ##     #     - SECOND_VAR: second value
# ##     #     - THIRD_VAR: third value
# ##     #
# ##     #     """
# ##
# ## If any key or value contains invalid Unicode, the [Unicode replacement character](https://unicode.org/glossary/#replacement_character)
# ## (`�`) will be used in place of any parts of keys or values that are invalid Unicode.
# walk : state, (state, Str, Str -> state) -> Task state [NonUnicodeEnv state]* [Env]*
# walk = \state, walker ->
#     Effect.envWalk state walker
#     |> InternalTask.fromEffect
# TODO could potentially offer something like walkNonUnicode which takes (state, Result Str Str, Result Str Str) so it
# tells you when there's invalid Unicode. This is both faster than (and would give you more accurate info than)
# using regular `walk` and searching for the presence of the replacement character in the resulting
# strings. However, it's unclear whether anyone would use it. What would the use case be? Reporting
# an error that the provided command-line args weren't valid Unicode? Does that still happen these days?
# TODO need to figure out clear rules for how to convert from camelCase to SCREAMING_SNAKE_CASE.
# Note that all the env vars decoded in this way become effectively *required* vars, since if any
# of them are missing, decoding will fail. For this reason, it might make sense to use this to
# decode all the required vars only, and then decode the optional ones separately some other way.
# Alternatively, it could make sense to have some sort of tag union convention here, e.g.
# if decoding into a tag union of [Present val, Missing], then it knows what to do.
# decodeAll : Task val [] [EnvDecodingFailed Str]* [Env*] | val has Decoding
