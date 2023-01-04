interface Env
    exposes [cwd, dict, var, decode, exePath, setCwd]
    imports [Task.{ Task }, Path.{ Path }, InternalPath, Effect, InternalTask, EnvDecoding]

## Reads the [current working directory](https://en.wikipedia.org/wiki/Working_directory)
## from the environment. File operations on relative [Path]s are relative to this directory.
cwd : Task Path [CwdUnavailable]
cwd =
    effect = Effect.map Effect.cwd \bytes ->
        if List.isEmpty bytes then
            Err CwdUnavailable
        else
            Ok (InternalPath.fromArbitraryBytes bytes)

    InternalTask.fromEffect effect

## Sets the [current working directory](https://en.wikipedia.org/wiki/Working_directory)
## in the environment. After changing it, file operations on relative [Path]s will be relative
## to this directory.
setCwd : Path -> Task {} [InvalidCwd]
setCwd = \path ->
    Effect.setCwd (InternalPath.toBytes path)
    |> Effect.map (\result -> Result.mapErr result \{} -> InvalidCwd)
    |> InternalTask.fromEffect

## Gets the path to the currently-running executable.
exePath : Task Path [ExePathUnavailable]
exePath =
    effect =
        Effect.map Effect.exePath \result ->
            when result is
                Ok bytes -> Ok (InternalPath.fromOsBytes bytes)
                Err {} -> Err ExePathUnavailable

    InternalTask.fromEffect effect

## Reads the given environment variable.
##
## If the value is invalid Unicode, the invalid parts will be replaced with the
## [Unicode replacement character](https://unicode.org/glossary/#replacement_character)
## (`�`).
var : Str -> Task Str [VarNotFound]
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
##     getU16Var : Str -> Task U16 [VarNotFound, DecodeErr DecodeError] [Read [Env]]
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
decode : Str -> Task val [VarNotFound, DecodeErr DecodeError] | val has Decoding
decode = \name ->
    Effect.envVar name
    |> Effect.map
        (
            \result ->
                result
                |> Result.mapErr (\{} -> VarNotFound)
                |> Result.try
                    (\varStr ->
                        Decode.fromBytes (Str.toUtf8 varStr) (EnvDecoding.format {})
                        |> Result.mapErr (\_ -> DecodeErr TooShort)))
    |> InternalTask.fromEffect

## Reads all the process's environment variables into a [Dict].
##
## If any key or value contains invalid Unicode, the [Unicode replacement character](https://unicode.org/glossary/#replacement_character)
## (`�`) will be used in place of any parts of keys or values that are invalid Unicode.
dict : Task (Dict Str Str) *
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
# walk : state, (state, Str, Str -> state) -> Task state [NonUnicodeEnv state] [Read [Env]]
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
# decodeAll : Task val [] [EnvDecodingFailed Str] [Env] | val has Decoding
