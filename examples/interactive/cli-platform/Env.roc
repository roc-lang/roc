interface Env
    exposes [cwd]
    imports [Task.{ Task }, Path.{ Path }, InternalPath, Effect, InternalTask]

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

# ## Reads the given environment variable.
# ##
# ## If the value is invalid Unicode, returns `VarInvalidUnicode` and
# ## a [Str] representing the invalid value with the [Unicode replacement character](https://unicode.org/glossary/#replacement_character)
# ## (`�`) replacing the invalid parts.
# var : Str -> Task Str [VarNotFound, VarInvalidUnicode Str]* [Env]*
# var = \var ->
#     Effect.envVar var
#     |> InternalTask.fromEffect

# ## Reads the given environment variable and attempts to decode it.
# ##
# ## The type being decoded into will be determined by type inference. For example,
# ## if this ends up being used like a `Task U16 …` then the environment variable
# ## will be decoded as a string representation of a `U16`.
# ##
# ##     getU16Var : Str -> Task U16 [VarNotFound, DecodeErr DecodeError]* [Env]*
# ##     getU16Var = \var -> Env.decode var
# ##     # If the environment contains a variable NUM_THINGS=123, then calling
# ##     # (getU16Var "NUM_THINGS") would return a task which succeeds with the U16 number 123.
# ##     #
# ##     # However, if the NUM_THINGS environment variable was set to 1234567, then
# ##     # (getU16Var "NUM_THINGS") would fail because that number is too big to fit in a U16.
# ##
# ## Supported types:
# ## - strings
# ## - numbers, as long as they contain only numeric digits, up to one `.`, and an optional `-` at the front for negative numbers
# ## - comma-separated lists (of either strings or numbers), as long as there are no spaces after the commas
# ##
# ## Trying to decode into any other types will always fail with a `DecodeErr`.
# decode : Str -> Task val [VarNotFound, DecodeErr DecodeError]* [Env]*
#     | val has Decode
# decode = \var ->
#     Effect.envVar var
#     |> InternalTask.fromEffect

# ## Reads all the process's environment variables into a [Dict].
# ##
# ## If any key or value is invalid Unicode, the task fails with `NonUnicodeEnv`.
# ## The `NonUnicodeEnv` tag contains the same [Dict] of environment variables that # ## a success would have produced, except with the [Unicode replacement character](https://unicode.org/glossary/#replacement_character)
# ## (`�`) replacing any parts of keys or values that are invalid Unicode.
# dict : Task (Dict Str Str) [NonUnicodeEnv (Dict Str Str)]* [Env]*
# dict = InternalTask.fromEffect Effect.envDict

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
# ## If any key or value is invalid Unicode, the task fails with `NonUnicodeEnv` and the accumulated
# ## walking state up to the point where the invalid Unicode was encountered.
# walk : state, (state, Str, Str -> state) -> Task state [NonUnicodeEnv state]* [Env]*
# walk = \state, walker ->
#     Effect.envWalk state walker
#     |> InternalTask.fromEffect

# TODO need to figure out clear rules for how to convert from camelCase to SCREAMING_SNAKE_CASE.
# Note that all the env vars decoded in this way become effectively *required* vars, since if any
# of them are missing, decoding will fail. For this reason, it might make sense to use this to
# decode all the required vars only, and then decode the optional ones separately some other way.
# Alternatively, it could make sense to have some sort of tag union convention here, e.g.
# if decoding into a tag union of [Present val, Missing], then it knows what to do.
# decodeAll : Task val [] [EnvDecodingFailed Str]* [Env*] | val has Decoding