interface Env
    exposes [args, argsUtf8, argList, ...]
    imports []

## Returns a list of the program's command-line arguments.
##
## Command-line arguments are not always valid Unicode, so this provides them
## as `List U8`s of raw bytes; you can decode those however you like!
##
## See [argsUtf8] for an alternative which produces [Str] instead of [List U8].
args : Task (List (List U8)) * [Read [Args]*]*

## Returns the program's command-line arguments, decoded as UTF-8 strings.
## If any of the arguments aren't valid UTF-8, the entire task fails with `BadUtf8`.
argsUtf8 :
    Task
        (List Str)
        [BadUtf8 { argIndex : Nat, bytes : List U8 }]*
        [Read [Args]*]*

## Walk through the program's command-line arguments.
##
## Command-line arguments are not always valid Unicode, so this provides them
## as `List U8`s of raw bytes; you can decode those however you like!
##
## See [walkArgsUtf8] for an alternative which produces [Str] instead of [List U8].
walkArgs :
    state,
    (state, List U8 -> Task state err fx)
    -> Task state err [Read [Args]*]fx

walkArgs :
    state,
    (state, List U8 -> state)
    -> Task state * [Read [Args]*]*

# How to print: "Processing arg number {argIndex}..."

main =
    {} <- Stdout.line "Starting..." |> Task.await

    { answer } = Env.walkArgs { index: 0, answer: [] } \state, arg ->
        answer = List.append state.answer arg

        { answer, index: state.index + 1 }

    answerStr = Num.toStr answer

    Stdout.line "Answer: \(answerStr)"

main =
    {} <- Stdout.line "Starting..." |> Task.await

    tasks = Env.walkArgs (Task.succeed { index: 0, answer: [] }) \prev, arg ->
        state <- Task.await prev
        str = Num.toStr (state.index + 1) ##################
        {} <- Stdout.line "Processing arg number \(str)..." |> Task.await
        answer = List.append state.answer arg

        Task.succeed { answer, index: state.index + 1 }

    { answer } <- Task.join tasks |> Task.await
    answerStr = Num.toStr answer ####################

    Stdout.line "Answer: \(answerStr)"

main =
    {} <- Stdout.line "Starting..." |> Task.await

    task = Env.walkArgs { index: 0, answer: [] } \state, arg ->
        str = Num.toStr (state.index + 1)
        {} <- Stdout.line "Processing arg number \(str)..." |> Task.await
        answer = List.append state.answer arg

        Task.succeed { answer, index: state.index + 1 }

    { answer } <- Task.await task
    answerStr = Num.toStr answer

    Stdout.line "Answer: \(answerStr)"


# (state, List U8 -> state)
task : Task MyState MyErr [Read [Args]*]MyFx
task =
    nestedTask : Task (Task MyState MyErr MyFx) * [Read [Args]*]*
    nestedTask = Env.walkArgs (Task.succeed init) \innerTask, arg ->
        state <- Task.await innerTask

        doStuffToState state arg
            |> Task.succeed

    Task.join nestedTask

# (state, List U8 -> Task state err fx)
task : Task MyState MyErr [Read [Args]*]MyFx
task = Env.walkArgs init \state, arg ->
    doStuffToState state arg
        |> Task.succeed

## Walk through the program's command-line arguments until `Done` says to stop.
##
## Command-line arguments are not always valid Unicode, so this provides them
## as `List U8`s of raw bytes; you can decode those however you like!
##
## See [walkArgsUtf8] for an alternative which produces [Str] instead of [List U8].
walkArgsUntil :
    state,
    (state, List U8 -> Task [Done state, Continue state] err fx)
    -> Task state err [Read [Args]*]fx

## Walk through the program's command-line arguments, decoded as UTF-8 strings.
## If any of the arguments aren't valid UTF-8, the entire task fails with `BadUtf8`.
walkArgsUtf8 :
    state,
    (state, Str -> Task state err fx)
    -> Task
        state
        [BadUtf8 { argIndex : Nat, bytes : List U8 }]err
        [Read [Args]*]fx

walkArgsUtf8Until :
    state,
    (state, Str -> Task [Done state, Continue state] err fx)
    -> Task
        state
        [BadUtf8 { argIndex : Nat, bytes : List U8 }]err
        [Read [Args]*]fx

## Environment variable entries may not be valid Unicode. If this environment variable
## is not valid unicode, the task fails with `InvalidUnicodeVar` and the raw bytes
## that were not valid unicode.
var :
    Str
    -> Task
        Str
        [VarNotFound Str, NonUnicodeVar (List U8)]*
        [Read [Env]*]*

## Environment variable keys and values may not be valid Unicode. This returns the raw bytes
## associated with the given [Str] key.
##
## To find an environment variable whose key is not a valid [Str], use [walkVarsUntil].
varBytes :
    Str
    -> Task
        (List U8)
        [VarNotFound Str]*
        [Read [Env]*]*

walkVars :
    state,
    (state, (List U8, List U8) -> Task state err fx)
    -> Task state err [Read [Env]*]fx

walkVarsUntil :
    state,
    (state, (List U8, List U8) -> Task [Done state, Continue state] err fx)
    -> Task state err [Read [Env]*]fx

## Get the system's current [Locale].
##
## The first time this task runs, it reads the locale from the environment and
## translates it into a [Locale]. However, since the operating system sets the system's
## locale when the process begins, and does not change it afterwards (changing the system-wide
## locale typically requires a reboot to fully take effect), calling this multiple
## times results in a cached answer being provided after the first time.
##
## This means the first time this task runs, it will do some work to look up the
## environment setting, and create the [Locale] structure from that. Every time you call it after
## the first will be much faster, as it will return the cached value instead of redoing that work.
locale : Task Locale * [Read [Env]*]*
# Windows: https://docs.microsoft.com/en-us/windows/win32/api/winnls/nf-winnls-getthreadlocale
# UNIX: setlocale [sic] https://www.man7.org/linux/man-pages/man3/setlocale.3.html
#       yes, really - to *get* the locale, you call setlocale passing NULL

## Get the system's current [Charset].
##
## This is the same as calling [locale] and passing that [Locale] to [Locale.charset].
charset : Task Charset * [Read [Env]*]*

# Note: setLocale seems like a mistake, because on Windows this can only be done on
# a per-thread basis, not a per-process basis.
# https://docs.microsoft.com/en-us/windows/win32/api/winnls/nf-winnls-setthreadlocale

# TODO: should we allow Env.set? I don't want to. Seems like it could mess up locale,
# for example. We could disallow setting certain ones, but that seems like a mistake.
# Also, env vars are literally global mutable variables, so...why are you setting them?
# There's always a workaround involving passing values around!
# Disallowing setEnv guarantees (assuming the host cooperates) that locale won't change.




- super low-level file I/O
- streaming I/O


1. download zip from S3
1. extract it
1. run headless libreoffice on it


--------- 10G CSV file ------------
1. get something from network [4K chunks]
1. decompress incrementally while downloading
1. parse incrementally while downloading

decompressAndParseDecoder =
    # decoder that runs first decompress from List U8 into List U8, then decode that into CSV

ReadStream.openHttps urlInfo decompressAndParseDecoder
    |> ReadStream.readChunks 4096 [] \entries, entry -> List.append entries entry

ReadStream.openHttps urlInfo decompressAndParseDecoder
    |> ReadStream.readChunks 4096 (Task.succeed []) \prev, entry ->
        { entries, totalBytes, bytesProcessed } <- Task.await prev

        {} <- Stdout.line "\(percentage)..." |> Task.await

        Task.succeed (List.append entries entry)

RocBin

#    -> Task (List CsvEntry) (ReadErr [DecodeErr Decode.Err]*) fx

readChunks 4 JSON.fromUtf8

1234

👍 decoding succeeded, answer is 1234, AND I could keep going, I only stopped due to eof

56.78

👍 decoding succeeded, answer is 56.78, AND I could keep going, I only stopped due to eof

"ab"

👍 decoding succeeded, answer is "ab", and I could NOT keep going, there better not be more.

"a"

👍 decoding succeeded, answer is "a", and found leftovers of "\n"

--------- csv ----------
👍 decoding succeeded, answer is [1, 2, 3], and found leftovers with unclosed quotes

DecodingResult ok err : [
    OkAndCouldKeepGoing ok
]

123456.78












