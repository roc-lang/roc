interface I2 exposes [
    Op,
    Task2,
    succeed,
    fail,
    await,
    stdoutLine,
    stdinLine,
    fileWriteUtf8,
    fileReadUtf8,
    SimTask2,
    SimOp,
    SimFailure,
    simFail,
    simulate,
    simAwait,
    simStdoutLine,
    simStdinLine,
    # simFileWriteUtf8,
    # simFileReadUtf8,
    taskToOp, # only intended to be used by the host; would not be exposed here in real implementation
] imports []

# Operations - states in the state machine

Op : [
    FileWriteUtf8 Str Str (Result {} [NotFound, Malformed] -> Op),
    FileReadUtf8 Str (Result Str [NotFound, Malformed] -> Op),
    StdoutLine Str ({} -> Op),
    StdinLine (Str -> Op),
    None,
]

# Task2 API

Task2 ok err fx := (Result ok err -> Op) -> Op

succeed : ok -> Task2 ok * *
succeed = \ok -> @Task2 \continue -> continue (Ok ok)

fail : err -> Task2 * err *
fail = \err -> @Task2 \continue -> continue (Err err)

await : Task2 a err fx, (a -> Task2 b err fx) -> Task2 b err fx
await = \@Task2 fromResult, fromOk ->
    @Task2 \continue ->
        fromResult \result ->
            @Task2 inner = when result is
                Ok ok -> fromOk ok
                Err err -> fail err

            inner continue

# Internal-only helper for getting an Op to the host,
# so the host doesn't need to deal with Task2
taskToOp : Task2 {} [] * -> Op
taskToOp = \@Task2 fromResult -> fromResult \Ok {} -> None

# Specific tasks (would be exposed by various different modules as normal)

stdoutLine : Str -> Task2 {} * [Write [Stdout]]
stdoutLine = \line ->
    @Task2 \toNext ->
        StdoutLine line \{} -> (toNext (Ok {}))

stdinLine : Task2 Str * [Read [Stdin]]
stdinLine =
    @Task2 \toNext ->
        StdinLine \line -> toNext (Ok line)

fileWriteUtf8 : Str, Str -> Task2 {} [FileWriteErr [NotFound, Malformed]] [Write [File]]
fileWriteUtf8 = \path, contents ->
    @Task2 \toNext ->
        FileWriteUtf8 path contents \result -> toNext (Result.mapErr result FileWriteErr)

fileReadUtf8 : Str -> Task2 Str [FileReadErr [NotFound, Malformed]] [Read [File]]
fileReadUtf8 = \path ->
    @Task2 \toNext ->
        FileReadUtf8 path \result -> toNext (Result.mapErr result FileReadErr)

# Simulation

SimTask2 ok err fx := (Result ok err -> SimOp) -> SimOp

SimOp : [
    FileWriteUtf8 (Str, Str -> Bool) (Result {} [NotFound, Malformed]) (Str, Str -> SimOp),
    FileReadUtf8 (Str -> Bool) (Result Str [NotFound, Malformed]) (Str -> SimOp),
    StdoutLine (Str -> Bool) (Str -> SimOp),
    StdinLine Str SimOp,
    None,
]

SimFailure : [SimFailure] # TODO make real variaants

simFail : err -> SimTask2 * err *
simFail = \err -> @SimTask2 \continue -> continue (Err err)

# TODO accumulate a stack of effects seen so far, and their inputs and outputs, so that we can give a detailed trace on failure
simulateOp : SimOp, Op -> Result {} SimFailure
simulateOp = \simOp, taskOp ->
    when T simOp taskOp is
        T None None -> Ok {}
        T (StdoutLine _validate lineToNext) (StdoutLine line toNext) ->
            simulateOp (lineToNext line) (toNext {})

        T (StdinLine line next) (StdinLine lineToNext) ->
            simulateOp next (lineToNext line)

        T (FileReadUtf8 _validate result pathToNext) (FileReadUtf8 path resultStrToNext) ->
             simulateOp (pathToNext path) (resultStrToNext result)

        T (FileWriteUtf8 _validate result pathAndContentToNext) (FileWriteUtf8 path content resultToNext) ->
             simulateOp (pathAndContentToNext path content) (resultToNext result)

        _ -> Err SimFailure

simulate : SimTask2 * * fx, Task2 {} [] fx -> Result {} SimFailure
simulate = \@SimTask2 simFn, @Task2 taskFn ->
    simulateOp (simFn \_ -> None) (taskFn \Ok {} -> None)

simAwait : SimTask2 a err fx, (a -> SimTask2 b err fx) -> SimTask2 b err fx
simAwait = \@SimTask2 fromResult, fromOk ->
    @SimTask2 \continue ->
        fromResult \result ->
            @SimTask2 inner = when result is
                Ok ok -> fromOk ok
                Err err -> simFail err

            inner continue

simStdoutLine : (Str -> Bool) -> SimTask2 Str * [Write [Stdout]]
simStdoutLine = \validate ->
    @SimTask2 \toNext ->
        StdoutLine validate \line -> toNext (Ok line)

simStdinLine : Str -> SimTask2 {} * [Read [Stdin]]
simStdinLine = \line ->
    @SimTask2 \toNext ->
        StdinLine line (toNext (Ok {}))

# TODO this doesn't type-check, but the error message is basically "expected"
#
# simFileWriteUtf8 :
#     (Result {} [FileWriteErr [NotFound, Malformed]]),
#     (Str, Str -> Bool)
#     -> SimTask2 { path : Str, contents : Str } [FileWriteErr [NotFound, Malformed]] [Write [File]]
# simFileWriteUtf8 = \result, validate ->
#     @SimTask2 \toNext ->
#         # TODO instead of storing Result here, should we incorporate it into the toNext call instead of
#         # always using Ok there? Not sure!
#         FileWriteUtf8 validate (Result.mapErr result FileWriteErr) \path, contents ->
#             toNext (Ok { path, contents })

# simFileReadUtf8 :
#     (Result Str [FileReadErr [NotFound, Malformed]]),
#     (Str -> Bool)
#     -> SimTask2 Str [FileReadErr [NotFound, Malformed]] [Read [File]]
# simFileReadUtf8 = \result, validate ->
#     @SimTask2 \toNext ->
#         # TODO instead of storing Result here, should we incorporate it into the toNext call instead of
#         # always using Ok there? Should SimTask2 even have a Result in this callback? Not sure!
#         FileReadUtf8 validate (Result.mapErr result FileReadErr) \path ->
#             toNext (Ok path)
