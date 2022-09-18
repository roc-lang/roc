interface Program
    exposes [Program, noArgs, withArgs, withNonUnicodeArgs]
    imports [Task.{ Task }, InternalProgram.{ InternalProgram }]

## A [command-line interface](https://en.wikipedia.org/wiki/Command-line_interface) program.
Program : InternalProgram

## A program which runs the given task and discards the values it produces on success or failure.
## One use for this is as an introductory [Program] when teaching someone how to use this platform.
##
## If the task succeeds, the program will exit with a [status](https://en.wikipedia.org/wiki/Exit_status)
## of 0. If the task fails, the program will exit with a status of 1.
## If the task crashes, the program will exit with a status of 2.
##
## For a similar program which specifies its exit status explicitly, see [Program.noArgs].
quick : Task * * * -> Program
quick = \task ->
    effect =
        InternalTask.toEffect task
        |> Effect.map \result ->
            when result is
                Ok _ -> 0
                Err _ -> 1

    InternalProgram.fromEffect effect

## A program which uses no [command-line arguments](https://en.wikipedia.org/wiki/Command-line_interface#Arguments)
## and specifies an [exit status](https://en.wikipedia.org/wiki/Exit_status) [U8].
##
## Note that the task's failure type must be `[]`. You can satisfy that by handling all
## the task's potential failures using something like [Task.attempt].
##
## For a similar program which does use command-line arguments, see [Program.withArgs].
noArgs : Task U8 [] * -> Program
noArgs = \task ->
    effect =
        InternalTask.toEffect task
        |> Effect.map \result ->
            when result is
                Ok exitStatus -> exitStatus
                Err _ -> 0 # TODO this is unreachable! Remove it after https://github.com/roc-lang/roc/issues/4054 lands

    InternalProgram.fromEffect effect

## A program which uses [command-line arguments](https://en.wikipedia.org/wiki/Command-line_interface#Arguments)
## and specifies an [exit status](https://en.wikipedia.org/wiki/Exit_status) [U8].
##
## Note that the task's failure type must be `[]`. You can satisfy that by handling all
## the task's potential failures using something like [Task.attempt].
##
## If any command-line arguments contain invalid Unicode, the invalid parts will be replaced with
## the [Unicode replacement character](https://unicode.org/glossary/#replacement_character)
## (`�`).
withArgs : (List Str -> Task U8 [] *) -> Program
withArgs = \toTask ->
    effect = Effect.after Effect.args \args ->
        toTask args
        |> InternalTask.toEffect
        |> Effect.map \result ->
            when result is
                Ok exitStatus -> exitStatus
                Err _ -> 0 # TODO this is unreachable! Remove it after https://github.com/roc-lang/roc/issues/4054 lands

    InternalProgram.fromEffect effect

## A program which uses [command-line arguments](https://en.wikipedia.org/wiki/Command-line_interface#Arguments)
## and a dictionary of [environment variables](https://en.wikipedia.org/wiki/Environment_variable).
##
## This is a combination of [Program.withArgs] and [Env.dict]. Note that the task's failure type
## must be `[]`. You can satisfy that by handling all the task's potential failures using
## something like [Task.attempt].
##
## If any command-line arguments contain invalid Unicode, the invalid parts will be replaced with
## the [Unicode replacement character](https://unicode.org/glossary/#replacement_character)
## (`�`).
withEnv : (List Str, Dict Str Str -> Task U8 [] *) -> Program
withEnv = \toTask ->
    effect =
        args <- Effect.args |> Effect.after
        dict <- Effect.envDict |> Effect.after

        toTask args dict
        |> InternalTask.toEffect
        |> Effect.map \result ->
            when result is
                Ok exitStatus -> exitStatus
                Err _ -> 0 # TODO this is unreachable! Remove it after https://github.com/roc-lang/roc/issues/4054 lands

    InternalProgram.fromEffect effect

# ## A combination of [Program.withArgs] and [Env.decodeAll], with the output of [Env.decodeAll]
# ## being passed after the command-line arguments.
# decodedEnv : (List Str, Result env [EnvDecodingFailed Str]* -> Task U8 [] *) -> Program
#     | env has Decode