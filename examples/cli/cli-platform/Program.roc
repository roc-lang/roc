interface Program
    exposes [Program, ExitCode, noArgs, withArgs, quick, withEnv, exitCode, exit]
    imports [Task.{ Task }, InternalProgram.{ InternalProgram }, InternalTask, Effect]

## A [command-line interface](https://en.wikipedia.org/wiki/Command-line_interface) program.
Program : InternalProgram

## An [exit status](https://en.wikipedia.org/wiki/Exit_status) code.
ExitCode := U8

## Converts a [U8] to an [ExitCode].
##
## If you already have a [Task] and want to convert its success type
## from `{}` to [ExitCode], you may find [Program.exit] convenient.
exitCode : U8 -> ExitCode
exitCode = @ExitCode

## Attach an [ExitCode] to a task.
##
##     Stderr.line "I hit an error and couldn't continue."
##     |> Program.exit 1
##
## Note that this does not terminate the current process! By design, this platform does not have
## a [Task] which terminates the current process. Instead, error handling should be consistently
## done through task failures.
##
## To convert a [U8] directly into an [ExitCode], use [Program.exitCode].
exit : Task {} [] fx, U8 -> Task ExitCode [] fx
exit = \task, code ->
    Task.map task \{} -> @ExitCode code

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
noArgs : Task ExitCode [] * -> Program
noArgs = \task ->
    effect =
        InternalTask.toEffect task
        |> Effect.map \Ok (@ExitCode u8) -> u8

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
withArgs : (List Str -> Task ExitCode [] *) -> Program
withArgs = \toTask ->
    effect = Effect.after Effect.args \args ->
        toTask args
        |> InternalTask.toEffect
        |> Effect.map \Ok (@ExitCode u8) -> u8

    InternalProgram.fromEffect effect

## A program which uses [command-line arguments](https://en.wikipedia.org/wiki/Command-line_interface#Arguments)
## and a dictionary of [environment variables](https://en.wikipedia.org/wiki/Environment_variable).
##
## This is a combination of [Program.withArgs] and `Env.dict`. Note that the task's failure type
## must be `[]`. You can satisfy that by handling all the task's potential failures using
## something like [Task.attempt].
##
## If any command-line arguments contain invalid Unicode, the invalid parts will be replaced with
## the [Unicode replacement character](https://unicode.org/glossary/#replacement_character)
## (`�`).
withEnv : (List Str, Dict Str Str -> Task ExitCode [] *) -> Program
withEnv = \toTask ->
    effect =
        args <- Effect.args |> Effect.after
        dict <- Effect.envDict |> Effect.after

        toTask args dict
        |> InternalTask.toEffect
        |> Effect.map \Ok (@ExitCode u8) -> u8

    InternalProgram.fromEffect effect

# ## A combination of [Program.withArgs] and [Env.decodeAll], with the output of [Env.decodeAll]
# ## being passed after the command-line arguments.
# decodedEnv : (List Str, Result env [EnvDecodingFailed Str]* -> Task U8 [] *) -> Program
#     | env has Decode
