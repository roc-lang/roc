interface Program
    exposes [Program, noArgs, withArgs, withNonUnicodeArgs]
    imports [Task.{ Task }, InternalProgram.{ InternalProgram }]

## A [command-line interface](https://en.wikipedia.org/wiki/Command-line_interface) program.
Program : InternalProgram

## A program which runs the given task and discards the values it produces on success or failure.
## One use for this is as an introductory [Program] when teaching someone how to use this platform.
##
## If the task succeeds, the program will exit with a status of 0.
## If the task fails or crashes, the program will exit with a [status](https://en.wikipedia.org/wiki/Exit_status)
## of 1.
##
## For a similar program which specifies its exit status explicitly, see [Program.noArgs].
quick : Task * * * -> Program
quick = InternalProgram.noArgs

## A program which uses no [command-line arguments](https://en.wikipedia.org/wiki/Command-line_interface#Arguments)
## and specifies an [exit status](https://en.wikipedia.org/wiki/Exit_status) [U8].
##
## Note that the task's failure type must be `[]`. You can satisfy that by handling all
## the task's potential failures using something like [Task.attempt].
##
## For a similar program which does use command-line arguments, see [Program.withArgs].
noArgs : Task U8 [] * -> Program
noArgs = InternalProgram.noArgs

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
withArgs = \fromStrArgs ->
    InternalProgram.withNonUnicodeArgs \bytes ->
        bytes
        |> List.map Str.display # TODO implement Str.display
        |> fromStrArgs
        |> InternalTask.toEffect

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

# ## A combination of [Program.withArgs] and [Env.decodeAll], with the output of [Env.decodeAll]
# ## being passed after the command-line arguments.
# decodedEnv : (List Str, Result env [EnvDecodingFailed Str]* -> Task U8 [] *) -> Program
#     | env has Decode