interface Command
    exposes [
        Command,
        Output,
        new,
        arg,
        args,
        env,
        envs,
        clearEnvs,
        status,
        output,
    ]
    imports [
        Task.{ Task },
        InternalTask,
        InternalCommand,
        Effect,
    ]

## Represents a command to be executed in a child process.
Command := InternalCommand.Command

## Errors from executing a command.
Error : InternalCommand.CommandErr

## Represents the output of a command.
Output : InternalCommand.Output

## Create a new command to execute the given program in a child process.
new : Str -> Command
new = \program ->
    @Command {
        program,
        args: [],
        envs: [],
        clearEnvs: Bool.false,
    }

## Add a single argument to the command.
##
## ```
## # Represent the command "ls -l"
## Command.new "ls"
## |> Command.arg "-l"
## ```
##
arg : Command, Str -> Command
arg = \@Command cmd, value ->
    @Command
        { cmd &
            args: List.append cmd.args value,
        }

## Add multiple arguments to the command.
##
## ```
## # Represent the command "ls -l -a"
## Command.new "ls"
## |> Command.args ["-l", "-a"]
## ```
##
args : Command, List Str -> Command
args = \@Command cmd, values ->
    @Command
        { cmd &
            args: List.concat cmd.args values,
        }

## Add a single environment variable to the command.
##
## ```
## # Run "env" and add the environment variable "FOO" with value "BAR"
## Command.new "env"
## |> Command.env "FOO" "BAR"
## ```
##
env : Command, Str, Str -> Command
env = \@Command cmd, key, value ->
    @Command
        { cmd &
            envs: List.concat cmd.envs [key, value],
        }

## Add multiple environment variables to the command.
##
## ```
## # Run "env" and add the variables "FOO" and "BAZ"
## Command.new "env"
## |> Command.envs [("FOO", "BAR"), ("BAZ", "DUCK")]
## ```
##
envs : Command, List (Str, Str) -> Command
envs = \@Command cmd, keyValues ->
    values = keyValues |> List.joinMap \(key, value) -> [key, value]
    @Command
        { cmd &
            envs: List.concat cmd.envs values,
        }

## Clear all environment variables, and prevent inheriting from parent, only
## the environment variables provided to command are available to the child.
##
## ```
## # Represents "env" with only "FOO" environment variable set
## Command.new "env"
## |> Command.clearEnvs
## |> Command.env "FOO" "BAR"
## ```
##
clearEnvs : Command -> Command
clearEnvs = \@Command cmd ->
    @Command { cmd & clearEnvs: Bool.true }

## Execute command and capture stdout and stderr
##
## > Stdin is not inherited from the parent and any attempt by the child process
## > to read from the stdin stream will result in the stream immediately closing.
##
output : Command -> Task Output *
output = \@Command cmd ->
    Effect.commandOutput (Box.box cmd)
    |> Effect.map Ok
    |> InternalTask.fromEffect

## Execute command and inheriting stdin, stdout and stderr from parent
##
status : Command -> Task {} Error
status = \@Command cmd ->
    Effect.commandStatus (Box.box cmd)
    |> InternalTask.fromEffect
