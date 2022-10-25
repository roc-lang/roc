interface Command
    exposes [
        Command,
        EnvOption,
        WorkingDirOption,
        new,
        arg,
        argBytes,
        argPath,
        env,
        envBytes,
        getArgs,
        getEnv,
        LaunchOptions,
        RunOptions,
        RunError,
        run,
        launch,
    ]
    imports [
        ChildProcess.{ ChildProcess },
        CommandStr.{ CommandStr, BytesError },
        CompletedProcess.{ CompletedProcess },
        Task.{ Task },
        Time.{ Duration },
        Path.{ Path },
        ProcessOutput.{ ProcessOutput },
        ProcessInput.{ ProcessInput },
    ]

Command := {
    path : Path,
    environ : EnvOption,
    stdin : ProcessInput,
    stdout : ProcessOutput,
    stderr : ProcessOutput,
    workingDir : WorkingDirOption,
}

EnvOption : [
    # Inherit the environment of the current (parent) process.
    Inherit,

    # Use an empty environment.
    Empty,

    # Like `Inherit`, but the given list of variable keys will be
    # removed from the environment to start with.
    # Variables in the given list that are not in the environment serve as a no-op.
    InheritWithout (List Str),
]

WorkingDirOption : [Dir Path, NotProvided]

CommandOptions : {
    # The [Path] of the process to run.
    path : Path,

    # How the environment for the command should be initialized.
    # If not provided, defaults to `Inherit`.
    environ ?EnvOption,

    # The method by which standard input should be provided to the child process.
    # If not provided, defaults to the standard input of the current (parent) process.
    stdin ?ProcessInput,

    # The method by which standard output should be consumed from the child process.
    # If not provided, defaults to being captured.
    stdout ?ProcessOutput,

    # The method by which standard error should be consumed from the child process.
    # If not provided, defaults to being captured.
    stderr ?ProcessOutput,

    # The working directory of the command to be run.
    # If not provided, defaults to the current working directory.
    workingDir ?WorkingDirOption,
}

# Creates a new [Command].
new : CommandOptions -> Command
new = \{ path, environ ? Inherit, stdin ? ProcessInput.devNull, stdout ? ProcessOutput.capture, stderr ? ProcessOutput.capture, workingDir ? NotProvided } ->
    @Command {
        path,
        environ,
        stdin,
        stdout,
        stderr,
        workingDir,
    }

# Adds a new string argument to be passed to the [Command].
arg : Command, Str -> Command

# Adds a new argument of arbitrary bytes to be passed to the [Command].
argBytes : Command, List U8 -> Task Command BytesError *

# Adds a new argument, built from a [Path], to be passed to the [Command].
argPath : Command, Path -> Task Command BytesError *

# `env cmd {key, val}` adds the key=val environment-variable mapping to the command.
# If a mapping with "key" was already present, it is overwritten.
env : Command, { key : Str, value : Str } -> Command

# `envBytes cmd {key, val}` adds the key=val environment-variable mapping to the command.
# If a mapping with "key" was already present, it is overwritten.
envBytes : Command, { key : List U8, value : List U8 } -> Task Command BytesError *

# Retrieves the current set of configured arguments for the [Command].
getArgs : Command -> List CommandStr

# Retrieves the current set of configured environment variables for the [Command].
getEnv : Command -> Dict CommandStr CommandStr

# Options for launching a command.
LaunchOptions : {
    # Run the command through the default shell, rather than calling the underlying
    # operating system process execution primitives.
    # Defaults to [Bool.false].
    shell ?Bool,
    # Whether the process should be kept alive if it is not terminated after all references
    # to it in Roc code are dropped.
    #
    # If [Bool.false], undead processes will be [kill][ChildProcess.kill]ed some time
    # after the last reference to the [ChildProcess] is released. The exact time such
    # processes will be killed is unspecified.
    # If [Bool.true], undread processes will not be [kill][ChildProcess.kill]ed until the
    # entire program has exited. The exact time such processes will be killed is unspecified.
    #
    # Defaults to [Bool.false].
    keepAlive ?Bool,
}

# Options for starting a command and running it to completion.
RunOptions : {
    # Run the command through the default shell, rather than calling the underlying
    # operating system process execution primitives.
    # Defaults to `false`.
    shell ?Bool,
    # Kill the command if it is not determined to have terminated within the given timeout.
    # Default to `NoTimeout`.
    timeout ?[Timeout Duration, NoTimeout],
}

# TODO: can we flush this out more granularly?
RunError : [
    FailedToLaunch Str,
    CommandNotFound,
    Other Str,
]

# Runs a `Command` to completion, returning a `CompletedProcess` (see below for its API)
# If the `Command` fails to launch, a `LaunchError` is returned.
run : Command, RunOptions -> Task CompletedProcess RunError [Process [Launch, Stop]*]*

# Launches a `Command`, returning a `ChildProcess` (see below for its API)
# If the `Command` fails to launch, a `LaunchError` is returned.
# The exact mechanism for command launching is unspecified and operating-system-specific.
launch : Command, LaunchOptions -> Result ChildProcess RunError [Process [Launch]*]*
