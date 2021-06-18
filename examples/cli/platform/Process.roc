interface Process
    exposes [
        env,
        walkEnv,
        walkEnvUntil,
        getEnv,
        setEnv,
        getCwd,
        setCwd,
        exitSuccess,
        exitFailure,
        exitWithStatus,
    ]
    imports [ fx.Effect ]

## Process ID
Pid : Pid.Internal.Pid

## Read the process's current environment variables into a [Dict].
##
## For a more flexible way to traverse the process's environment variables,
## see [walkEnv].
env : Task (Dict Str Str) *
env =
    walkEnv Dict.empty Dict.insert

## Read the process's current environment variables, then walk over them.
walkEnv : state, (state, key, value -> state) -> Task state *
walkEnv =
    Effect.walkEnvVars

## Read the process's current environment variables, then walk over them until
## either they run out or the callback function returns `Done`.
walkEnvUntil : state, (state, key, value -> [ Continue state, Done state ]) -> Task state *
walkEnvUntil =
    Effect.walkEnvVarsUntil

## Read the current value of a particular environment variable in the process.
##
## Performance note: This traverses the entire list of environment variables,
## in order, until it finds a match or runs out. If you already have a [Dict]
## like the one returned by [Process.env], looking up the entry in that will
## typically be much faster than running this repeatedly!
getEnv : Str -> Task Str [ VarNotFound ]*
getEnv = \name ->
    Effect.envVar name

## Change the value of an environment variable in the currently-running process.
setEnv : Str, Str -> Task {} [ BadVarName Str, BadVarVal Str ]*
setEnv = \name, val ->
    Effect.setEnvVar name val

## Get the path of the currently-running executable.
getExePath : Task Str Io.Err
getExePath =
    Effect.getExePath
        |> Effect.map Ok

## Get the process's [current working directory](https://en.wikipedia.org/wiki/Working_directory)
## (CWD).
getCwd : Task Str Io.Err
getCwd = \path ->
    Effect.setCwd path

## Change the process's [current working directory](https://en.wikipedia.org/wiki/Working_directory)
## (CWD).
setCwd : Path -> Task {} *
setCwd = \path ->
    Effect.setCwd path

## Exit the process with a status code indicating it succeeded.
##
## (This will automatically use an appropriate status code for success depending
## on the sytem. Some systems use different exit codes to indicate success
## and failure!)
exitSuccess : Task {} *
exitSucecss =
    Effect.exitSuccess

## Exit the process with a status code indicating it failed.
##
## This will automatically use an appropriate status code for failure depending
## on the system. Some systems use different exit codes to indicate success
## and failure!
##
## To specify a particular exit status code, see [exitWithStatus].
exitFailure : Task {} *
exitFailure =
    Effect.exitFailure

## Exit the process with the given status code.
##
## When using this, be careful not to overlap with the operating system's
## exit codes for success and failure accidentally. On most popular operating
## systems, 0 is the exit code for success and 1 is the normal exit code for
## failure. However, this is not universal! OpenVMS, for example, [uses 1
## as the exit code for success](https://www.ibm.com/docs/en/ds8800?topic=system-openvms-messages-exit-codes).
##
## The [exitSuccess] and [exitFailure] functions are designed to provide the exit
## codes the operating system expects for those outcomes, so it's best to use
## those instead of this function unless you need a very specific exit code.
##
## **Note**: In UNIX and other POSIX systems, sometimes only [the least significant 8 bits](https://en.wikipedia.org/wiki/Exit_status#POSIX) of the Failure; it's least risky to provide values that will
## fit in a [U8] here, even though the operating systems suppport [I32].
## (As an example of this caution, [Bash script exit codes](https://tldp.org/LDP/abs/html/exitcodes.html)
## must always be [U8] values.)
exitWithStatus : I32 -> Task {} *
exitWithStatus = \code ->
    Effect.exitWithStatus code

pid : Task Pid *
pid =
    Effect.getPid
        |> Effect.map \raw -> Ok (Internal.Pid.fromRaw raw)

# Notes:
#
# It seems difficult (impossible?) in the general case to
# reliably provide a "gracefully terminate process" function that
# works on Windows. Windows seems to require knowing whether
# a process is graphical or console.
#
# https://stackoverflow.com/questions/2055753/how-to-gracefully-terminate-a-process
#
# It seems possible to find that out on a per-thread basis,
# but not a per-process basis:
# https://stackoverflow.com/a/31356431
#
# ...so what if we don't have access to get the main
# thread of another process that we'd otherwise be able
# to terminate?
#
# Maybe this is why Rust only supports process::kill and
# no cross-platform graceful version!

## This is a [`SIGKILL`](https://en.wikipedia.org/wiki/Signal_(IPC)#SIGKILL) signal on UNIX systems and a [`TerminateProcess`](https://docs.microsoft.com/en-us/windows/win32/api/processthreadsapi/nf-processthreadsapi-terminateprocess) on Windows.
##
## Note that on Windows, this can cause the application to
## stop responding if a thread was waiting on a kernel object at the tim,.
## [The official documentation](https://docs.microsoft.com/en-us/windows/win32/procthread/terminating-a-process#how-processes-are-terminated)
## recommends only terminating a Windows process when all of
## its threads are in known states.
kill : Pid -> Task {} Io.Err
kill = \pid ->
    Effect.map (Effect.kill (Pid.Internal.toRaw pid)) Ok
