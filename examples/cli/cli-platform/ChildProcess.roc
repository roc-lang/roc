interface ChildProcess
    exposes [
        ChildProcess,
        WaitOptions,
        ProcessError,
        wait,
        check,
        kill,
        processId,
    ]
    imports [
        Task.{ Task },
        CompletedProcess.{ CompletedProcess },
        Time.{ Duration },
    ]

ChildProcess := {}

WaitOptions : {
    # Kill the command if it is not determined to have terminated within the given timeout.
    # Default to `NoTimeout`.
    timeout ?[Timeout Duration, NoTimeout],
}

# TODO: can this be more granular?
ProcessError a : [
    Other Str,
]a

# Waits for the child process to exit completely.
wait : ChildProcess, WaitOptions -> Task CompletedProcess (ProcessError []) [Process [Poll, Stop]*]*

# Non-blockingly checks whether the child process has exited.
# If it has, the `CompletedProcess` is returned.
# Otherwise, returns `NotCompleted`.
check : ChildProcess -> Task CompletedProcess ProcessError [NotCompleted] [Process [Poll, Stop]*]*

# Instructs the operating system to kill the process immediately.
# Blocks until the process is killed.
kill : ChildProcess -> Task CompletedProcess (ProcessError []) [Process [Poll, Stop]*]*

# Returns an ID for the active process guaranteed to be unique among all actively running processes
# on the current operating system.
# The uniqueness property is only guaranteed while the process is active, and as such this property
# may not be preserved at any moment the `processId` is read, due to TOCTOU races.
#
# As such, the returned ID should never be relied on for performing actions against the `ChildProcess`.
# There is no guarantee that the returned ID continues to be unique after returned, and moreover, it may
# not necessarily be the PID of the process.
# If you need to communicate with the child process, either do so over the [ProcessInput], arguments,
# environment variables, or call [kill].
processId : ChildProcess -> Task Nat ProcessError [Process [Poll]*]*
