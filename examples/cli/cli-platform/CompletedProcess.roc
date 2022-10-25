interface CompletedProcess
    exposes [
        CompletedProcess,
        ProcessExit,
        stdout,
        stderr,
        processExit,
        wasSuccessful,
    ]
    imports [
        Time.{ Duration },
        ProcessOutput.{ ProcessOutput },
        Task.{ Task },
    ]

CompletedProcess := {
    stdout : ProcessOutput,
    stderr : ProcessOutput,
}

# Returns the standard output of a completed process, or `Err NotCaptured`
# if the process was [not captured][ProcessOutput].
# The standard output need not be valid UTF-8!
stdout : CompletedProcess -> Task (List U8) [NotCaptured] [File [Read]*]*

# Returns the standard error of a completed process, or `Err NotCaptured`
# if the process was [not captured][ProcessOutput].
# The standard error need not be valid UTF-8!
stderr : CompletedProcess -> Task (List U8) [NotCaptured] [File [Read]*]*

# How a completed process exited
ProcessExit : [
    # The process exited with a known exit code.
    Code U8,
    # The process was terminated by a signal.
    # This variant is only possible on UNIX, and never possible on Windows.
    Signal U8,
    # The process was terminated by a timeout.
    # This takes precendence over `Signal` when the host was responsible for terminating
    # the process.
    # Contains the amount of time the process ran for being being terminated.
    TimedOut Duration,
    # The mechanism by which the process exited cannot be determined.
    # This is only relevant on Windows or other non-UNIX operating systems, where there doesn't
    # appear to be a way to determine what signal terminated a process if it did.
    Unknown,
]

# Returns information on why a completed process exited.
processExit : CompletedProcess -> ProcessExit

# Returns Bool.true if and only if the process exited with exit code 0.
wasSuccessful : ProcessExit -> Bool
wasSuccessful = \exit ->
    when exit is
        Code 0 -> Bool.true
        _ -> Bool.false
