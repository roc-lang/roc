interface ProcessOutput
    exposes [
        FileDescriptor,
        ProcessOutput,
        pipeTo,
        capture,
        devNull,
    ]
    imports []

FileDescriptor mode := {}

ProcessOutput := [
    Capture,
    PipeTo (FileDescriptor [Write]),
    DevNull,
]

# Pipes the process output to an open file descriptor that can be written to.
pipeTo : FileDescriptor [Write] -> ProcessOutput
pipeTo = \fd -> @ProcessOutput (PipeTo fd)

# The output of a child process should be captured.
capture : ProcessOutput
capture = @ProcessOutput Capture

# The process output should be ignored.
# This is equivalent to piping to the null device, which is `/dev/null` on POSIX or `nul` on Windows.
devNull : ProcessOutput
devNull = @ProcessOutput DevNull
