interface ProcessInput
    exposes [
        FileDescriptor,
        ProcessInput,
        fromStr,
        fromBytes,
        pipeFrom,
        devNull,
    ]
    imports []

FileDescriptor mode := {}

ProcessInput := [
    PipeFrom (FileDescriptor [Read]),
    FromBytes (List U8),
    DevNull,
]

# Provides exactly the contents of the given string as input to the process, then
# closes the process input.
fromStr : Str -> ProcessInput
fromStr = \s -> @ProcessInput (FromBytes (Str.toUtf8 s))

# Provides exactly the contents of the given list of bytes as input to the process,
# then closes the process input.
fromBytes : List U8 -> ProcessInput
fromBytes = \bytes -> @ProcessInput (FromBytes bytes)

# Pipes the given readable file descriptor to the process's standard input.
pipeFrom : FileDescriptor [Read] -> ProcessInput
pipeFrom = \fd -> @ProcessInput (PipeFrom fd)

# The process input should be fed nothing.
# This is equivalent to piping to the null device, which is `/dev/null` on POSIX or `nul` on Windows.
devNull : ProcessInput
devNull = @ProcessInput DevNull
