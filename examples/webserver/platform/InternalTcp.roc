interface InternalTcp
    exposes [
        Stream,
        ConnectErr,
        StreamErr,
        ConnectResult,
        WriteResult,
        ReadResult,
        ReadExactlyResult,
        fromConnectResult,
        fromWriteResult,
        fromReadResult,
    ]
    imports []

Stream := Nat

ConnectErr : [
    PermissionDenied,
    AddrInUse,
    AddrNotAvailable,
    ConnectionRefused,
    Interrupted,
    TimedOut,
    Unsupported,
    Unrecognized I32 Str,
]

StreamErr : [
    PermissionDenied,
    ConnectionRefused,
    ConnectionReset,
    Interrupted,
    OutOfMemory,
    BrokenPipe,
    Unrecognized I32 Str,
]

# The following custom types will go away when glue supports the builtin `Result`

ConnectResult : [
    Connected Stream,
    Error ConnectErr,
]

fromConnectResult : ConnectResult -> Result Stream ConnectErr
fromConnectResult = \result ->
    when result is
        Connected stream ->
            Ok stream

        Error err ->
            Err err

WriteResult : [
    Wrote,
    Error StreamErr,
]

fromWriteResult : WriteResult -> Result {} StreamErr
fromWriteResult = \result ->
    when result is
        Wrote ->
            Ok {}

        Error err ->
            Err err

ReadResult : [
    Read (List U8),
    Error StreamErr,
]

fromReadResult : ReadResult -> Result (List U8) StreamErr
fromReadResult = \result ->
    when result is
        Read bytes ->
            Ok bytes

        Error err ->
            Err err

ReadExactlyResult : [
    Read (List U8),
    UnexpectedEOF,
    Error StreamErr,
]
