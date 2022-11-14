interface InternalFile
    exposes [ReadErr, WriteErr]
    imports []

ReadErr : [
    NotFound,
    Interrupted,
    InvalidFilename,
    PermissionDenied,
    TooManySymlinks, # aka FilesystemLoop
    TooManyHardlinks,
    TimedOut,
    StaleNetworkFileHandle,
    OutOfMemory,
    Unsupported,
    Unrecognized I32 Str,
]

WriteErr : [
    NotFound,
    Interrupted,
    InvalidFilename,
    PermissionDenied,
    TooManySymlinks, # aka FilesystemLoop
    TooManyHardlinks,
    TimedOut,
    StaleNetworkFileHandle,
    ReadOnlyFilesystem,
    AlreadyExists, # can this happen here?
    WasADirectory,
    WriteZero, # TODO come up with a better name for this, or roll it into another error tag
    StorageFull,
    FilesystemQuotaExceeded, # can this be combined with StorageFull?
    FileTooLarge,
    ResourceBusy,
    ExecutableFileBusy,
    OutOfMemory,
    Unsupported,
    Unrecognized I32 Str,
]

# DirReadErr : [
#     NotFound,
#     Interrupted,
#     InvalidFilename,
#     PermissionDenied,
#     TooManySymlinks, # aka FilesystemLoop
#     TooManyHardlinks,
#     TimedOut,
#     StaleNetworkFileHandle,
#     NotADirectory,
#     OutOfMemory,
#     Unsupported,
#     Unrecognized I32 Str,
# ]
# RmDirError : [
#     NotFound,
#     Interrupted,
#     InvalidFilename,
#     PermissionDenied,
#     TooManySymlinks, # aka FilesystemLoop
#     TooManyHardlinks,
#     TimedOut,
#     StaleNetworkFileHandle,
#     NotADirectory,
#     ReadOnlyFilesystem,
#     DirectoryNotEmpty,
#     OutOfMemory,
#     Unsupported,
#     Unrecognized I32 Str,
# ]
