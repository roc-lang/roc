interface InternalDir
    exposes [ReadErr, DeleteErr, DirEntry]
    imports [FileMetadata.{ FileMetadata }, Path.{ Path }]

DirEntry : {
    path : Path,
    type : [File, Dir, Symlink],
    metadata : FileMetadata,
}

ReadErr : [
    NotFound,
    Interrupted,
    InvalidFilename,
    PermissionDenied,
    TooManySymlinks, # aka FilesystemLoop
    TooManyHardlinks,
    TimedOut,
    StaleNetworkFileHandle,
    NotADirectory,
    OutOfMemory,
    Unsupported,
    Unrecognized I32 Str,
]

DeleteErr : [
    NotFound,
    Interrupted,
    InvalidFilename,
    PermissionDenied,
    TooManySymlinks, # aka FilesystemLoop
    TooManyHardlinks,
    TimedOut,
    StaleNetworkFileHandle,
    NotADirectory,
    ReadOnlyFilesystem,
    DirectoryNotEmpty,
    OutOfMemory,
    Unsupported,
    Unrecognized I32 Str,
]
