interface FileMetadata
    exposes [FileMetadata, bytes, type, isReadonly, mode]
    imports []

# Design note: this is an opaque type rather than a type alias so that
# we can add new operating system info if new OS releases introduce them,
# as a backwards-compatible change.
FileMetadata := {
    bytes : U64,
    type : [File, Dir, Symlink],
    isReadonly : Bool,
    mode : [Unix U32, NonUnix],
}

bytes : FileMetadata -> U64
bytes = \@FileMetadata info -> info.bytes

isReadonly : FileMetadata -> Bool
isReadonly = \@FileMetadata info -> info.isReadonly

type : FileMetadata -> [File, Dir, Symlink]
type = \@FileMetadata info -> info.type

mode : FileMetadata -> [Unix U32, NonUnix]
mode = \@FileMetadata info -> info.mode

# TODO need to create a Time module and return something like Time.Utc here.
# lastModified : FileMetadata -> Utc
# TODO need to create a Time module and return something like Time.Utc here.
# lastAccessed : FileMetadata -> Utc
# TODO need to create a Time module and return something like Time.Utc here.
# created : FileMetadata -> Utc
