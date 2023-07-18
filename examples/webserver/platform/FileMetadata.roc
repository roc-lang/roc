interface FileMetadata
    exposes [FileMetadata, bytes, type, isReadonly, mode]
    imports []

# Design note: this is an opaque type rather than a type alias so that
# we can add new operating system info if new OS releases introduce them,
# as a backwards-compatible change.
## An opaque type that represents metadata about a file.
FileMetadata := {
    bytes : U64,
    type : [File, Dir, Symlink],
    isReadonly : Bool,
    mode : [Unix U32, NonUnix],
}

## Returns the number of bytes in the associated file.
bytes : FileMetadata -> U64
bytes = \@FileMetadata info -> info.bytes

## Returns [Bool.true] if the associated file is read-only.
isReadonly : FileMetadata -> Bool
isReadonly = \@FileMetadata info -> info.isReadonly

## Returns the type of the associated file.
type : FileMetadata -> [File, Dir, Symlink]
type = \@FileMetadata info -> info.type

## Returns the mode of the associated file.
mode : FileMetadata -> [Unix U32, NonUnix]
mode = \@FileMetadata info -> info.mode

# TODO need to create a Time module and return something like Time.Utc here.
# lastModified : FileMetadata -> Utc
# TODO need to create a Time module and return something like Time.Utc here.
# lastAccessed : FileMetadata -> Utc
# TODO need to create a Time module and return something like Time.Utc here.
# created : FileMetadata -> Utc
