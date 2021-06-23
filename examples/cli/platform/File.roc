interface File
    exposes [ FileReadErr, FileOpenErr, FileWriteErr, DirReadErr, readUtf8, writeUtf8 ]
    imports [ Task.{ Task }, fx.Effect.{ after } ]

## A string that represents a file path.
##
## This is an ordinary [Str], and there is no validation performed on it because
## different operating systems have different rules for what's considered a
## valid file path. (To validate a path, you can pass it to [File.canonicalize]
## and see if it succeeds. This may give different answers on different OSes!)
##
## This type's purpose is to make other type annotations more self-documenting,
## so you can know to expect that a partiuclar string represents a path on the
## file system.
Path : Str

# These various file errors come from the POSIX errno values - see
# http://www.virtsync.com/c-error-codes-include-errno for the actual codes, and
# https://www.gnu.org/software/libc/manual/html_node/Error-Codes.html for documentation
#
# The goal of this design is:
# * Whenever a function returns a `Task`, that task's error type represents all the errors that could happen.
# * The errors are union-friendly; if I run a task that reads, and then another that writes, I should get all the read *and* write errors.
# * To make the errors friendlier to chaining, they should always include the `Path` of the attempted operation. This way it's possible to tell which one failed after the fact.


## These errors can happen when opening a file, before attempting to read from
## it or write to it. The #FileReadErr and #FileWriteErr tag unions begin with
## these tags and then add more specific ones.
FileOpenErr a :
    [
        FileNotFound Path,
        PermissionDenied Path,
        SymLinkLoop Path,
        TooManyOpenFiles Path,
        IoError Path,
        UnknownError I64 Path,
    ]a

## Errors when attempting to read a non-directory file.
FileReadErr a :
    FileOpenErr
        [
            FileWasDir Path,
            InvalidSeek Path,
            IllegalByteSequence Path,
            FileBusy Path,
        ]a

## Errors when attempting to read a directory.
DirReadErr a :
    FileOpenErr
        [
            FileWasNotDir Path,
        ]a

## Errors when attempting to write a non-directory file.
FileWriteErr a :
    FileOpenErr
        [
            FileWasDir Path,
            ReadOnlyFileSystem Path,
        ]a


## Read a file's raw bytes
#readBytes : Path -> Task (List U8) (FileReadErr *)
#readBytes = \path ->
#    Effect.readBytes (Path.toStr path)

## Read a file's bytes and interpret them as UTF-8 encoded text.
readUtf8 : Path -> Task.Task Str (FileReadErr [ BadUtf8 Str.Utf8ByteProblem Nat ]*)
readUtf8 = \path ->
    Effect.map (Effect.readAllUtf8 (Path.toStr path)) \answer ->
        # errno values - see
        # https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/errno.h.html
        when answer.errno is
            0 ->
                when Str.fromUtf8 answer.bytes is
                    Ok str -> Ok str
                    Err (BadUtf8 problem index) -> Err (BadUtf8 problem index)
            1 -> Err (PermissionDenied path)
            2 -> Err (FileNotFound path)
            19 -> Err (FileWasDir path)
            # TODO handle other errno scenarios that could come up
            _ -> Err (UnknownError answer.errno path)

writeUtf8 : Path, Str -> Task.Task {} (FileWriteErr [ BadThing ]*)
writeUtf8 = \path, data ->
    path
        |> Path.toStr
        |> Effect.writeAllUtf8 data
        |> Effect.map \res ->
            when res.errno is
                0 -> Ok {}
                _ -> Err BadThing

## Read a file's bytes, one chunk at a time, and use it to build up a state.
##
## After each chunk is read, it gets passed to a callback which builds up a
## state - optionally while running other tasks.
#readChunks : Path, U64, state, (state, List U8 -> Task state []err) -> Task state (FileReadErr err)

## Like #readChunks except after each chunk you can either `Continue`,
## specifying how many bytes you'd like to read next, or `Stop` early.
#readChunksOrStop : Path, U64, state, (state, List U8 -> [ Continue U64 (Task state []err), Stop (Task state []err) ]) -> Task state (FileReadErr err)
