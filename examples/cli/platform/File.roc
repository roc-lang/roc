interface File
    exposes [ Path, FileOpenErr, FileReadErr, FileWriteErr, DirReadErr, readBytes, readUtf8, writeBytes, writeUtf8 ]
    imports [ fx.Effect, Task.{ Task } ]

## A file path. This is an alias for an ordinary string; it isn't guaranteed to be
## a valid file path or anything, it's just to make types that deal with file paths
## more self-documenting.
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
## it or write to it. The [FileReadErr] and [FileWriteErr] tag unions begin with
## these tags and then add more specific ones.
FileOpenErr a :
    [
        FileNotFound Path,
        PermissionDenied Path,
        SymLinkLoop Path,
        TooManyOpenFiles Path,
        IoError Path,
        UnknownError I32 Path,
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

## Read a file's bytes and interpret them as UTF-8 encoded text.
readUtf8 : Path -> Task.Task Str (FileReadErr [ BadUtf8 Str.Utf8ByteProblem Nat ]*)
readUtf8 = \path ->
    Task.await (readBytes path) \bytes ->
        when Str.fromUtf8 bytes is
            Ok str -> Task.succeed str
            Err (BadUtf8 problem index) -> Task.fail (BadUtf8 problem index)


readBytes : Path -> Task.Task (List U8) (FileReadErr *)
readBytes = \path ->
    Effect.map (Effect.readAllBytes path) \answer ->
        when answer.error is
            A -> Ok answer.bytes
            err -> Err err


writeUtf8 : Path, Str -> Task.Task {} (FileWriteErr *)
writeUtf8 = \path, str ->
    writeBytes path (Str.toUtf8 str)


writeBytes : Path, List U8 -> Task.Task {} (FileWriteErr *)
writeBytes = \path, data ->
    Effect.map (Effect.writeAllBytes path data) \errno ->
        when errno is
            0 -> Ok {}
            1 -> Err (PermissionDenied path)
            2 -> Err (FileNotFound path)
            19 -> Err (FileWasDir path)
            # TODO handle other errno scenarios that could come up
            _ -> Err (UnknownError errno path)


## Read a file's bytes, one chunk at a time, and use it to build up a state.
##
## After each chunk is read, it gets passed to a callback which builds up a
## state - optionally while running other tasks.
#readChunks : Path, U64, state, (state, List U8 -> Task state []err) -> Task state (FileReadErr err)

## Like #readChunks except after each chunk you can either `Continue`,
## specifying how many bytes you'd like to read next, or `Stop` early.
#readChunksOrStop : Path, U64, state, (state, List U8 -> [ Continue U64 (Task state []err), Stop (Task state []err) ]) -> Task state (FileReadErr err)