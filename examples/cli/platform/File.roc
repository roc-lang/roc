interface File
    exposes [ FileReadErr, FileOpenErr, FileWriteErr, DirReadErr, readUtf8, writeUtf8 ]
    imports [ Task.{ Task }, fx.Effect.{ after } ]

# These various file errors come from the POSIX errno values - see
# http://www.virtsync.com/c-error-codes-include-errno for the actual codes, and
# https://www.gnu.org/software/libc/manual/html_node/Error-Codes.html for documentation
#
# The goal of this design is:
# * Whenever a function returns a `Task`, that task's error type represents all the errors that could happen.
# * The errors are union-friendly; if I run a task that reads, and then another that writes, I should get all the read *and* write errors.
# * To make the errors friendlier to chaining, they should always include the path of the attempted operation. This way it's possible to tell which one failed after the fact.


## These errors can happen when opening a file, before attempting to read from
## it or write to it. The #FileReadErr and #FileWriteErr tag unions begin with
## these tags and then add more specific ones.
FileOpenErr a :
    [
        FileNotFound Str,
        PermissionDenied Str,
        SymLinkLoop Str,
        TooManyOpenFiles Str,
        IoError Str,
        ## Operating systems can add new error codes, so it's possible for the
        ## platform to receive one it doesn't know about.
        UnknownError I64 Str,
    ]a

## Errors when attempting to read a non-directory file.
FileReadErr a :
    FileOpenErr
        [
            FileWasDir Str,
            InvalidSeek Str,
            IllegalByteSequence Str,
            FileBusy Str,
        ]a

## Errors when attempting to read a directory.
DirReadErr a :
    FileOpenErr
        [
            FileWasNotDir Str,
        ]a

## Errors when attempting to write a non-directory file.
FileWriteErr a :
    FileOpenErr
        [
            FileWasDir Str,
            ReadOnlyFileSystem Str,
        ]a


## Read all of a file's bytes.
##
## For example, here's how to read a file's bytes and interpret them as a
## UTF-8 string:
##
##     File.readAll "myfile.txt"
##         |> Task.map Str.fromUtf8
readAll : Str -> Task.Task (List U8) (FileReadErr *)
readAll = \path ->
    Effect.map (Effect.readAllUtf8 path) \answer ->
        errno = answer.errno

        if errno == 0 then
            Ok answer.bytes
        else
            Err (errnoFileRead errno path)

## Write all of the given bytes to a file.
##
## For example, here's how to write a string to a file using UTF-8 encoding:
##
##     Str.toUtf8 "contents of the file"
##         |> File.writeAll "myfile.txt"
writeAll : List U8, Str -> Task.Task {} (FileWriteErr *)
writeAll = \data, str ->
    Effect.map (Effect.writeAll path data) \answer ->
        errno = answer.errno

        if errno == 0 then
            Ok answer.bytes
        else
            Err (errnoFileWrite errno path)

## Private helper to convert an `errno` integer to a `FileReadErr`.
##
## For the list of errno values, see:
## https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/errno.h.html
errnoFileRead : I64, Str -> FileReadErr *
errnoFileRead = \errno path ->
    when errno is
        1 -> PermissionDenied path
        2 -> FileNotFound path
        19 -> FileWasDir path
        # TODO handle other errno scenarios that could come up
        _ -> UnknownError errno path

## Private helper to convert an `errno` integer to a `FileWriteErr`.
##
## For the list of errno values, see:
## https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/errno.h.html
errnoFileWrite : I64, Str -> FileWriteErr *
errnoFileWrite = \errno, path ->
    when errno is
        1 -> PermissionDenied path
        2 -> FileNotFound path
        19 -> FileWasDir path
        # TODO handle other errno scenarios that could come up
        _ -> UnknownError errno path


## Read a file's bytes, one chunk at a time, and use it to build up a state.
##
## After each chunk is read, it gets passed to a callback which builds up a
## state - optionally while running other tasks.
#readChunks : Path, U64, state, (state, List U8 -> Task state []err) -> Task state (FileReadErr err)

## Like #readChunks except after each chunk you can either `Continue`,
## specifying how many bytes you'd like to read next, or `Stop` early.
#readChunksOrStop : Path, U64, state, (state, List U8 -> [ Continue U64 (Task state []err), Stop (Task state []err) ]) -> Task state (FileReadErr err)
