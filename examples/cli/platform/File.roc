interface File
    exposes [ Path, OpenErr, ReadErr, ReadUtf8Err, WriteErr, DirReadErr, readBytes, readUtf8, readUtf8Infallible, writeBytes, writeUtf8 ]
    imports [ fx.Effect, Task.{ Task }, Stdout ]

## TODO move this to an internal module; this should not be exposed in userspace
##
## === THIS MUST BE MANUALLY KEPT IN SYNC WITH THE ONE IN lib.rs ===
ReadErrTag :
    [
        FileBusy,
        FileWasDir,
        IllegalByteSequence,
        InvalidSeek,
    ]

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
## it or write to it. The [ReadErr] and [FileWriteErr] tag unions begin with
## these tags and then add more specific ones.
OpenErr :
    [
        FileNotFound Path,
        PermissionDenied Path,
        SymLinkLoop Path,
        TooManyOpenFiles Path,
        IoError Path,
        UnknownError I32 Path,
    ]

## Errors when attempting to read a non-directory file.
ReadErr :
    [
        FileWasDir Path,
        InvalidSeek Path,
        IllegalByteSequence Path,
        FileBusy Path,
    # TODO FIXME - should be the following, which parses but doesn't type-check:
    #]OpenErr
    ]

ReadUtf8Err : [ BadUtf8 Str.Utf8ByteProblem Nat ]ReadErr

## Errors when attempting to read a directory.
DirReadErr :
    [
        FileWasNotDir Path,
    ]OpenErr

# ## Errors when attempting to write a non-directory file.
WriteErr :
    [
        FileWasDir Path,
        ReadOnlyFileSystem Path,
    # TODO FIXME - should be the following, which parses but doesn't type-check:
    #]OpenErr
    ]

readUtf8Infallible : Path -> Task Str ReadErr
readUtf8Infallible = \path ->
    Task.await (readBytesInfallible path) \bytes ->
        when Str.fromUtf8 bytes is
            Ok str -> Task.succeed str
            # TODO FIXME replace with:  -> Task.fail (ReadUtf8 (BadUtf8 problem index))
            Err (BadUtf8 problem index) -> Task.succeed "<<bad utf8>>"


## Read a file's bytes and interpret them as UTF-8 encoded text.
# TODO FIXME use this instead:
#readUtf8 : Path -> Task Str [ ReadUtf8 ReadUtf8Err ]*
readUtf8 : Path -> Task Str [ ReadUtf8 ReadErr ]*
readUtf8 = \path ->
    result <- Task.attempt (readBytes path)

    when result is
        Ok bytes ->
            when Str.fromUtf8 bytes is
                Ok str -> Task.succeed str
                # TODO FIXME replace with:  -> Task.fail (ReadUtf8 (BadUtf8 problem index))
                Err (BadUtf8 problem index) -> Task.succeed ""

        #Err (ReadBytes problem) -> Task.fail (ReadUtf8 problem)
        Err str -> Task.fail (ReadUtf8 (FileWasDir str))

readBytesInfallible : Path -> Task (List U8) ReadErr
readBytesInfallible = \path ->
    Effect.after (Effect.readAllBytes path) \result ->
        when result is
            Ok bytes -> Task.succeed bytes
            Err err -> Task.succeed []


# readBytes : Path -> Task (List U8) [ ReadBytes ReadErr ]*
# readBytes = \path ->
#     Effect.after (Effect.readAllBytes path) \result ->
#         when result is
#             Ok answer -> Task.succeed answer
#             Err readErr -> Task.fail (ReadBytes readErr)

readBytes : Path -> Task (List U8) Str
readBytes = \path ->
    Effect.after (Effect.readAllBytes path) \result ->
        when result is
            Ok answer -> Task.succeed answer
            Err _ -> Task.fail "something went wrong"

writeUtf8 : Path, Str -> Task {} [ WriteUtf8 WriteErr ]*
writeUtf8 = \path, str ->
    # TODO call Effect.writeAllBytes directly once it returns a sum type instead of errno.
    # That way we don't have to unwrap and re-wrap the output of writeBytes
    writeBytes path (Str.toUtf8 str)
        |> Task.mapFail \WriteBytes problem -> WriteUtf8 problem


writeBytes : Path, List U8 -> Task {} [ WriteBytes WriteErr ]*
writeBytes = \path, data ->
    # TODO replace errno with having Effect.writeBytes output Result {} WriteErr
    Effect.map (Effect.writeAllBytes path data) \errno ->
        when errno is
            0 -> Ok {}
            # 1 -> Err (WriteBytes (PermissionDenied path))
            # 2 -> Err (WriteBytes (FileNotFound path))
            # 19 -> Err (WriteBytes (FileWasDir path))
            # TODO handle other errno scenarios that could come up
            # _ -> Err (WriteBytes (UnknownError errno path))
            _ -> Err (WriteBytes (FileWasDir path)) # TODO FIXME use UnknownError from OpenErr


## Read a file's bytes, one chunk at a time, and use it to build up a state.
##
## After each chunk is read, it gets passed to a callback which builds up a
## state - optionally while running other tasks.
#readChunks : Path, U64, state, (state, List U8 -> Task state []err) -> Task state (FileReadErr err)

## Like #readChunks except after each chunk you can either `Continue`,
## specifying how many bytes you'd like to read next, or `Stop` early.
#readChunksOrStop : Path, U64, state, (state, List U8 -> [ Continue U64 (Task state []err), Stop (Task state []err) ]) -> Task state (FileReadErr err)
