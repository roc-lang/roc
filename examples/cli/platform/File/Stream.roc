## A stream of bytes from a file.
interface File.Stream
    exposes [ Stream ]
    imports [ (File.Internal as Internal).{ Fd, toRaw, fromRaw }, Task.{ Task } ]

# How Linux file status flags are handled by this API:
# https://www.man7.org/linux/man-pages/man2/open.2.html
#
# * O_DIRECTORY - [ TODO idea: should we have separate openDir and openFile? Need to look into how to do directories! ]
# * O_APPEND - see `openAppend` and `append`
# * O_ASYNC, O_NONBLOCK, O_NDELAY - these are for host authors only.
# * O_CREAT - the `createIfNotExists` option in OpenConfig
# * O_TRUNC - the `truncate` option in OpenConfig
# * O_NOFOLLOW - the `fileCanBeSymlink` option in OpenConfig
# * O_NOCTTY - n/a - doesn't apply to files, only to terminal devices
# * O_PATH - Linux-only; intentionally unsupported, due to lack of known compelling use cases
# * O_LARGEFILE - intentionally unsupported; hosts should set this based on target - TODO is there a macOS / Windows equivalent?
# * O_CLOEXEC - intentionally unsupported; hosts should always set this, so fork-exec'd processes don't leak file descriptors - see http://tzimmermann.org/2017/08/17/file-descriptors-during-fork-and-exec/ and https://github.com/nodejs/node-v0.x-archive/issues/6905#issuecomment-32679517 - TODO is there an equivalent on macOS and Windows?
# * O_SYNC - Linux-only; not supported for now, but might be in the future (need to understand use cases better)
# * O_DIRECT - Linux-only; not supported for now, but might be in the future (need to understand use cases better)
# * O_DSYNC - Linux-only; not supported for now, but might be in the future (need to understand use cases better)
# * O_NOATIME - Linux-only; not supported for now, but might be in the future if it can be done nicely cross-platform
# * O_EXCL - intentionally unsupported because all it does is introduce potential EEXIST errors, and adds nothing over just not specifying O_CREAT in the first place and letting the open fail on ENOENT if the file doesn't exist:
#
#        exists? | O_CREAT | O_EXCL | outcome
#        ------------------------------------
#        yes     | yes     | yes    | EEXIST
#        yes     | yes     | no     | nothing
#        yes     | no      | yes    | nothing
#        yes     | no      | no     | nothing
#        no      | yes     | yes    | created
#        no      | yes     | no     | created
#        no      | no      | yes    | ENOENT
#        no      | no      | no     | ENOENT
#
# How macOS file status flags are handled by this API:
# https://developer.apple.com/library/archive/documentation/System/Conceptual/ManPages_iPhoneOS/man2/open.2.html
#
# * O_NONBLOCK - same as Linux
# * O_APPEND - same as Linux
# * O_CREAT - same as Linux
# * O_TRUNC - same as Linux
# * O_EXCL - same as Linux
# * O_NOFOLLOW - same as Linux
# * O_SHLOCK, O_EXLOCK - unsupported because they're BSD-only and atomicity for "open and lock" doesn't seem crucial
# * O_SYMLINK - unsupported because it's BSD-only and doesn't seem critical
#
# Error handling notes
#
# We're not going to include error tags for EINTR (interrupted by signal)
# because the CLI platform doesn't specify a way to recover from a signal and
# continue running the Roc code anyway, so any code that would pattern match on
# an EINTR tag would never get successfully executed anyway.


## On UNIX systems, this refers to a file descriptor.
## On Windows, it refers to a file handle.
Stream a : Internal.Stream a

Permissions :
    {
        read : Bool,
        write : Bool,
        execute : Bool,
    }

## An error that can occur when opening a file stream.
OpenErr :
    [
        # TODO ENOENT
    ]OpenPathErr

## An error that can occur when opening a file stream for writing.
OpenWriteErr :
    [
        # TODO EISDIR - can we cause this to happen when opening for reading too?
        # TODO EROFS
        # TODO ETXTBSY
    ]OpenErr

## An error that can occur when creating a file.
CreateErr :
    [
        # TODO EDQUOT
        # TODO EEXIST
        # TODO EIO
        # TODO ENOSPC
    ]OpenPathErr

## An error that can occur when opening a file path, for example during an
## *open*, *create*, or *trunc* operation.
OpenPathErr :
    [
        ## The operating system's per-process limit on how many file descriptors
        ## or handles has been reached, so no more file streams can be created
        ## until at least one other is closed (or the OS limit is increased).
        ##
        ## This corresponds to the `EMFILE` error on UNIX systems.
        TooManyStreams,

        # TODO EACCES
        # TODO EFAULT
        # TODO ELOOP
        # TODO ENAMETOOLONG
        # TODO ENFILE
        # TODO ENOTDIR ("A component of the path prefix is not a directory")
        # TODO ENXIO
        # TODO EPERM

        ## The filesystem gave an unknown error with this description string.
        Unknown Str,
    ]

## An error that can occur when closing a file stream.
CloseErr :
    [
        ## The stream was closed or otherwise somehow invalid.
        ##
        ## This corresponds to the `EBADF` error on UNIX systems.
        StreamWasClosed,

        # TODO EIO
        # TODO ENOSPC, EDQUOT

        ## The filesystem gave an unknown error with this description string.
        Unknown Str,
    ]

TempErr :
    [
        # TODO EOPNOTSUPP
    ]OpenPathErr

## An error that can occur when reading from a file stream.
ReadErr :
    [
    ]StreamErr

## An error that can occur when writing to a stream.
WriteErr :
    [
    ]StreamErr

## An error that can occur when using any stream.
StreamErr :
    [
        ## The stream was closed or otherwise somehow invalid.
        ##
        ## This corresponds to the `EBADF` error on UNIX systems.
        StreamWasClosed,

        ## The operating system's per-process limit on how many file descriptors
        ## or handles has been reached, so no more file streams can be created
        ## until at least one other is closed (or the OS limit is increased).
        ##
        ## This corresponds to the `EMFILE` error on UNIX systems.
        TooManyStreams,

        ## The filesystem gave an unknown error with this description string.
        Unknown Str,
    ]


# Creating Streams

## Open a file with `Read` permission as a stream, use that stream to run a
## task, and close the file automatically afterwards.
##
## writeSmileyIfEmpty : Task
## writeSmileyIfEmpty =
##     stream <- openRead "source.txt" {}
##
##     text <- Task.await (readUntilEof stream)
openRead : Str, (Stream [ Read ] -> Task ok []err) -> Task ok [ OpenFailed OpenErr Str ]err
openRead = \path, config, fdToTask ->
    Effect.openRead path config
        |> Effect.after \result ->
            when result is
                Ok fd -> fdToTask fd
                Err err -> Task.fail (OpenFailed err path)

## Open a stream with `Read` and `Close` permissions.
##
## Whereas [openRead] always closes the file right after the task completes,
## this lets you close the file sooner with [Stream.close] if you like. If you
## don't, the platform will decide when to close the file - which could result
## in the the file staying open longer than it would have with [openRead].
openReadClose : Str -> Task (Stream [ Read, Close ]) [ OpenFailed OpenErr Str ]*
openReadClose = \path, config ->
    Effect.openRead
        |> makeOpenCloseTask path config

openWrite : Str, (Stream [ Write ] -> Task ok []err) -> Task ok [ OpenFailed OpenWriteErr Str ]err

openWriteClose : Str -> Task (Stream [ Write, Close ]) [ OpenFailed OpenWriteErr Str ]*
openWriteClose = \path, config ->
    Effect.openWrite
        |> makeOpenCloseTask path config

openAppend : Str, (Stream [ Append ] -> Task ok []err) -> Task ok [ OpenFailed OpenWriteErr Str ]err

openAppendClose : Str -> Task (Stream [ Append, Close ]) [ OpenFailed OpenWriteErr Str ]*
openAppendClose = \path, config ->
    Effect.openAppend
        |> makeOpenCloseTask path config

openReadAppend : Str, (Stream [ Read, Append ] -> Task ok []err) -> Task ok [ OpenFailed OpenWriteErr Str ]err

openReadAppendClose : Str -> Task (Stream [ Read, Append, Close ]) [ OpenFailed OpenWriteErr Str ]*
openReadAppendClose = \path, config ->
    Effect.openReadAppend
        |> makeOpenCloseTask path config

## Open a file with `Read` and `Write` permissions as a stream, use that stream
## to run a task, and close the file after that task either succeeds or fails.
##
## >>> writeSmileyIfEmpty :
## >>>     Task {}
## >>>         [
## >>>             OpenFailed OpenWriteErr Str,
## >>>             ReadFailed ReadErr Str,
## >>>             WriteFailed WriteErr Str,
## >>>         ]
## >>> writeSmileyIfEmpty =
## >>>     stream <- openReadWrite "source.txt" {}
## >>>
## >>>     bytes <- Task.await (read stream 0 Eof) # Read the whole file
## >>>
## >>>     if List.isEmpty bytes then
## >>>         write stream 0 (Str.toUtf8 ":)")
## >>>     else
## >>>         Task.succeed {}
openReadWrite : Str, (Stream [ Read, Write ] -> Task ok []err) -> Task ok [ OpenFailed OpenWriteErr Str ]err

openReadWriteClose : Str -> Task (Stream [ Read, Write, Close ]) [ OpenFailed OpenWriteErr Str ]*
openReadWriteClose = \path, config ->
    Effect.openReadWrite
        |> makeOpenCloseTask path config

# Create - create a file
#
## Create a file at the given path, and open it as a stream with write permissions only.
##
## Fails if there is already a file with that name.
createWrite : Str, Mode, (Stream [ Write ] -> Task ok []err) -> Task ok [ CreateFailed OpenWriteErr Str ]err

# On Windows, use CreateFileA with the CREATE_NEW flag to create files and error if they exist already:
# https://docs.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-createfilea

createWriteClose : Str, Mode -> Task (Stream [ Write, Close ]) [ CreateFailed OpenWriteErr Str ]*

createReadWrite : Str, Mode, (Stream [ Read, Write ] -> Task ok []err) -> Task ok [ CreateFailed OpenWriteErr Str ]err

createReadWriteClose : Str, Mode -> Task (Stream [ Read, Write, Close ]) [ CreateFailed OpenWriteErr Str ]*

# Temporary files
#
# Different OSes have different ways to create temporary files. Characteristics
# we want:
#
# 1. The OS will delete the file automatically when the fd is closed. Even if the Roc program segfaults, the tempfile will be deleted.
# 2. It should be secure; an attacker shouldn't be able to open it.
#
# MACOS, FREEBSD, NETBSD
#
# Create the file with O_CREAT, O_EXCL, and O_EXLOCK - this guarantees that
# either creating will fail, or else we have the exclusive lock on the file.
# That in turn means we can unlink it being certain that no other process got a
# handle to it in the meantime; as long as our process doesn't crash right
# between when the open and the unlink happen, we'll be in the state we want.
# (Also, if it does crash, then we end up with an empty file in the tempdir,
# but nothing was written to it yet!) If creating fails because there's already
# a file in the tempdir with that name, keep retrying with different names
# until we find one that works.
#
# LINUX
#
# https://www.man7.org/linux/man-pages/man2/openat.2.html
# use open with O_TMPFILE - must be specified with either write or readwrite
# permissions, no appending! Gives us an unlinked file, which is what we want.
# (For a named file, use File.Temp instead. I guess we can also have File.Temp.link?)

## Open an unnamed temporary file with `Read` and `Write` permissions as a stream,
## use that stream to run a task, and delete the file after that task either succeeds
## or fails.
##
## This is designed to be secure, which means:
## * This temporary file should be both unique and inaccessible to other processes while it is open.
## * It should not be possible for the temporary file to be deleted before the stream is closed.
## * Once the stream is closed, or if the Roc program is terminated early, the operating system should delete the file immediately. There should be no opportunity for any other process to read the file before it is deleted.
##
## > If the Roc program is terminated early at precisely the wrong moment, it's
## > theoretically possible for an empty file with a randomly-generated name to
## > end up left behind in the sytem's temporary directory. In the extremely
## > unlikely event that this happens, the file will be guaranteed to be empty;
## > nothing will ever have been written to it. This situation cannot happen on
## > Windows or Linux, but it can happen on other operating systems.
createTemp : (Stream [ Read, Write ] -> Task ok []err) -> Task ok [ CreateTempFailed OpenWriteErr ]err

createTempClose : Task (Stream [ Read, Write, Close ]) [ CreateTempFailed OpenWriteErr ]*

# Truncation - open an existing file and immediately truncate it to 0 bytes

# NOTE: to do this on Windows, use CreateFileA with the TRUNCATE_EXISTING flag
# https://docs.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-createfilea

truncWrite : Str, (Stream [ Write ] -> Task ok []err) -> Task ok [ TruncFailed OpenWriteErr Str ]err

truncWriteClose : Str -> Task (Stream [ Write, Close ]) [ TruncFailed OpenWriteErr Str ]*

truncReadWrite : Str, (Stream [ Read, Write ] -> Task ok []err) -> Task ok [ TruncFailed OpenWriteErr Str ]err

truncReadWriteClose : Str -> Task (Stream [ Read, Write, Close ]) [ TruncFailed OpenWriteErr Str ]*

truncAppend : Str, (Stream [ Append ] -> Task ok []err) -> Task ok [ TruncFailed OpenWriteErr Str ]err

truncAppendClose : Str -> Task (Stream [ Append, Close ]) [ TruncFailed OpenWriteErr Str ]*

## Close a file stream. Any future tasks run on this stream will fail.
close : Stream [ Close ]* -> Task {} [ CloseFailed CloseErr Str ]*
close = \stream ->
    { fd, path } = toRaw stream

    Effect.close fd
        |> Task.mapFail \err -> CloseFailed err path

## Read the file metadata for the file this stream is opened on.
metadata : Stream * -> Task {} [ MetaFailed MetaErr Str ]*

## Set the size of a file to be the given number of bytes.
##
## If the new size is smaller than the file's current size, the extra bytes
## will be lost. If the new size is larger, the additional bytes will be zeroes.
setSize : Stream [ Write ]*, Nat -> Task {} [ MetaFailed MetaErr Str ]*

## Set whether the file is read-only.
setReadonly : Stream [ Write ]*, Bool -> Task {} [ PermFailed PermErr Str ]*

## On Windows, this always fails with the `SetExeUnsupported` error.
setExecutable : Stream [ Write ]*, Bool -> Task {} [ SetExeFailed [ SetExeUnsupported ]PermErr Str ]*

## Duplicate a stream with the `Close` permission.
##
## This allows calling [close] on either the old stream or the new stream,
## while still having the file remain open on the filesystem (with the other
## stream).
duplicate : Stream [ Close ]a -> Task (Stream [ Close ]a) [ DupFailed DupErr Str ]*

## Instruct the filesystem to flush any buffered data for this file to the
## underlying disk.
##
## Filesystems typically buffer some file data in memory before periodically
## writing that data to the actual underlying disk. This sends a request to the
## filesystem that it should immediately flush (synchronize) that data to the
## disk. This task succeeds after the filesystem has reported that the flush
## completed successfully.
##
## This can reduce performance compared to letting the filesystem flush its
## buffers according to its own preferred schedule, but it can be useful when
## you explicitly do not want to proceed until you are confident your data has
## been flushed to disk already.
##
## Note that although this flushes both contents and metadata for the file
## this stream has opened, it doesn't guarantee that the file's directory entry
## in its parent directory will also be flushed to disk. (Directories can be
## be flushed separately to persist their entries to disk.)
flush : Stream [ Write ]* -> Task {} [ SyncFailed SyncErr Str ]*
# fsync on UNIX, FlushFileBuffers on windows
# https://docs.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-flushfilebuffers

## Instruct the filesystem to flush any buffered data for this file to the
## underlying disk, without also flushing metadata.
##
## On UNIX, this works as intended. On Windows, flushing data but not metadata
## is unsupported, so calling this on Windows is equivalent to calling [flush].
flushDataOnly : Stream [ Write ]* -> Task {} [ SyncFailed SyncErr Str ]*
# `fdatasync` on UNIX

## Read from a stream starting at the given byte index, reading either until
## the end of the file (`Eof`) or at most the given number of bytes (`AtMost`).
#
## This may encounter the end of the file before reading the requested number of
## `AtMost` bytes, in which case it will return all the bytes it read before it
## hit the end of the file.
read : Stream [ Read ]*, Nat, [ Eof, AtMost Nat ] -> Task (List U8) [ ReadFailed ReadErr Str ]*
read = \stream, index, config ->
    # This API mainly takes the [ Eof, AtMost Nat ] argument as a way to prevent
    # something like `readAtMostBytes` from taking two `Nat` arguments in a row,
    # which could get mixed up. Immediately delegate to more specialized
    # functions, so that the function will be reliably inlined, and this
    # conditional will be optimized away!
    when config is
        Eof -> readUntilEof stream index
        AtMost bytes, -> readAtMostBytes stream index bytes

## Helper for `read` (not exposed)
readUntilEof : Stream [ Read ]*, Nat  -> Task (List U8) [ ReadFailed ReadErr Str ]*
readUntilEof = \stream, index ->
    { fd, path } = toRaw stream

    Effect.readUntilEof fd
        |> Effect.map \res -> Result.mapErr res \err -> ReadFailed err path

## Helper for `read` (not exposed)
readAtMostBytes : Stream [ Read ]*, Nat, Nat  -> Task (List U8) [ ReadFailed ReadErr Str ]*
readAtMostBytes = \stream, index, bytes ->
    { fd, path } = toRaw stream

    Effect.readAtMostBytes fd bytes
        |> Effect.map \res -> Result.mapErr res \err -> ReadFailed err path

## Write the given bytes to a file stream, beginning at the given byte offset
## from the start of the stream.
write : Stream [ Write ]*, Nat, List U8 -> Task {} [ WriteFailed WriteErr Str ]*
write = \stream, offset, contents ->
    { fd, path } = toRaw stream

    # TODO: make sure we're giving pwrite the correct offset size on all
    # targets (64-bit on 64-bit targets, 32-bit on 32-bit targets) - the
    # docs for this are confusing.
    Effect.writeStart fd offset contents
        |> Task.mapFail \err -> WriteFailed err path

## Write the given bytes to a file stream, beginning at the given byte offset
## from the end of the stream.
writeFromEnd : Stream [ Write ]*, Nat, List U8 -> Task {} [ WriteFailed WriteErr ]*
writeFromEnd = \stream, offset, contents ->
    { fd, path } = toRaw stream

    Effect.writeEnd fd offset contents
        |> Task.mapFail \err -> WriteFailed err path

## Append
##
## Note: On typical filesystems, it's safe if different processes append to the
## same file at once, because the append process finds the end of the file and
## writes to it as an atomic operation. (TODO: that's true on Linux; is it true
## on every OS?)
##
## However, on NFS filesystems, appending to the same file from different
## processes can cause file corruption. This is because NFS does not support an
## atomic append operation, which creates an unavoidable race condition between
## finding the end of the file and writing to it.
append : Stream [ Append ]*, List U8 -> Task {} [ AppendFailed WriteErr Str ]*
append = \stream, contents ->
    { fd, path } = toRaw stream

    Effect.writeCur fd contents
        |> Task.mapFail \err -> AppendFailed err path

lockShared : Stream [ Write ]* ->
lockShared = \stream ->
    # NOTE: This requires a Write stream because Linux requires it for locking
    # files over NFS.
    #
    # https://www.man7.org/linux/man-pages//man2/flock.2.html
    todo

## Helper for translating raw effect outputs into appropriate tasks (not exposed)
makeOpenCloseTask : Str, (Str -> Task Fd err) -> Task (Stream *) [ OpenFailed err Str ]*
makeOpenCloseTask = \path, config, toEffect ->
    toEffect path config
        |> Effect.map \result ->
            when result is
                Ok fd -> Ok (fromRaw { fd, path })
                Err err -> OpenFailed err path
