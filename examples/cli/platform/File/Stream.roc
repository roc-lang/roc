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


## On UNIX systems, this refers to a file descriptor.
## On Windows, it refers to a file handle.
Stream a : Internal.Stream a

Mode :
    {
        # TODO file modes like read-only, executable, etc.
    }

## An error that can occur when opening a file stream.
OpenErr :
    [
        # TODO list all the errors that can happen when opening a file

        ## The filesystem gave an unknown error with this description string.
        Unknown Str
    ]

## An error that can occur when closing a file stream.
CloseErr :
    [
        # TODO list all the errors that can happen when closing a file

        ## The filesystem gave an unknown error with this description string.
        Unknown Str,
    ]

## An error that can occur when reading from a file stream.
ReadErr :
    [
        StreamWasClosed,
        # TODO list all the other errors that can happen when reading from a file

        ## The filesystem gave an unknown error with this description string.
        Unknown Str,
    ]

## An error that can occur when writing to a stream.
WriteErr :
    [
        StreamWasClosed,
        # TODO list all the other errors that can happen when writing to a file

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

openWrite : Str, (Stream [ Write ] -> Task ok []err) -> Task ok [ OpenFailed OpenErr Str ]err

openWriteClose : Str -> Task (Stream [ Write, Close ]) [ OpenFailed OpenErr Str ]*
openWriteClose = \path, config ->
    Effect.openWrite
        |> makeOpenCloseTask path config

openAppend : Str, (Stream [ Append ] -> Task ok []err) -> Task ok [ OpenFailed OpenErr Str ]err

openAppendClose : Str -> Task (Stream [ Append, Close ]) [ OpenFailed OpenErr Str ]*
openAppendClose = \path, config ->
    Effect.openAppend
        |> makeOpenCloseTask path config

openReadAppend : Str, (Stream [ Read, Append ] -> Task ok []err) -> Task ok [ OpenFailed OpenErr Str ]err

openReadAppendClose : Str -> Task (Stream [ Read, Append, Close ]) [ OpenFailed OpenErr Str ]*
openReadAppendClose = \path, config ->
    Effect.openReadAppend
        |> makeOpenCloseTask path config

## Open a file with `Read` and `Write` permissions as a stream, use that stream
## to run a task, and close the file after that task either succeeds or fails.
##
## >>> writeSmileyIfEmpty :
## >>>     Task {}
## >>>         [
## >>>             OpenFailed OpenErr Str,
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
openReadWrite : Str, (Stream [ Read, Write ] -> Task ok []err) -> Task ok [ OpenFailed OpenErr Str ]err

openReadWriteClose : Str -> Task (Stream [ Read, Write, Close ]) [ OpenFailed OpenErr Str ]*
openReadWriteClose = \path, config ->
    Effect.openReadWrite
        |> makeOpenCloseTask path config

# Create - create a file
#
## Create a file at the given path, and open it as a stream with write permissions only.
##
## Fails if there is already a file with that name.
createWrite : Str, Mode, (Stream [ Write ] -> Task ok []err) -> Task ok [ CreateFailed OpenErr Str ]err

# On Windows, use CreateFileA with the CREATE_NEW flag to create files and error if they exist already:
# https://docs.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-createfilea

createWriteClose : Str, Mode -> Task (Stream [ Write, Close ]) [ CreateFailed OpenErr Str ]*

createReadWrite : Str, Mode, (Stream [ Read, Write ] -> Task ok []err) -> Task ok [ CreateFailed OpenErr Str ]err

createReadWriteClose : Str, Mode -> Task (Stream [ Read, Write, Close ]) [ CreateFailed OpenErr Str ]*

# Truncation - open an existing file and immediately truncate it to 0 bytes

# NOTE: to do this on Windows, use CreateFileA with the TRUNCATE_EXISTING flag
# https://docs.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-createfilea

truncWrite : Str, (Stream [ Write ] -> Task ok []err) -> Task ok [ TruncFailed OpenErr Str ]err

truncWriteClose : Str -> Task (Stream [ Write, Close ]) [ TruncFailed OpenErr Str ]*

truncReadWrite : Str, (Stream [ Read, Write ] -> Task ok []err) -> Task ok [ TruncFailed OpenErr Str ]err

truncReadWriteClose : Str -> Task (Stream [ Read, Write, Close ]) [ TruncFailed OpenErr Str ]*

truncAppend : Str, (Stream [ Append ] -> Task ok []err) -> Task ok [ TruncFailed OpenErr Str ]err

truncAppendClose : Str -> Task (Stream [ Append, Close ]) [ TruncFailed OpenErr Str ]*

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
openTemp : (Stream [ Read, Write ] -> Task ok []err) -> Task ok [ OpenTempFailed OpenErr ]err

openTempClose : Task (Stream [ Read, Write, Close ]) [ OpenTempFailed OpenErr ]*

## Close a file stream. Any future tasks run on this stream will fail.
close : Stream [ Close ]* -> Task {} [ CloseFailed CloseErr Str ]*
close = \stream ->
    { fd, path } = toRaw stream

    Effect.close fd
        |> Task.mapFail \err -> CloseFailed err path



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

## Helper for translating raw effect outputs into appropriate tasks (not exposed)
makeOpenCloseTask : Str, (Str -> Task Fd err) -> Task (Stream *) [ OpenFailed err Str ]*
makeOpenCloseTask = \path, config, toEffect ->
    toEffect path config
        |> Effect.map \result ->
            when result is
                Ok fd -> Ok (fromRaw { fd, path })
                Err err -> OpenFailed err path
