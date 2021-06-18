# TODO: we need to give platform authors a way to declare streams as
# "managed resources" that get automatically cleaned up during panic unwinding
# or when they become unreachable. Until then, these are risky to use! You need to remember to close them manually, or else they'll leak. Also you need to
# make sure not to use-after-close, because you might end up reading from a
# totally different file! Managed resources let platform authors silently
# prevent both of these problems.

# NOTES
#
# This should be a capability module, and all other file operations should
# be built using it.
#
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

## A stream of bytes. This could be coming from a file, a socket,
interface File
    exposes [ Stream ]
    imports [ Stream.Internal.{ toRaw, fromRaw }, Task.{ Task }, File.{ Path } ]

## On UNIX systems, this refers to a file descriptor.
## On Windows, it refers to a file handle.
Stream a : Stream.Internal.Stream a

Mode :
    {
        # TODO file modes
    }

OpenConfig :
    {
        ## Default: Fail
        ifNotExists ? [ Fail, Create Mode ] # O_CREAT

        ## Default: False
        truncate ? Bool # O_TRUNC

        ## Default: True
        fileCanBeSymlink ? Bool # O_NOFOLLOW - TODO: when O_NOFOLLOW is set, ELOOP means something different and should be translated to a different error variant.
    }

## An error that can occur when opening a file stream.
OpenErr :
    [

    ]

## An error that can occur when closing a file stream.
CloseErr :
    [

    ]

## An error that can occur when reading from a file stream.
ReadErr others :
    [
        # EAGAIN and EWOULDBLOCK should never happen, because the host should
        # always choose either nonblocking or blocking I/O.
        StreamWasClosed
    ]others

## An error that can occur when writing to a stream.
WriteErr :
    [
        FileWasOpenedNonBlocking
    ]

## Reads the given number of bytes from a stream, then returns those bytes
## along with a new stream which has been [advance]d to right after those bytes.
read : Stream [ Read ]*, Nat -> Task (List U8) [ FileRead ReadErr ]*
read = \stream, bytes ->
    toRaw stream
        |> Effect.read bytes
        |> Effect.map (\result -> Result.mapErr FileRead result)

## Read from the stream until it ends.
##
## Note that the task will not complete until the end is encountered, which for
## streams like `stdin` may never happen without specific user input - so this
## task might never complete!
readUntilEnd : Stream [ Read ]* -> Task (List U8) [ FileRead ReadErr ]*
readUntilEnd = \stream ->
    toRaw stream
        |> Effect.readUntilEof
        |> Effect.map (\result -> Result.mapErr FileRead result)

## Write the given bytes to a file stream, beginning at the given byte offset
## from the start of the stream.
write : Stream [ Write ]*, Nat, List U8 -> Task {} [ FileWrite WriteErr ]*
write = \stream, offset, bytes ->
    toRaw stream
        # TODO: make sure we're giving pwrite the correct offset size on all
        # targets (64-bit on 64-bit targets, 32-bit on 32-bit targets) - the
        # docs for this are confusing.
        |> Effect.write offset bytes
        |> Effect.map (\result -> Result.mapErr FileWrite result)


# The file is opened in append mode.  Before each write(2),
# the file offset is positioned at the end of the file, as
# if with lseek(2).  The modification of the file offset and
# the write operation are performed as a single atomic step.

# O_APPEND may lead to corrupted files on NFS filesystems if
# more than one process appends data to a file at once.
# This is because NFS does not support appending to a file,
# so the client kernel has to simulate it, which can't be
# done without a race condition.
append : Stream [ Append ]*, List U8 -> Task {} [ FileWrite WriteErr ]*
append = \stream, bytes ->
    toRaw stream
        |> Effect.writeCur offset bytes
        |> Effect.map (\result -> Result.mapErr FileWrite result)

## Write the given bytes to a file stream, beginning at the given byte offset
## from the end of the stream.
writeFromEnd : Stream [ Write ]*, Nat, List U8 -> Task {} [ FileWrite WriteErr ]*
writeFromEnd = \stream, offset bytes ->
    toRaw stream
        # TODO: make sure we're giving pwrite the correct offset size on all
        # targets (64-bit on 64-bit targets, 32-bit on 32-bit targets) - the
        # docs for this are confusing.
        |> Effect.writeEnd offset bytes
        |> Effect.map (\result -> Result.mapErr FileWrite result)

## Close a file stream. Any future tasks run on this stream will fail.
close : Stream * -> Task {} [ FileClose CloseErr ]*
close = \stream ->
    fromRaw stream
        |> Effect.close stream
        |> Effect.map (\result -> Result.mapErr StreamClose result)

## Open a file and creates a stream that refers to the start of that file.
openRead : Str -> Task (Stream [ Read ]) [ FileOpen OpenErr ]*
openRead = \path, mode ->
    Effect.openRead path
        |> Effect.map (\result -> Result.mapErr FileOpen result)

## Open a file and creates a stream that refers to the start of that file.
openWrite : Str -> Task (Stream [ Write ]) [ FileOpen OpenErr ]*
openWrite = \path, mode ->
# O_CLOEXEC, O_CREAT, O_DIRECTORY, O_EXCL, O_NOCTTY, O_NOFOLLOW, O_TMPFILE, O_TRUNC
    Effect.openWrite path
        |> Effect.map (\result -> Result.mapErr FileOpen result)

## Creates a temporary file in the given directory and opens it for writing.
##
## To open the temporary file for both reading and writing, use [openTempReadWrite].
openTmp : Str -> Task (Stream [ Write ]) [ FileOpen OpenErr ]*
openTmp = \path ->
    # NOTE: need to figure out what to do about O_EXCL here
    #
    # Uses for this, from the docs: https://www.man7.org/linux/man-pages/man2/open.2.html
    #
    # * Improved tmpfile(3) functionality: race-free creation
    #   of temporary files that (1) are automatically deleted
    #   when closed; (2) can never be reached via any pathname;
    #   (3) are not subject to symlink attacks; and (4) do not
    #   require the caller to devise unique names.

    # *  Creating a file that is initially invisible, which is
    #   then populated with data and adjusted to have
    #   appropriate filesystem attributes (fchown(2),
    #   fchmod(2), fsetxattr(2), etc.)  before being atomically
    #   linked into the filesystem in a fully formed state
    #   (using linkat(2) as described above).
    Effect.openWrite path
        |> Effect.map (\result -> Result.mapErr FileOpen result)

openTmpReadWrite : Str -> Task (Stream [ Read, Write ]) [ FileOpen OpenErr ]*

## Open a file and creates a stream that refers to the start of that file.
openAppend : Str -> Task (Stream [ Append ]) [ FileOpen OpenErr ]*
openAppend = \path, mode ->
    fromRaw stream
        |> Effect.openWrite path
        |> Effect.map (\result -> Result.mapErr FileOpen result)

## Open a file and creates a stream that refers to the start of that file.
openReadAppend : Str -> Task (Stream [ Read, Append ]) [ FileOpen OpenErr ]*
openReadAppend = \path, mode ->
    fromRaw stream
        |> Effect.openWrite path
        |> Effect.map (\result -> Result.mapErr FileOpen result)

## Open a file and creates a stream that refers to the start of that file.
openReadWrite : Str -> Task (Stream [ Read, Write ]) [ FileOpen OpenErr ]*
openReadWrite = \path ->
    fromRaw stream
        |> Effect.openReadWrite path
        |> Effect.map (\result -> Result.mapErr FileOpen result)

## Open a file and read all of its bytes.
##
## For example, here's how to read a file's bytes and interpret them as a
## UTF-8 string:
##
##     File.readAll "myfile.txt"
##         |> Task.map Str.fromUtf8
readAll : Str -> Task (List U8) [ FileOpen OpenErr, FileRead ReadErr ]*
readAll = \path ->
    stream <- Task.await (openRead path)
    bytes <- Task.await (write stream)

    Task.succeed bytes


## Open a file and write all of the given bytes to it.
##
## For example, here's how to write a string to a file using UTF-8 encoding:
##
##     Str.toUtf8 "contents of the file"
##         |> File.writeAll "myfile.txt"
writeAll : List U8, Str -> Task.Task {} [ FileOpen OpenErr, FileWrite WriteErr ]*
writeAll = \bytes, str ->
    stream <- Task.await (openWrite path)

    write stream 0 bytes


## Read a file's bytes, one chunk at a time, and use it to build up a state.
##
## After each chunk is read, it gets passed to a callback which builds up a
## state - optionally while running other tasks.
#readChunks : Path, U64, state, (state, List U8 -> Task state []err) -> Task state (FileReadErr err)

## Like #readChunks except after each chunk you can either `Continue`,
## specifying how many bytes you'd like to read next, or `Stop` early.
#readChunksOrStop : Path, U64, state, (state, List U8 -> [ Continue U64 (Task state []err), Stop (Task state []err) ]) -> Task state (FileReadErr err)
