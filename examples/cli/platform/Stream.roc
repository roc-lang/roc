# TODO: we need to give platform authors a way to declare streams as
# "managed resources" that get automatically cleaned up during panic unwinding
# or when they become unreachable. Until then, these are risky to use! You need
# to remember to close them manually, or else they'll leak. Also you need to
# make sure not to use-after-close, because you might end up reading from a
# totally different file! Managed resources let platform authors silently
# prevent both of these problems.

# NOTES
#
# This should be a capability module, and all other file operations should
# be built using it.

# IDEA:
#
# What if the Stream API capabilities don't specify how to open one of these?
# Like they just specify that you have to have read, write, advance, and close.
# Then, we could have File.stream, Socket.stream, Stdin.stream, etc.
# The platform can implement whichever of these makes sense.
# Then, we don't need to handle OpenErr, just CloseErr.
# This also means we have a cool abstraction for streaming HTTP, sockets, etc.
#
# Concern: how do we know whether to clean up a Socket as a stream or whatever
# else? I guess we'd need to have it store an enum for which type it is, and
# then do a conditional on it. Not the end of the world I suppose.
#
# Concern 2: does "seek" actually make sense in a HTTP stream? Should we really
# offer that? Also, is "seek" even a good API for this? Yeah, what the hell -
# just offer "read from start" and "read from end" and that's it.
# Okay, but then: does "read from end" make sense for HTTP?
#
# NOTE: in windows you can try to close a fd as a socket and if it's not a
# socket, it will return a NOTSOCK error, which in turn means we don't need
# to store whether the stream is a socket in the data structure!
#
# I think maybe Stream shouldn't support seeking at all, and just reading from
# the front. Seems like the only real use case is reading a file from the end,
# or from the middle, and we can just make File-specific functions for those.
#
# Okay, next question: should it be just `Stream` or `Stream a` and by default
# it gives you `Stream (List U8)`? That would mean we could let you specify
# transformations that happen asynchronously as we read each thing! Ooo!
# Nope,
#
# Okay, so:
# * FileStream
# * StdioStream
# * TcpStream
# * OsSocketStream
#
# FileStream supports reading from end, also seeking to middle
# StdioStream doesn't support those
# TcpStream also supports things like shutdown etc.
#
# Should opening be in scope? Prob so we can have the error module be in here,
# so we can do all the errno stuff

## A stream of bytes. This could be coming from a file, a socket,
interface FileStream
    exposes [ FileStream ]
    imports [ Stream.Internal.{ toRaw, fromRaw }, Task.{ Task }, File.{ Path } ]

## On UNIX systems, this refers to a file descriptor.
## On Windows, it refers to a file handle.
FileStream : Stream.Internal.Stream

## An error that can occur when closing a file stream.
OpenErr others :
    [

    ]others

## An error that can occur when closing a stream.
CloseErr others :
    [

    ]others

## An error that can occur when reading from a stream.
ReadErr others :
    [

    ]others

#      EAGAIN The file descriptor fd refers to a file other than a
#             socket and has been marked nonblocking (O_NONBLOCK), and
#             the read would block.  See open(2) for further details on
#             the O_NONBLOCK flag.
#
#      EAGAIN or EWOULDBLOCK
#             The file descriptor fd refers to a socket and has been
#             marked nonblocking (O_NONBLOCK), and the read would block.
#             POSIX.1-2001 allows either error to be returned for this
#             case, and does not require these constants to have the
#             same value, so a portable application should check for
#             both possibilities.
#
#      EBADF  fd is not a valid file descriptor or is not open for
#             reading.
#
#      EFAULT buf is outside your accessible address space.
#
#      EINTR  The call was interrupted by a signal before any data was
#             read; see signal(7).
#
#      EINVAL fd is attached to an object which is unsuitable for
#             reading; or the file was opened with the O_DIRECT flag,
#             and either the address specified in buf, the value
#             specified in count, or the file offset is not suitably
#             aligned.
#
#      EINVAL fd was created via a call to timerfd_create(2) and the
#             wrong size buffer was given to read(); see
#             timerfd_create(2) for further information.
#
#      EIO    I/O error.  This will happen for example when the process
#             is in a background process group, tries to read from its
#             controlling terminal, and either it is ignoring or
#             blocking SIGTTIN or its process group is orphaned.  It may
#             also occur when there is a low-level I/O error while
#             reading from a disk or tape.  A further possible cause of
#             EIO on networked filesystems is when an advisory lock had
#             been taken out on the file descriptor and this lock has
#             been lost.  See the Lost locks section of fcntl(2) for
#             further details.

## An error that can occur when writing to a stream.
WriteErr others :
    [

    ]others

## Reads the given number of bytes from a stream, then returns those bytes
## along with a new stream which has been [advance]d to right after those bytes.
read : Stream, Nat -> Task (List U8) (ReadErr *)
read = \stream, bytes ->
    toRaw stream
        |> Effect.read bytes
        |> Effect.map (\result -> Result.mapErr errnoToErr result)

## Read from the stream until it ends.
##
## Note that the task will not complete until the end is encountered, which for
## streams like `stdin` may never happen without specific user input - so this
## task might never complete!
readUntilEnd : Stream -> Task (List U8) (ReadErr *)
readUntilEnd = \stream ->
    toRaw stream
        |> Effect.readUntilEof
        |> Effect.map (\result -> Result.mapErr errnoToErr result)

## Write the given bytes to a file stream, beginning at the given byte offset
## from the start of the stream.
write : Stream, Nat, List U8 -> Task {} (WriteErr *)
write = \stream, offset bytes ->
    toRaw stream
        # TODO: make sure we're giving pwrite the correct offset size on all
        # targets (64-bit on 64-bit targets, 32-bit on 32-bit targets) - the
        # docs for this are confusing.
        |> Effect.write offset bytes
        |> Effect.map (\result -> Result.mapErr errnoToErr result)

## Write the given bytes to a file stream, beginning at the given byte offset
## from the end of the stream.
writeEnd : Stream, Nat, List U8 -> Task {} (WriteErr *)
writeEnd = \stream, offset bytes ->
    toRaw stream
        # TODO: make sure we're giving pwrite the correct offset size on all
        # targets (64-bit on 64-bit targets, 32-bit on 32-bit targets) - the
        # docs for this are confusing.
        |> Effect.writeEnd offset bytes
        |> Effect.map (\result -> Result.mapErr errnoToErr result)

## Close a file stream. Any future tasks run on this stream will fail.
close : Stream -> Task {} (CloseErr *)
close = \stream ->
    fromRaw stream
        |> Effect.close stream
        |> Effect.map (\result -> Result.mapErr errnoToErr result)

## Open a file and creates a stream that refers to the start of that file.
open : Path, File.OpenMode -> Task Stream (OpenErr *)
open = \path, mode ->
    fromRaw stream
        |> Effect.open path
        |> Effect.map (\result -> Result.mapErr errnoToOpenErr result)

## Private helper for converting
errnoToOpenErr
