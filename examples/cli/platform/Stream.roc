interface File.Stream
    exposes [ Stream ]
    imports [ Stream.Internal, Task.{ Task }, File.{ Path } ]

# Notes
#
# What's weird about this (and pretty frustrating tbh) is that we can't have
# two FDs open to the same point in a file at the same time. It totally makes
# sense why that would be, but

## On UNIX systems, this is a file descriptor.
## On Windows, it's a file handle.
Stream : Stream.Internal.Stream

## Reads the given number of bytes from a stream, then returns those bytes
## along with a new stream which has been [advance]d to right after those bytes.
read : Stream, Nat -> Task { bytes : List U8, next : Stream } Io.Err
read = \stream ->
    Effect.read (Stream.Internal.toRaw stream)
        |> Effect.map Ok

## Writes the given bytes to a stream, then returns a new stream which has been
## [advance]d to right after the bytes that were just written.
write : Stream, List U8 -> Task Stream Io.Err
write = \stream, bytes ->
    Effect.read (Stream.Internal.toRaw stream) bytes
        |> Effect.map Ok

## Opens a file and creates a stream that refers to the start of that file.
open : Path, File.OpenMode -> Task Stream Io.Err
open = \path, mode ->
    Effect.open path mode
        |> Effect.map \stream -> Ok (Stream.fromRaw stream)

## Closes the stream's file. Any future tasks run on this stream will fail.
close : Stream -> Task {} Io.Err
close = \path, mode ->
    Effect.open path mode
        |> Effect.map \stream -> Ok (Stream.fromRaw stream)

## Returns a stream which has been advanced the given number of bytes.
##
## This can change the stream's position to after the end of the file, which
## is not necessarily a problem.
##
## Stream positions can never be less than 0 or greater than [Num.maxI64], so if
## this would position the stream past either of those boundaries, it will
## instead panic with an overflow.
advance : Stream, I64 -> Stream
advance = \stream, bytes ->
    # Note: we should use lseek64 for this - https://linux.die.net/man/3/lseek64
    # because it uses the off64_t type (which guaratnees 64-bit offsets)
    # rather than off_t which doesn't guarantee a size, but which does specify
    # that it must be signed.
    #
    # https://stackoverflow.com/questions/9073667/where-to-find-the-complete-definition-of-off-t-type/14351239#14351239
    #
    # However, that can make things tricky for 32-bit targets. It's not a given
    # that their system libc implementations will have been compiled with
    # support for 64-bit file offsets!
    #
    # https://stackoverflow.com/questions/14184031/what-is-the-difference-between-largefile-source-and-file-offset-bits-64
    #
    # For that reason, 32-bit host implementations would need to gracefully
    # degrade. As in, try to link lseek64, but if it's not available, then
    # fall back on lseek with an explicit check that panicks if it's ever given
    # an I64 offset that can't fit in off_t.
    Effect.lseek64cur (Stream.Internal.toRaw stream) bytes

## Returns the current position referred to by this stream, in bytes from
## the start of its file,
pos : Stream -> U64
pos = \stream ->
    Effect.tell64 (Stream.Internal.toRaw stream)

## Repositions the stream to the start of its file plus the given number of bytes.
##
## This can change the stream's position to after the end of the file, which
## is not necessarily a problem.
##
## Stream positions can never be less than 0 or greater than [Num.maxI64], so if
## this would position the stream past either of those boundaries, it will
## instead panic with an overflow.
fromStart : Stream, U64 -> Stream
fromStart = \stream, bytes ->
    Effect.lseek64start (Stream.Internal.toRaw stream) bytes

## Repositions the stream to the end of its file minus the given number of bytes.
##
## This can be negative because it is allowed to change the stream's position to
## after the end of the file.
##
## Stream positions can never be less than 0 or greater than [Num.maxI64], so if
## this would position the stream past either of those boundaries, it will
## instead panic with an overflow.
fromEnd : Stream, I64 -> Stream
fromEnd = \stream, bytes ->
    Effect.lseek64start (Stream.Internal.toRaw stream) bytes
