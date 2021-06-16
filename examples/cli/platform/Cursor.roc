interface File.Cursor
    exposes [ Cursor ]
    imports [ Cursor.Internal, Task.{ Task }, File.{ Path } ]

# Notes
#
# What's weird about this (and pretty frustrating tbh) is that we can't have
# two FDs open to the same point in a file at the same time. It totally makes
# sense why that would be, but

## On UNIX systems, this is a file descriptor.
## On Windows, it's a file handle.
Cursor : Cursor.Internal.Cursor

readUtf8 : Cursor -> Task Str (FileReadErr [ BadUtf8 Str.Utf8ByteProblem Nat ]*)
readUtf8 = \cursor ->
    fd = Cursor.Internal.toRaw cursor

    Effect.map (Effect.readUtf8

## Open a file and create a cursor that refers to the start of that file.
open : Path, File.OpenMode -> Task Cursor Io.Err
open = \path, mode ->
    Effect.open path mode

## Advance the position referred to by this cursor by the given number of bytes.
##
## This can change the cursor's position to after the end of the file, which
## is not necessarily a problem.
##
## This task will fail if it would result in the cursor having a negative position.
advance : Cursor, I64 -> Task {} Io.Err
advance = \cursor, bytes ->
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
    Effect.lseek64cur (Cursor.Internal.toRaw cursor) bytes

## Returns the current position referred to by this cursor, in bytes from
## the start of the file,
pos : Cursor -> Task I64 Io.Err
pos = \cursor ->
    Effect.tell64 (Cursor.Internal.toRaw cursor)

## Change the position referred to by this cursor to be the start of
## its file plus the given number of bytes.
##
## This can change the cursor's position to after the end of the file, which
## is not necessarily a problem.
##
## This task will fail if given a number of bytes higher than [Num.maxI64].
fromStart : Cursor, U64 -> Task {} Io.Err
fromStart = \cursor, bytes ->
    Effect.lseek64start (Cursor.Internal.toRaw cursor) bytes

## Change the position referred to by this cursor to be the end of
## its file minus the given number of bytes.
##
## This can change the cursor's position to after the end of the file, which
## is not necessarily a problem.
##
## This task will fail if it would result in the cursor having a negative position.
fromEnd : Cursor, I64 -> Task {} Io.Err
fromEnd = \cursor, bytes ->
    Effect.lseek64start (Cursor.Internal.toRaw cursor) bytes
