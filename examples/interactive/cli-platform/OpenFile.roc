interface OpenFile
    exposes [OpenFile, OpenErr, MetadataErr, ReadErr, WriteErr, fromStr, toStr]
    imports [File.{ Metadata }]

## An [OpenFile] represents a [file descriptor](https://en.wikipedia.org/wiki/File_descriptor)
## on UNIX systems or a [file handle](https://docs.microsoft.com/en-us/windows/win32/fileio/file-handles)
## on Windows.
##
## When you open a file, for example using [openRead], that file will remain open as long as
## the [OpenFile] is still referenced anywhere in the program. Once the program no longer has
## a reference to the [OpenFile], the corresponding file will be closed automatically.
OpenFile permissions := {
    # A Windows HANDLE is an isize, and a UNIX file descriptor is i32.
    # A Nat will always be enough to fit either, on a 32-bit or 64-bit target.
    # On a 16-bit target, Nat will be too small, meaning that you can only have
    # ~65,000 files open at a time on your 16-bit target. That's probably way
    # more than you can have open on that target in practice anyway, so this seems fine.
    # If somehow that does turn out to be a problem, we can always make this a 64-bit integer,
    # which will be bigger than necessary on 32-bit and 16-bit targets, but will work
    # everywhere.
    #
    # An OpenFile should only ever be created in the host,
    # because only the host can create an entry into the "needs closing on dealloc" hashmap!
    handleOrFd : Nat,

    # Standard I/O streams (stdout, stderr, stdin) are always open, so no heap
    # allocation is needed to track whether there are any references left to them.
    # Other files need closing, so we use a zero-sized Box to track the reference count.
    #
    # In the host's roc_dealloc, whenever it receives a request to deallocate 0 bytes,
    # it checks to see if the pointer it received is an entry in the global hashmap of
    # pointers to file descriptors, which gets an entry inserted whenever we do an `open`.
    #
    # This is a (Box []) to emphasize that this should never be created outside the host,
    # because only the host can create an entry into the "needs closing on dealloc" hashmap!
    references : [AlwaysOpen, NeedsClosing (Box [])],
} # has Eq, Hash, Ord # no Encode or Decode; you should never serialize these!

## ## Opening Files

# TODO what's the win32 call version of `open` in UNIX?
openRead : Path -> Task (OpenFile [Read [Disk]*]) (OpenErr *) [Read [Disk]*]*
openWrite : Path -> Task (OpenFile [Write [Disk]*]) (OpenErr *) [Read [Disk]*]*
openReadWrite : Path -> Task (OpenFile [Read [Disk]*, Write [Disk]*]) (OpenErr *) [Read [Disk]*]*

# TODO what are the win32 and Linux calls for tempfiles?
openTempRead : Str -> Task (OpenFile [Read [Disk]*]) (OpenErr *) [Read [Disk]*]*
openTempWrite : Str -> Task (OpenFile [Write [Disk]*]) (OpenErr *) [Read [Disk]*]*
openTempReadWrite : Str -> Task (OpenFile [Read [Disk]*, Write [Disk]*]) (OpenErr *) [Read [Disk]*]*

openReadRestricted : Path, token -> Task (OpenFile [Read [Disk, Restricted token]*]) (OpenErr *) [Read [Disk]*]*
openWriteRestricted : Task (OpenFile [Write [Disk, Restricted token]*]) (OpenErr *) [Read [Disk]*]*
openReadWriteRestricted : Task (OpenFile [Read [Disk, Restricted token]*, Write [Disk, Restricted token]*]) (OpenErr *) [Read [Disk]*]*

## ## File I/O

# Attempt to read the given number of bytes. There may not have been enough bytes, in which
# case the List U8 will have a length lower than Nat.
read : OpenFile [Read a]*, Nat -> Task (List U8) (ReadErr *) [Read a]*

# pread on Linux - TODO: is there an equivalent on Windows?
readAt : OpenFile [Read a]*, { bytes : Nat, offset : Nat } -> Task (List U8) (ReadErr *) [Read a]*

# Technically this means you can read the metadata of stdin/stdout/stderr - which I
# suppose is well-defined and harmless, but then again it might be a mistake. You
# can always special-case those if you have a code path that tries to read metadata,
# by explicitly doing an == check to see if what you have happens to be
metadata : OpenFile [Read a]* -> Task Metadata (ReadErr *) [Metadata a]*

# write
write : OpenFile [Write a]*, List U8 -> Task (WriteErr *) [Write a]*
writeUtf8 : OpenFile [Write a]*, Str -> Task (WriteErr *) [Write a]*
writeUtf16 : OpenFile [Write a]*, Str -> Task (WriteErr *) [Write a]*

# pwrite on UNIX - TODO is there an equivalent in Windows?
writeAt : OpenFile [Write a]*, List U8, Nat -> Task (WriteErr *) [Write a]*
writeUtf8At : OpenFile [Write a]*, Str, Nat -> Task (WriteErr *) [Write a]*
writeUtf16At : OpenFile [Write a]*, Str, Nat -> Task (WriteErr *) [Write a]*

# Resize to the given number of bytes - ftruncate on UNIX, ??? on Windows
# TODO: On Linux, ftruncate pads with \0 bytes if you make it bigger. Does it on Windows?
resize : OpenFile [Write a]*, Nat -> Task (WriteErr *) [Write a]*

# sync() on UNIX - TODO: is there a Windows equivalent?
flushAll : Task (WriteErr *) [Write a]*

# fsync on UNIX, or maybe syncfs? What's the difference? - TODO: is there a Windows equivalent?
#
# NOTE: from https://man7.org/linux/man-pages/man3/stdio.3.html
#     In cases where a large amount of computation is
#     done after printing part of a line on an output terminal, it is
#     necessary to fflush(3) the standard output before going off and
#     computing so that the output will appear.
# So this means when doing writes to stdout/stderr, may need to flush them explicitly.
# ALSO means, when doing Stdout.line, should always flush right after I suppose.
flush : OpenFile [Write a]*, Nat -> Task (WriteErr *) [Write a]*

# fdatasync on UNIX - TODO: is there a Windows equivalent?
#
# From the docs: "The aim of fdatasync() is to reduce disk activity for
# applications that do not require all metadata to be synchronized
# with the disk."
flushNonMetadata : OpenFile [Write a]*, Nat -> Task (WriteErr *) [Write a]*

## ## Standard I/O

## An [OpenFile] to read from [standard output](https://en.wikipedia.org/wiki/Standard_streams#Standard_output_(stdout))
## (`stdout`). *Reading* from `stdout` is very uncommon;
## it is much more common to write to it, for example using [stdoutWrite].
##
## The `stdout` file is always open, so there's no need to [open] it.
stdoutRead : OpenFile [Read [Stdout]*]

## An [OpenFile] to write to [standard output](https://en.wikipedia.org/wiki/Standard_streams#Standard_output_(stdout))
## (`stdout`).
##
## The `stdout` file is always open, so there's no need to [open] it.
stdoutWrite : OpenFile [Write [Stdout]*]

## An [OpenFile] to read from or write to [standard output](https://en.wikipedia.org/wiki/Standard_streams#Standard_output_(stdout))
## (`stdout`). *Reading* from `stdout` is very uncommon;
## it is much more common to write to it.
##
## The `stdout` file is always open, so there's no need to [open] it.
stdoutReadWrite : OpenFile [Read [Stdout]*, Write [Stdout]*]

## An [OpenFile] to read from [standard error](https://en.wikipedia.org/wiki/Standard_streams#Standard_error_(stderr))
## (`stderr`). *Reading* from `stderr` is very uncommon;
## it is much more common to write to it, for example using [stderrWrite].
##
## The `stderr` file is always open, so there's no need to [open] it.
stderrRead : OpenFile [Read [Stderr]*]

## An [OpenFile] to write to [standard error](https://en.wikipedia.org/wiki/Standard_streams#Standard_error_(stderr))
## (`stderr`).
##
## The `stderr` file is always open, so there's no need to [open] it.
stderrWrite : OpenFile [Write [Stderr]*]

## An [OpenFile] to read from or write to [standard error](https://en.wikipedia.org/wiki/Standard_streams#Standard_error_(stderr))
## (`stderr`). *Reading* from `stderr` is very uncommon;
## it is much more common to write to it.
##
## The `stderr` file is always open, so there's no need to [open] it.
stderrReadWrite : OpenFile [Read [Stderr]*, Write [Stderr]*]

## An [OpenFile] to read from [standard input](https://en.wikipedia.org/wiki/Standard_streams#Standard_input_(stdin))
## (`stdin`).
##
## The `stdin` file is always open, so there's no need to [open] it.
stdinRead : OpenFile [Read [Stdin]*]

## An [OpenFile] to write to [standard input](https://en.wikipedia.org/wiki/Standard_streams#Standard_input_(stdin))
## (`stdin`). *Writing* to `stdin` is very uncommon;
## it is much more common to read from it, for example using [stdinRead].
##
## The `stdin` file is always open, so there's no need to [open] it.
stdinWrite : OpenFile [Write [Stdin]*]

## An [OpenFile] to read from or write to [standard input](https://en.wikipedia.org/wiki/Standard_streams#Standard_input_(stdin))
## (`stdin`). *Writing* to `stdin` is very uncommon;
## it is much more common to read from it.
##
## The `stdin` file is always open, so there's no need to [open] it.
stdinReadWrite : OpenFile [Read [Stdin]*, Write [Stdin]*]

## ## Errors

# TODO these should be different bc they can't just have a Path in them, and
# also because OpenErr should be distinct from ReadErr and WriteErr - like once
# you've dealt with the "open" errors, you shouldn't have to deal with them
# anymore after that (e.g. "file not found" is not in WriteErr). In contrast,
# the `File` errors combine them.

# The source of an error - could be a path,
# or else stdout, stderr, or stdin.
Source : [
    Stdout,
    Stderr,
    Stdin,
    Path Path,
]

# TODO OpenFile needs its own distinct set of errors from File; the latter
# only works with Paths, so its errors should report Path only. In contrast,
# these should work with Source because you might not have a Path if you're
# doing a stdio thing.

OpenErr a : []
ReadErr a : []
WriteErr a : [
    # Special-case error - on UNIX, write and pwrite have this behavior:
    #
    #     On success, the number of bytes written is returned.  On error,
    #     -1 is returned, and errno is set to indicate the error.
    #     Note that a successful write() may transfer fewer than count bytes.
    #
    #     Such partial writes can occur for various reasons; for
    #     example, because there was insufficient space on the disk device
    #     to write all of the requested bytes, or because a blocked write()
    #     to a socket, pipe, or similar was interrupted by a signal handler
    #     after it had transferred some, but before it had transferred all
    #     of the requested bytes.
    #
    # Rather than expect callers to test for this on all otherwise-successful writes,
    # we translate this scenario into an error. The Nat payload is the number of
    # bytes that were successfully written.
    IncompleteWrite Source Nat
]a
MetadataErr : []
