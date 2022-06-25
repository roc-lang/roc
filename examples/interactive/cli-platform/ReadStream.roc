interface ReadStream
    exposes [Stream, OpenErr, MetadataErr, ReadErr, WriteErr, fromStr, toStr]
    imports [File.{ Metadata }]

## A stream represents a buffered [file descriptor](https://en.wikipedia.org/wiki/File_descriptor)
## on UNIX systems or a buffered [file handle](https://docs.microsoft.com/en-us/windows/win32/fileio/file-handles)
## on Windows. The buffer size is 8192 bytes.
##
## When you open a file, for example using [openRead], that file will remain open as long as
## the [Stream] is still referenced anywhere in the program. Once the program no longer has
## a reference to the [Stream], the corresponding file will be closed automatically.
ReadStream output permissions := {
    fromBytes : List U8 -> Result input Decode.Err,

    # A Windows HANDLE is an isize, and a UNIX file descriptor is i32.
    # A Nat will always be enough to fit either, on a 32-bit or 64-bit target.
    # On a 16-bit target, Nat will be too small, meaning that you can only have
    # ~65,000 files open at a time on your 16-bit target. That's probably way
    # more than you can have open on that target in practice anyway, so this seems fine.
    # If somehow that does turn out to be a problem, we can always make this a 64-bit integer,
    # which will be bigger than necessary on 32-bit and 16-bit targets, but will work
    # everywhere.
    #
    # A stream should only ever be created in the host,
    # because only the host can create an entry into the "needs closing on dealloc" hashmap!
    handleOrFd : Nat,

    # TODO update everything below this line to reflect that now we do our own buffering,
    # and the buffer length is stored on the heap at the beginning of the buffer allocation
    # (right after the refcount). Buffers for stdio are statically allocated, but buffers for
    # other files are allocated per-open. Windows has strict alignment requirements for
    # buffers, so we make sure to account for those when giving roc_alloc its alignment arg
    # on Windows. We want to do this allocation ourselves because we need an allocation anyway
    # in order to do the reference counting, and having libc do a separate allocation for
    # buffering would be redundant.
    # ------------------------------------------------------------------------------------a
    # Standard I/O streams (stdout, stderr, stdin) are always open, so no heap
    # allocation is needed to track whether there are any references left to them.
    # Other files need closing, so we use a zero-sized Box to track the reference count.
    #
    # These buffers (and the Box, for NeedsClosingUnbuffered) are all allocated to a separate
    # arena on the heap. This way, in the host's roc_dealloc, whenever it receives a request
    # to deallocate a pointer, it checks to see if the pointer it received is within the
    # address space of that arena. If so, then that pointer should have a corresponding entry
    # in the global hashmap of pointers to file descriptors, which gets an entry inserted
    # whenever we do an `open`.
    #
    # The `Buffered` variant does not distinguish between being a buffer for stdio or not,
    # but when it's initially allocated, if it's for a stdio stream, its buffer gets
    # allocated using normal roc_alloc, and no entry is made in the file descriptor hashmap.
    # That way, it is never automatically closed (as it must not be).
    #
    # Separately, unbuffered stdio actually uses the Buffered variant - but with an empty list.
    # This way there's no heap allocation (unlike if we used Unbuffered, which always has a Box),
    # and we also don't need an extra variant causing an extra conditional branch (or jump table).
    memory : [
        # This is a (Box []) to emphasize that this should never be created outside the host,
        # because only the host can create an entry into the "needs closing on dealloc" hashmap!
        NeedsClosing (Box []),
        AlwaysOpen, # stdin, stdout, or stderr
    ],
} # has no abilities, not even Eq, because it contains a function!


## ## Opening a ReadStream

## Example:
##
##     # Stream [Read [Disk]*]
##     stream <- ReadStream.openPath (Path.fromStr "example.txt") Json.toUtf8
##     bytes <- ReadStream.read 32 # Read the next 32 bytes from the stream
##
## This has a `Metadata` effect because, like [File.exists], it tells whether a file exists on disk.
openPath :
    Path,
    fmt
    -> Task
        (ReadStream output [Read [Disk]*]*)
        (OpenFileErr *)
        [Metadata]*
    | output has Decode
    | fmt has DecodeFormat
# TODO what's the win32 call version of `open` in UNIX?

openSocket :
    SocketInfo,
    fmt
    -> Task
        (ReadStream output [Read [Socket]]*)
        (OpenSocketErr *)
        *
    | output has Decode
    | fmt has DecodeFormat

## ## Transforming Data

map : ReadStream a err fx, (a -> b) -> ReadStream b err fx
map = \@ReadStream stream, fromAtoB ->
    @ReadStream { stream &
        fromBytes: \bytes ->
            stream.fromBytes bytes
                |> Result.map fromAtoB
    }


## ## Reading Data

## Read and decode the given number of bytes. If there are any leftover bytes that couldn't
## be decoded, they will be processed on the next read.
##
## The returned Task produces a `[Done ok, More ok, Empty]` tag union.
## - `Empty` notes that the read succeeded, but there was nothing there to read.
## - `More ok` returns a successfully-decoded value, and notes that there's more left to read.
## - `Done ok` returns a successfully-decoded value, and notes that there's no more left to read. Subsequent attempts to [read] this string will produce `Empty`.
read :
    ReadStream output fx,
    Nat,
    Nat
    -> Task
        [Done output, More output, Empty]
        (ReadErr [DecodeErr Decode.Err]*)
        fx

# pread on Linux - TODO: is there a Windows version?
# Yes: SetFilePointerEx. (Nicer API than SetFilePointer.) Can be used as either pread or seek.
# Problem: the windows one only works on disk files, not e.g. sockets. But like...why would you
# want to skip over socket info? Okay, so...I guess this is the plan.

## Returns a slice of bytes within a file on disk, without advancing the stream.
## The first [Nat] is the index within the file (starting from the beginning of the file)
## and the second [Nat] is the total number of bytes to include in the slice.
byteSlice :
    ReadStream (List U8) [Read [Disk]a]b,
    Nat, # starting byte
    Nat # length
    -> Task
        [Done output, More output, Empty]
        (ReadErr *) # note: no DecodeErr here, because we don't decode!
        [Read [Disk]a]b

# Read chunks of N bytes at a time, until EOF is reached. Can build up state along the way,
# including a buffer. Cool design thing: even without seamless slices, we can hold onto a reference
# to the chunk buffer, so it won't get deallocated if you don't use it at the end of the callback.
# Then if we have a unique reference to it in the host, we can write directly into it for the
# next iteration - so, no reallocations!
readChunks :
    ReadStream ok fx
    Nat,
    state,
    (state, ok -> Task state (ReadErr [DecodeErr Decode.Err]err) fx)
    -> Task state (ReadErr [DecodeErr Decode.Err]err) fx

readChunksUntil :
    ReadStream ok err fx
    Nat,
    state,
    (state, ok -> Task [Done, Continue state] (ReadErr [DecodeErr Decode.Err]err) fx)
    -> Task state (ReadErr [DecodeErr Decode.Err]err) fx


# fdatasync on UNIX - TODO: is there a Windows equivalent?
#
# From the docs: "The aim of fdatasync() is to reduce disk activity for
# applications that do not require all metadata to be synchronized
# with the disk."
flushNonMetadata : Stream [Write a]*, Nat -> Task (WriteErr *) [Write a]*

## ## Standard I/O

## A stream to read from or write to [standard output](https://en.wikipedia.org/wiki/Standard_streams#Standard_output_(stdout))
## (`stdout`). *Reading* from `stdout` is very uncommon;
## it is much more common to write to it.
##
## The `stdout` stream is always open, so there's no need to [open] it.
stdout : Stream [Read [Stdout]*, Write [Stdout]*]

## A stream to read from or write to [standard error](https://en.wikipedia.org/wiki/Standard_streams#Standard_error_(stderr))
## (`stderr`). *Reading* from `stderr` is very uncommon;
## it is much more common to write to it.
##
## The `stderr` stream is always open, so there's no need to [open] it.
stderr : Stream [Read [Stderr]*, Write [Stderr]*]

## A stream to read from or write to [standard input](https://en.wikipedia.org/wiki/Standard_streams#Standard_input_(stdin))
## (`stdin`). *Writing* to `stdin` is very uncommon;
## it is much more common to read from it.
##
## The `stdin` stream is always open, so there's no need to [open] it.
stdin : Stream [Read [Stdin]*, Write [Stdin]*]

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

# TODO Stream needs its own distinct set of errors from File; the latter
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
