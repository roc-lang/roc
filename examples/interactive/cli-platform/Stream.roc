interface Stream
    exposes [Stream, OpenErr, MetadataErr, ReadErr, WriteErr, fromStr, toStr]
    imports [File.{ Metadata }]

# TODO is there any use for dup() (and win32 equivalent) in this API?

    # https://man7.org/linux/man-pages/man3/setbuf.3.html
    # https://docs.microsoft.com/en-us/windows/win32/multimedia/performing-memory-file-i-o
# Unbuffered file I/O in Windows has alignment requirements that normal Roc data structures
# can't meet. So if we tried to say "give me a List U8 and I'll do an unbuffered write to disk",
# unless you miraculously got a List U8 with the exact right alignment requirements, it's
# getting copied to a buffer anyway. So we simplify things by always using buffers.
# https://docs.microsoft.com/en-us/windows/win32/fileio/file-buffering
# Actually, let's do the buffering ourselves. This way, we can avoid a double allocation;
# we need to allocate anyway in order to get a refcount, so we can just do the buffering
# ourselves using that allocation. It's technically always tracked as a (Box []) but
# in actuality it'll point to a heap allocation of fixed capacity (whatever buffer size
# we decide all Streams get), which is stateful on a per-Stream basis.
#
# Java uses this buffer size: (4096 * 2 = 8192) bytes. Seems like if we give you control
# https://stackoverflow.com/questions/13433286/optimal-buffer-size-for-reading-file-in-c
# https://stackoverflow.com/questions/236861/how-do-you-determine-the-ideal-buffer-size-when-using-fileinputstream
# https://stackoverflow.com/a/10698422
#
# We should support custom buffer sizes, and we should store them on the heap. If you provide
# a suboptimal buffer size, sorry. Maybe we say the minimum buffer size is 4096, and we round
# up to the nearest multiple of 4096, because we mmap pages in to use for the buffer. Yeah
# that seems fine! That should take care of our Windows alignment issues automatically too.
# Actually I guess we can allocate more or less to the buffer, and if you specify a low
# buffer size (e.g. 1) then that just means we're skipping the buffer all the time bc
# the length of the thing you pass is less than the capacity.
#
# Anyway, we need to store length on the heap but capacity in the struct. That's becasue
# we check struct capacity when doing a write; if you're writing more data than the capacity
# of the buffer, then we skip the buffer (and in fact truncate the buffer) and go straight
# to disk. If capacity is 0, we don't bother chasing the pointer to truncate it.
#

## A stream represents a buffered [file descriptor](https://en.wikipedia.org/wiki/File_descriptor)
## on UNIX systems or a buffered [file handle](https://docs.microsoft.com/en-us/windows/win32/fileio/file-handles)
## on Windows. The buffer size is 8192 bytes.
##
## When you open a file, for example using [openRead], that file will remain open as long as
## the [Stream] is still referenced anywhere in the program. Once the program no longer has
## a reference to the [Stream], the corresponding file will be closed automatically.
Stream permissions := {
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
} # has Eq, Hash, Ord # no Encode or Decode; you should never serialize these!

## ## Opening a Stream

## Example:
##
##     # Stream [Read [Disk]*]
##     stream <- Stream.openDiskRead 1024 (Path.fromStr "example.txt")
##     bytes <- Stream.read 32 # Read the next 32 bytes from the stream
##
## This has a `Metadata` effect because, like [File.exists], it tells whether a file exists on disk.
openDiskRead : Path -> Task (Stream [Read [Disk]*]) (OpenFileErr *) [Metadata]*
# TODO what's the win32 call version of `open` in UNIX?
openDiskWrite : Path -> Task (Stream [Write [Disk]*]) (OpenFileErr *) [Metadata]*
openDiskReadWrite : Path -> Task (Stream [Read [Disk]*, Write [Disk]*]) (OpenFileErr *) [Metadata]*

## Like [openDiskRead], but with a custom buffer size, in bytes.
##
## Pass a buffer size of 0 to get an unbuffered stream.
openDiskReadBuf : Path, Nat -> Task (Stream [Read [Disk]*]) (OpenFileErr *) [Metadata]*
openDiskWriteBuf : Path, Nat -> Task (Stream [Write [Disk]*]) (OpenFileErr *) [Metadata]*
openDiskReadWriteBuf : Path, Nat -> Task (Stream [Read [Disk]*, Write [Disk]*]) (OpenFileErr *) [Metadata]*

# https://forums.codeguru.com/showthread.php?280671-using-ReadFile()-and-WriteFile()-with-a-socket
openSocketRead : SocketInfo -> Task (Stream [Read [Socket]*]) (OpenSocketErr *) *
openSocketWrite : SocketInfo -> Task (Stream [Write [Socket]*]) (OpenSocketErr *) *
openSocketReadWrite : SocketInfo -> Task (Stream [Read [Socket]*, Write [Socket]*]) (OpenSocketErr *) *

# TODO do we want to allow persistent vs in-memory (as much as possible) tempfiles?
# What's the point in a persistent tempfile anyway? If, for debugging purposes, I want
# to switch to persistent, maybe that's an argumetn for tempfiles always having Disk?
# TODO does it make sense to open (even a named) tempfile for reading only?
## This has a `Metadata` effect because, like [File.exists], it tells whether a temporary file
## with the given name exists on disk.
# TODO does it actually do this? Can you possibly tell if a tempfile already exists this way?
openTempRead : Str -> Task (Stream [Read [Disk]*]) (OpenFileErr *) [Metadata]*
# TODO what are the win32 and Linux calls for tempfiles?
## This has a `Write [Disk]` effect because it can create a new temporary file on disk.
openTempWrite : Str -> Task (Stream [Write [Disk]*]) (OpenFileErr *) [Write [Disk]]*
openTempReadWrite : Str -> Task (Stream [Read [Disk]*, Write [Disk]*]) (OpenFileErr *) [Metadata]*

## Open an in-memory file for both reading and writing. (There's no point in opening an
## in-memory file for reading but not writing.)
##
## Note that if the system runs out of available memoory, the operating system may temporarily
## write this "in-memory" file to disk.
# https://stackoverflow.com/a/50087392
openMemFile : Task (Stream [Read [MemFile]*, Write [MemFile]*]) (OpenFileErr *) *

## ## File I/O

# Attempt to read the given number of bytes. There may not have been enough bytes, in which
# case the List U8 will have a length lower than Nat.
read : Stream [Read a]*, Nat -> Task (List U8) (ReadErr *) [Read a]*

# pread on Linux - TODO: is there an equivalent on Windows?
readAt : Stream [Read a]*, { bytes : Nat, offset : Nat } -> Task (List U8) (ReadErr *) [Read a]*

# Technically this means you can read the metadata of stdin/stdout/stderr - which I
# suppose is well-defined and harmless, but then again it might be a mistake. You
# can always special-case those if you have a code path that tries to read metadata,
# by explicitly doing an == check to see if what you have happens to be. Likewise
# with in-memory "files," if they aren't also well-defined. But I suppose you might want
# to know e.g. when the in-memory file was created?
# Also note: technically you don't need read permission on unix to access metadata. What about
# windows though?
metadata : Stream [Read a]* -> Task Metadata (ReadErr *) [Metadata a]*

# write()
write : Stream [Write a]*, List U8 -> Task (WriteErr *) [Write a]*
writeUtf8 : Stream [Write a]*, Str -> Task (WriteErr *) [Write a]*

# pwrite on UNIX - TODO is there an equivalent in Windows?
writeAt : Stream [Write a]*, List U8, Nat -> Task (WriteErr *) [Write a]*
writeUtf8At : Stream [Write a]*, Str, Nat -> Task (WriteErr *) [Write a]*

# Resize to the given number of bytes - ftruncate on UNIX, ??? on Windows
# TODO: On Linux, ftruncate pads with \0 bytes if you make it bigger. Does it on Windows?
resize : Stream [Write a]*, Nat -> Task (WriteErr *) [Write a]*

# fsync on UNIX, or maybe syncfs? What's the difference?
# windows Equivalent: https://docs.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-flushfilebuffers
#
# Nice article on disk caches and flushing https://docs.microsoft.com/en-us/windows/win32/fileio/file-caching
#
# NOTE: from https://man7.org/linux/man-pages/man3/stdio.3.html
#     In cases where a large amount of computation is
#     done after printing part of a line on an output terminal, it is
#     necessary to fflush(3) the standard output before going off and
#     computing so that the output will appear.
# So this means when doing writes to stdout/stderr, may need to flush them explicitly.
# ALSO means, when doing Stdout.line, should always flush right after I suppose.
## Flushes any buffered data to disk, and then flushes the disk cache to hardware.
flush : Stream [Write a]*, Nat -> Task (WriteErr *) [Write a]*

# sync() on UNIX - TODO: is there a Windows equivalent?
flushAll : Task (WriteErr *) [Write a]*

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
