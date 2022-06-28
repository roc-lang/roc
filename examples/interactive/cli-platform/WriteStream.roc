interface WriteStream
    exposes [Stream, OpenErr, MetadataErr, StreamReadErr, StreamWriteErr, fromStr, toStr]
    imports [File.{ Metadata }]

## A stream represents a buffered [file descriptor](https://en.wikipedia.org/wiki/File_descriptor)
## on UNIX systems or a buffered [file handle](https://docs.microsoft.com/en-us/windows/win32/fileio/file-handles)
## on Windows. The buffer size is 8192 bytes.
##
## When you open a file, for example using [openRead], that file will remain open as long as
## the [Stream] is still referenced anywhere in the program. Once the program no longer has
## a reference to the [Stream], the corresponding file will be closed automatically.
WriteStream input permissions := [
    Stdio {
        # We only allow writing strings to stdio, not arbitrary bytes, because on Windows



        # TODO is this true? Can we write UTF-8 to Windows if the code page is set?
        # TODO: if this is true, how do I send e.g. Ctrl-D to stdin? Can that be done? Should it?




        # stdio is UTF-16, whereas on Linux it's UTF-8. We need to know the text encoding
        # of the bytes you're sending it, so that we can convert to the native encoding -
        # and sending a Str is the most ergonomic way to ensure that.
        toStr : input -> Str,
        type : [Stdin, Stdout, Stderr],
    },
    NeedsClosing {
        # How to translate the `input` type into a `List U8`.
        # Functions like `map` wrap this in other functions.
        toBytes : input -> List U8,

        # A stream should only ever be created in the host,
        # because only the host can create an entry into the "needs closing on dealloc" hashmap!
        #
        # A Windows HANDLE is an isize, and a UNIX file descriptor is i32.
        # A Nat will always be enough to fit either, on a 32-bit or 64-bit target.
        # On a 16-bit target, Nat will be too small, meaning that you can only have
        # ~65,000 files open at a time on your 16-bit target. That's probably way
        # more than you can have open on that target in practice anyway, so this seems fine.
        # If somehow that does turn out to be a problem, we can always make this two different
        # variants, e.g. FdNeedsClosing and HandleNeedsClosing, with Fd being I32 and Handle
        # being Nat. But for now, this would be an extra conditional and more code complexity
        # for no benefit in practice.
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
        buffer : Box [],
        # This is a (Box []) to emphasize that this should never be created outside the host,
        # because only the host can create an entry into the "needs closing on dealloc" hashmap!
    }
] # has no abilities, not even Eq, because it contains a function!

## ## Opening a WriteStream

## Example:
##
##     # Stream [Read [Disk]*]
##     stream <- WriteStream.openPath (Path.fromStr "example.txt") Json.toUtf8
##     {} <- WriteStream.append 32 # Read the next 32 bytes from the stream
##
## This has a `Metadata` effect because, like [File.exists], it tells whether a file exists on disk.
openPath :
    Path,
    encoding
    -> Task
        (WriteStream input [Write [Disk]*]*)
        (OpenFileErr *)
        [Metadata]*
    | input has Encode
    | encoding has Encoding
openPath = \path, encoding ->
    # Effect.openWrite calls open() on UNIX, CreateFile() on Windows
    Effect.openWrite path
        |> Effect.after \result -> openFromResult result encoding

## Open the given path for appending only.
openPathAppend :
    Path,
    fmt
    -> Task
        (WriteStream input [Append [Disk]]*)
        (OpenFileErr *)
        [Metadata]*
    | input has Encode
    | fmt has EncodeFormat
openPathAppend = \path, encoding ->
    # Effect.openAppend calls open() on UNIX, CreateFile() on Windows
    Effect.openAppend path
        |> Effect.after \result -> openFromResult result encoding

openSocket :
    SocketInfo,
    encoding
    -> Task
        (WriteStream input [Append [Socket]]*)
        (OpenSocketErr *)
        *
    | input has Encode
    | encoding has Encoding
openSocket = \path, encoding ->
    # Effect.openSocketAppend calls open() on UNIX, socket() on Windows
    Effect.openSocketAppend path
        |> Effect.after \result -> openFromResult result encoding

openFromResult = \result, encoding ->
    when result is
        Ok { fd, buffer } ->
            NeedsClosing {
                toBytes: \input -> Encode.encode input encoding,
                handleOrFd: fd,
                buffer,
            }
                |> @WriteStream
                |> Task.succeed

        Err err -> Task.fail err

# TODO open tempfiles - named? Unnamed? Should it be just temp dirs?

# TODO ReadDirStream? WriteDirStream? Should those just be a part of ReadStream and WriteStream somehow?
# Nah probably not.

## ## Transforming Data

####################### TODOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
# I forgot about wanting to support e.g. Stream.each or something, which
# doesn't just transform the data at each step, but also runs an effect.
# To do that we need to give it an error type. Important, though: do we
# actually want to support that? Or is this just a bunch of data transformations,
# and then at the end we let you run a task with the output? e.g. "pipe" it?
#
# Example motivating use case: progress bar in a CLI. I want to incrementally
# download and decompress a file, and then in between each step, print a status
# message. Isn't that just ReadStream.walk though? Like I walk over it, and then
# at each step I print the status update and then append to a WriteStream I
# kept in the walk's state. That seems...straightforward and a good example?
#
# Okay cool, so then WriteStream.map is just for data transformations.
#
# ALSO: what about named pipes vs anonymous pipes vs UNIX sockets etc?

map :
    WriteStream a err fx,
    (b -> a)
    -> WriteStream b err fx
map = \@WriteStream stream, fromBtoA ->
    when stream is
        Stdio rec ->
            toStr = \b -> rec.toStr (fromBtoA b)

            @WriteStream (Stdio { rec & toStr })

        NeedsClosing rec ->
            toBytes = \b -> rec.toBytes (fromBtoA b)

            @WriteStream (NeedsClosing { rec & toBytes })

# write()
## Append the input to the stream.
append : WriteStream input fx, input -> Task (StreamWriteErr *) fx

# This is only necessary if we don't have seamless slices.
# First argument: start index; second argument: length
appendSlice : WriteStream (List U8) fx, List U8, Nat, Nat -> Task (StreamWriteErr *) fx

## Write the given bytes into an open file on disk, starting from the given byte offset.
## (The offset is from the beginning of the file, not the position of the stream.)
##
## Note that this cannot be called with a stream that's been opened in Append mode,
## because in that case Linux silently disregards the offset. Use [append] for appending.
# pwrite on UNIX - TODO how do we do the equivalent in Windows?
writeBytes :
    WriteStream (List U8) [Write [Disk]a]b,
    Nat,
    List U8
    -> Task {} (StreamWriteErr *) [Write [Disk]a]b

## Like [writeBytes], but writing from a particular slice of the given bytes.
writeByteSlice :
    WriteStream (List U8) [Write [Disk]a]b,
    Nat,
    List U8,
    Nat,
    Nat
    -> Task {} (StreamWriteErr *) [Write [Disk]a]b

# Resize to the given number of bytes - ftruncate on UNIX, ??? on Windows
# TODO: On Linux, ftruncate pads with \0 bytes if you make it bigger. Does it on Windows?
# TODO: This presumably works on files on disk; does it work on anything else?
#       e.g. presumably on stdio it blows up or something. What about sockets?
resize : Stream [Write a]*, Nat -> Task (StreamWriteErr *) [Write a]*

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
# TODO can this fail? fsync errors:
#    [EBADF]		fildes is not a valid descriptor. [not applicable]
#    [EINTR]		Its execution is interrupted by a signal. [just retry if that happens?]
#    [EINVAL]		fildes refers to a file type (e.g., a socket) that does not support this operation. [not applicable]
#    [EIO]		An I/O error occurred while reading from or writing to the file system.
## Flushes any buffered data to disk, and then flushes the OS disk cache to hardware.
## Only supports [WriteStream]s which write to disk.
flush : WriteStream * [Write [Disk]a]b -> Task {} [StreamWriteErr IoErr]* [Write [Disk]a]b

# sync() on UNIX - TODO: is there a Windows equivalent?
flushAll : Task (StreamWriteErr *) [Write a]*

## ## Standard I/O

############# TODO IPC can happen over stdin/stdout, so: use for List U8 over Str there.
#############      could offer both Str and List U8 alternatives.

writeUtf8 : Path, Str -> Task {} (WriteErr *) [Write [Disk]*]*

# NOTE: this has to be Str because Windows uses UTF-16 stdio and Linux uses UTF-8,
# so if we know it's UTF-8 strings on the Roc side, we can translate on the Windows side.
stdin : WriteStream Str [Write [Stdin]*]*
stdin = @WriteStream (Stdio { type: Stdin, toStr: \str -> str })

stdout : WriteStream Str [Write [Stdout]*]*
stdout = @WriteStream (Stdio { type: Stdout, toStr: \str -> str })

stderr : WriteStream Str [Write [Stderr]*]*
stderr = @WriteStream (Stdio { type: Stderr, toStr: \str -> str })

stdinAppend : WriteStream Str [Append [Stdin]*]*
stdinAppend = @WriteStream (Stdio { type: Stdin, toStr: \str -> str })

stdoutAppend : WriteStream Str [Append [Stdout]*]*
stdoutAppend = @WriteStream (Stdio { type: Stdout, toStr: \str -> str })

stderrAppend : WriteStream Str [Append [Stderr]*]*
stderrAppend = @WriteStream (Stdio { type: Stderr, toStr: \str -> str })
