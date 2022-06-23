interface File
    exposes [Metadata, MetadataErr, ReadErr, WriteErr]
    imports []

Metadata : {}

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
