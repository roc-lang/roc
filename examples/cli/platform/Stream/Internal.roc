interface Stream.Internal
    exposes [
        Stream,
        Raw,
        fromNum,
        toNum,
    ]
    imports [ fx.Fd ]

## fd is a file descriptor on UNIX systems and HFILE on Windows.
##
## offset is a number of bytes from the beginning of the file; this will be
## passed to pread64 and pwrite64 so that we're never actually calling lseek64
Raw : { fd : Fd, offset : U64 }


Stream : [ @Stream Raw ]

fromRaw : Raw -> Stream
fromRaw = \raw -> @Stream raw

toRaw : Stream -> Raw
toRaw = \@Stream raw -> raw
