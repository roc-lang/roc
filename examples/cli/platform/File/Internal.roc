interface File.Internal
    exposes [
        Stream,
        Raw,
        fromNum,
        toNum,
    ]
    imports [ fx.Fd ]

## fd is a file descriptor on UNIX systems and HFILE on Windows. Either way
## it's an i32.
##
## Path is the string that was originally provided when the file was opened.
## It's used in error messages, so you know which path the error came from.
Raw : { path : Str, fd : Fd }


Stream a : [ @Stream Raw ]a

fromRaw : Raw -> Stream *
fromRaw = \raw -> @Stream raw

toRaw : Stream * -> Raw
toRaw = \@Stream raw -> raw
