interface InternalPath
    exposes [
        UnwrappedPath,
        InternalPath,
        wrap,
        unwrap,
        toBytes,
        fromArbitraryBytes,
        fromOsBytes,
    ]
    imports []

InternalPath := UnwrappedPath

UnwrappedPath : [
    # We store these separately for two reasons:
    # 1. If I'm calling an OS API, passing a path I got from the OS is definitely safe.
    #    However, passing a Path I got from a RocStr might be unsafe; it may contain \0
    #    characters, which would result in the operation happening on a totally different
    #    path. As such, we need to check for \0s and fail without calling the OS API if we
    #    find one in the path.
    # 2. If I'm converting the Path to a Str, doing that conversion on a Path that was
    #    created from a RocStr needs no further processing. However, if it came from the OS,
    #    then we need to know what charset to assume it had, in order to decode it properly.
    # These come from the OS (e.g. when reading a directory, calling `canonicalize`,
    # or reading an environment variable - which, incidentally, are nul-terminated),
    # so we know they are both nul-terminated and do not contain interior nuls.
    # As such, they can be passed directly to OS APIs.
    #
    # Note that the nul terminator byte is right after the end of the length (into the
    # unused capacity), so this can both be compared directly to other `List U8`s that
    # aren't nul-terminated, while also being able to be passed directly to OS APIs.
    FromOperatingSystem (List U8),

    # These come from userspace (e.g. Path.fromBytes), so they need to be checked for interior
    # nuls and then nul-terminated before the host can pass them to OS APIs.
    ArbitraryBytes (List U8),

    # This was created as a RocStr, so it might have interior nul bytes but it's definitely UTF-8.
    # That means we can `toStr` it trivially, but have to validate before sending it to OS
    # APIs that expect a nul-terminated `char*`.
    #
    # Note that both UNIX and Windows APIs will accept UTF-8, because on Windows the host calls
    # `_setmbcp(_MB_CP_UTF8);` to set the process's Code Page to UTF-8 before doing anything else.
    # See https://docs.microsoft.com/en-us/windows/apps/design/globalizing/use-utf8-code-page#-a-vs--w-apis
    # and https://docs.microsoft.com/en-us/cpp/c-runtime-library/reference/setmbcp?view=msvc-170
    # for more details on the UTF-8 Code Page in Windows.
    FromStr Str,
]

wrap : UnwrappedPath -> InternalPath
wrap = @InternalPath

unwrap : InternalPath -> UnwrappedPath
unwrap = \@InternalPath raw -> raw

## TODO do this in the host, and iterate over the Str
## bytes when possible instead of always converting to
## a heap-allocated List.
toBytes : InternalPath -> List U8
toBytes = \@InternalPath path ->
    when path is
        FromOperatingSystem bytes -> bytes
        ArbitraryBytes bytes -> bytes
        FromStr str -> Str.toUtf8 str

fromArbitraryBytes : List U8 -> InternalPath
fromArbitraryBytes = \bytes ->
    @InternalPath (ArbitraryBytes bytes)

fromOsBytes : List U8 -> InternalPath
fromOsBytes = \bytes ->
    @InternalPath (FromOperatingSystem bytes)
