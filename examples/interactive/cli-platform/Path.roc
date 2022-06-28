interface Path
    exposes [Path, PathComponent, WindowsRoot, toComponents, walkComponents]
    imports [Locale, CharsetErr]

## You can canonicalize a [Path] using [Path.canonicalize].
##
## Comparing canonical paths is often more reliable than comparing raw ones.
## For example, `Path.fromStr "foo/bar/../baz" == Path.fromStr "foo/baz"` will return `False`,
## because those are different paths even though their canonical equivalents would be equal.
##
## Also note that canonicalization reads from the file system (in order to resolve symbolic
## links, and to convert relative paths into absolute ones). This means that it is not only
## a [Task] (which can fail), but also that running [canonicalize] on the same [Path] twice
## may give different answers. An example of a way this could happen is if a symbolic link
## in the path changed on disk to point somewhere else in between the two [canonicalize] calls.
##
## Similarly, remember that canonical paths are not guaranteed to refer to a valid file. They
## might have referred to one when they were canonicalized, but that file may have moved or
## been deleted since the canonical path was created. So you might [canonicalize] a [Path],
## and then immediately use that [Path] to read a file from disk, and still get back an error
## because something relevant changed on the filesystem between the two operations.
##
## Also note that different filesystems have different rules for syntactically valid paths.
## Suppose you're on a machine with two disks, one formatted as ext4 and another as FAT32.
## It's possible to list the contents of a directory on the ext4 disk, and get a [CanPath] which
## is valid on that disk, but invalid on the other disk. One way this could happen is if the
## directory on the ext4 disk has a filename containing a `:` in it. `:` is allowed in ext4
## paths but is considered invalid in FAT32 paths.
Path := [
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
    NulTerminated : List U8,

    # These come from userspace (e.g. Path.fromBytes), so they need to be checked for interior
    # nuls and then nul-terminated before the host can pass them to OS APIs.
    ArbitraryBytes : List U8,

    # This was created as a RocStr, so it might have interior nul bytes but it's definitely UTF-8.
    # That means we can `toStr` it trivially, but have to validate before sending it to OS
    # APIs that expect a nul-terminated `char*`.
    #
    # Note that both UNIX and Windows APIs will accept UTF-8, because on Windows the host calls
    # `_setmbcp(_MB_CP_UTF8);` to set the process's Code Page to UTF-8 before doing anything else.
    # See https://docs.microsoft.com/en-us/windows/apps/design/globalizing/use-utf8-code-page#-a-vs--w-apis
    # and https://docs.microsoft.com/en-us/cpp/c-runtime-library/reference/setmbcp?view=msvc-170
    # for more details on the UTF-8 Code Page in Windows.
    FromStr : Str,
]

## ## Creating and transforming

## Note that the path may not be valid depending on the filesystem where it is used.
## For example, paths containing `:` are valid on ext4 and NTFS filesystems, but not
## on FAT ones. So if you have multiple disks on the same machine, but they have
## different filesystems, then this path could be valid on one but invalid on another!
##
## It's safest to assume paths are invalid (even syntactically) until given to an operation
## which uses them to open a file. If that operation succeeds, then the path was valid
## (at the time). Otherwise, error handling can happen for that operation rather than validating
## up front for a false sense of security (given symlinks, parts of a path being renamed, etc.).
fromStr : Str -> Path
fromStr = \str -> @Path (FromStr str)

## Not all filesystems use Unicode paths. This function can be used to create a path which
## is not valid Unicode (like a Roc [Str] is), but which is valid for a particular filesystem.
##
## Note that if the list contains any `0` bytes, sending this path to any file operations
## (e.g. [File.read] or [WriteStream.openPath]) will fail.
fromBytes : List U8 -> Path
fromBytes = \bytes -> @Path (ArbitraryBytes bytes)

## Note that canonicalization reads from the file system (in order to resolve symbolic
## links, and to convert relative paths into absolute ones). This means that it is not only
## a [Task] (which can fail), but also that running [canonicalize] on the same [Path] twice
## may give different answers. An example of a way this could happen is if a symbolic link
## in the path changed on disk to point somewhere else in between the two [canonicalize] calls.
##
## Returns an effect type of `[Metadata, Cwd]` because it can resolve symbolic links
## and can access the current working directory by turning a relative path into an
## absolute one (which can prepend the absolute path of the current working directory to
## the relative path).
canonicalize : Path -> Task Path (CanonicalizeErr *) [Metadata, Read [Env]]*

## Unfortunately, operating system paths do not include information about which charset
## they were originally encoded with. It's most common (but not guaranteed) that they will
## have been encoded with the same charset as the operating system's curent locale (which
## typically does not change after it is set during installation of the OS), so
## this should convert a [Path] to a valid string as long as the path was created
## with the given [Charset]. (Use [Env.charset] to get the current system charset.)
##
## For a conversion to [Str] that is lossy but does not return a [Result], see
## [displayUtf8].
toInner : Path -> [Unicode Str, Bytes (List U8)]

## Assumes a path is encoded as [UTF-8](https://en.wikipedia.org/wiki/UTF-8),
## and converts it to a string using [Str.displayUtf8].
##
## This conversion is lossy because the path may contain invalid UTF-8 bytes. If that happens,
## any invalid bytes will be replaced with the [Unicode replacement character](https://unicode.org/glossary/#replacement_character)
## instead of returning an error. As such, it's rarely a good idea to use the [Str] returned
## by this function for any purpose other than displaying it to a user.
##
## When you don't know for sure what a path's encoding is, UTF-8 is a popular guess because
## it's the default on UNIX and also is the encoding used in Roc strings. This platform also
## automatically runs applications under the [UTF-8 code page](https://docs.microsoft.com/en-us/windows/apps/design/globalizing/use-utf8-code-page)
## on Windows.
##
## Converting paths to strings can be an unreliable operation, because operating systems
## don't record the paths' encodings. This means it's possible for the path to have been
## encoded with a different character set than UTF-8 even if UTF-8 is the system default,
## which means when [displayUtf8] converts them to a string, the string may include gibberish.
## [Here is an example.](https://unix.stackexchange.com/questions/667652/can-a-file-path-be-invalid-utf-8/667863#667863)
##
## If you happen to know the [Charset] that was used to encode the path, you can use
## [toStrUsingCharset] instead of [displayUtf8].
displayUtf8 : Path -> Str
displayUtf8 = \@Path path ->
    when path is
        FromStr str -> str
        NulTerminated bytes | ArbitraryBytes bytes ->
            Str.displayUtf8 bytes

isEq : Path, Path -> Bool
isEq = @Path p1, @Path p2 ->
    when p1 is
        NulTerminated bytes1 | ArbitraryBytes bytes1 ->
            when p2 is
                NulTerminated bytes2 | ArbitraryBytes bytes2 -> bytes1 == bytes2
                # We can't know the encoding that was originally used in the path, so we convert
                # the string to bytes and see if those bytes are equal to the path's bytes.
                #
                # This may sound unreliable, but it's how all paths are compared; since the OS
                # doesn't record which encoding was used to encode the path name, the only
                # reasonable# definition for path equality is byte-for-byte equality.
                FromStr str2 -> Str.isEqUtf8 str2 bytes1

        FromStr str1 ->
            when p2 is
                NulTerminated bytes2 | ArbitraryBytes bytes2 -> Str.isEqUtf8 str1 bytes2
                FromStr str2 -> str1 == str2

compare : Path, Path -> [Lt, Eq, Gt]
compare = @Path p1, @Path p2 ->
    when p1 is
        NulTerminated bytes1 | ArbitraryBytes bytes1 ->
            when p2 is
                NulTerminated bytes2 | ArbitraryBytes bytes2 -> Ord.compare bytes1 bytes2
                FromStr str2 -> Str.compareUtf8 str2 bytes1

        FromStr str1 ->
            when p2 is
                NulTerminated bytes2 | ArbitraryBytes bytes2 -> Ord.compare str1 bytes2
                FromStr str2 -> str1 == str2

## Returns `True` if the path is absolute.
##
## A path is only absolute if it begins with an absolute root
## (see [PathRoot] for examples of roots) _and_ it contains neither `..` nor `.` path
## components.
##
## Note that an absolute path may contain unresolved symlinks, so even an absolute path
## may change when passed to [canonicalize].
isAbsolute : Path -> Bool

## ## Path Components

PathComponent : [
    ParentDir, # e.g. ".." on UNIX or Windows
    CurrentDir, # e.g. "." on UNIX
    Named Str, # e.g. "stuff" on UNIX
    DirSep Str, # e.g. "/" on UNIX, "\" or "/" on Windows. Or, sometimes, "¥" on Windows - see
                # https://docs.microsoft.com/en-us/windows/win32/intl/character-sets-used-in-file-names
                #
                # This is included as an option so if you're transforming part of a path,
                # you can write back whatever separator was originally used.
]

PathRoot : [
    WindowsRoot WindowsRoot, # e.g. "C:" on Windows
    UnixRoot, # "/" on UNIX
    PathIsRelative, # no root
]

WindowsRoot : [
    # TODO see https://doc.rust-lang.org/std/path/enum.Root.html
]

root : Path -> PathRoot

toComponents : Path -> (PathRoot, List PathComponent)

walkComponents :
    Path,
    # None means it's a relative path
    (PathRoot -> state),
    (state, PathComponent -> state)
    -> state
