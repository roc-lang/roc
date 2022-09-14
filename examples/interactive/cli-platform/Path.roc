interface Path
    exposes [
        Path,
        PathComponent,
        CanonicalizeErr,
        WindowsRoot,
        # toComponents,
        # walkComponents,
        display,
        fromStr,
        fromBytes,
        withExtension,
    ]
    imports [InternalPath.{ InternalPath }]

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
Path : InternalPath

CanonicalizeErr a : [
    PathCanonicalizeErr {},
]a

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
fromStr = \str ->
    FromStr str
    |> InternalPath.wrap

## Not all filesystems use Unicode paths. This function can be used to create a path which
## is not valid Unicode (like a Roc [Str] is), but which is valid for a particular filesystem.
##
## Note that if the list contains any `0` bytes, sending this path to any file operations
## (e.g. `File.read` or `WriteStream.openPath`) will fail.
fromBytes : List U8 -> Path
fromBytes = \bytes ->
    ArbitraryBytes bytes
    |> InternalPath.wrap

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
# canonicalize : Path -> Task Path (CanonicalizeErr *) [Metadata, Read [Env]]*
## Unfortunately, operating system paths do not include information about which charset
## they were originally encoded with. It's most common (but not guaranteed) that they will
## have been encoded with the same charset as the operating system's curent locale (which
## typically does not change after it is set during installation of the OS), so
## this should convert a [Path] to a valid string as long as the path was created
## with the given `Charset`. (Use `Env.charset` to get the current system charset.)
##
## For a conversion to [Str] that is lossy but does not return a [Result], see
## [display].
# toInner : Path -> [Str Str, Bytes (List U8)]
## Assumes a path is encoded as [UTF-8](https://en.wikipedia.org/wiki/UTF-8),
## and converts it to a string using `Str.display`.
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
## which means when [display] converts them to a string, the string may include gibberish.
## [Here is an example.](https://unix.stackexchange.com/questions/667652/can-a-file-path-be-invalid-utf-8/667863#667863)
##
## If you happen to know the `Charset` that was used to encode the path, you can use
## `toStrUsingCharset` instead of [display].
display : Path -> Str
display = \path ->
    when InternalPath.unwrap path is
        FromStr str -> str
        FromOperatingSystem bytes | ArbitraryBytes bytes ->
            when Str.fromUtf8 bytes is
                Ok str -> str
                # TODO: this should use the builtin Str.display to display invalid UTF-8 chars in just the right spots, but that does not exist yet!
                Err _ -> "�"

# isEq : Path, Path -> Bool
# isEq = \p1, p2 ->
#     when InternalPath.unwrap p1 is
#         FromOperatingSystem bytes1 | ArbitraryBytes bytes1 ->
#             when InternalPath.unwrap p2 is
#                 FromOperatingSystem bytes2 | ArbitraryBytes bytes2 -> bytes1 == bytes2
#                 # We can't know the encoding that was originally used in the path, so we convert
#                 # the string to bytes and see if those bytes are equal to the path's bytes.
#                 #
#                 # This may sound unreliable, but it's how all paths are compared; since the OS
#                 # doesn't record which encoding was used to encode the path name, the only
#                 # reasonable# definition for path equality is byte-for-byte equality.
#                 FromStr str2 -> Str.isEqUtf8 str2 bytes1
#         FromStr str1 ->
#             when InternalPath.unwrap p2 is
#                 FromOperatingSystem bytes2 | ArbitraryBytes bytes2 -> Str.isEqUtf8 str1 bytes2
#                 FromStr str2 -> str1 == str2
# compare : Path, Path -> [Lt, Eq, Gt]
# compare = \p1, p2 ->
#     when InternalPath.unwrap p1 is
#         FromOperatingSystem bytes1 | ArbitraryBytes bytes1 ->
#             when InternalPath.unwrap p2 is
#                 FromOperatingSystem bytes2 | ArbitraryBytes bytes2 -> Ord.compare bytes1 bytes2
#                 FromStr str2 -> Str.compareUtf8 str2 bytes1 |> Ord.reverse
#         FromStr str1 ->
#             when InternalPath.unwrap p2 is
#                 FromOperatingSystem bytes2 | ArbitraryBytes bytes2 -> Str.compareUtf8 str1 bytes2
#                 FromStr str2 -> Ord.compare str1 str2
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

## Note that a root of Slash (`/`) has different meanings on UNIX and on Windows.
## * On UNIX, `/` at the beginning of the path refers to the filesystem root, and means the path is absolute.
## * On Windows, `/` at the beginning of the path refers to the current disk drive, and means the path is relative.
# PathRoot : [
#     WindowsSpecificRoot WindowsRoot, # e.g. "C:" on Windows
#     Slash,
#     None,
# ]
# TODO see https://doc.rust-lang.org/std/path/enum.Prefix.html
WindowsRoot : []

## Returns the root of the path.
# root : Path -> PathRoot
# components : Path -> { root : PathRoot, components : List PathComponent }
## Walk over the path's [components].
# walk :
#     Path,
#     # None means it's a relative path
#     (PathRoot -> state),
#     (state, PathComponent -> state)
#     -> state
## Returns the path without its last [`component`](#components).
##
## If the path was empty or contained only a [root](#PathRoot), returns the original path.
# dropLast : Path -> Path
# TODO see https://doc.rust-lang.org/std/path/struct.Path.html#method.join for
# the definition of the term "adjoin" - should we use that term?
# append : Path, Path -> Path
# append = \prefix, suffix ->
#     content =
#         when InternalPath.unwrap prefix is
#             FromOperatingSystem prefixBytes ->
#                 when InternalPath.unwrap suffix is
#                     FromOperatingSystem suffixBytes ->
#                         # Neither prefix nor suffix had interior nuls, so the answer won't either
#                         List.concat prefixBytes suffixBytes
#                         |> FromOperatingSystem
#                     ArbitraryBytes suffixBytes ->
#                         List.concat prefixBytes suffixBytes
#                         |> ArbitraryBytes
#                     FromStr suffixStr ->
#                         # Append suffixStr by writing it to the end of prefixBytes
#                         Str.appendToUtf8 suffixStr prefixBytes (List.len prefixBytes)
#                         |> ArbitraryBytes
#             ArbitraryBytes prefixBytes ->
#                 when InternalPath.unwrap suffix is
#                     ArbitraryBytes suffixBytes | FromOperatingSystem suffixBytes ->
#                         List.concat prefixBytes suffixBytes
#                         |> ArbitraryBytes
#                     FromStr suffixStr ->
#                         # Append suffixStr by writing it to the end of prefixBytes
#                         Str.writeUtf8 suffixStr prefixBytes (List.len prefixBytes)
#                         |> ArbitraryBytes
#             FromStr prefixStr ->
#                 when InternalPath.unwrap suffix is
#                     ArbitraryBytes suffixBytes | FromOperatingSystem suffixBytes ->
#                         List.concat suffixBytes (Str.toUtf8 prefixStr)
#                         |> ArbitraryBytes
#                     FromStr suffixStr ->
#                         Str.concat prefixStr suffixStr
#                         |> FromStr
#     InternalPath.wrap content
# appendStr : Path, Str -> Path
# appendStr = \prefix, suffixStr ->
#     content =
#         when InternalPath.unwrap prefix is
#             FromOperatingSystem prefixBytes | ArbitraryBytes prefixBytes ->
#                 # Append suffixStr by writing it to the end of prefixBytes
#                 Str.writeUtf8 suffixStr prefixBytes (List.len prefixBytes)
#                 |> ArbitraryBytes
#             FromStr prefixStr ->
#                 Str.concat prefixStr suffixStr
#                 |> FromStr
#     InternalPath.wrap content
## Returns `True` if the first path begins with the second.
# startsWith : Path, Path -> Bool
# startsWith = \path, prefix ->
#     when InternalPath.unwrap path is
#         FromOperatingSystem pathBytes | ArbitraryBytes pathBytes ->
#             when InternalPath.unwrap prefix is
#                 FromOperatingSystem prefixBytes | ArbitraryBytes prefixBytes ->
#                     List.startsWith pathBytes prefixBytes
#                 FromStr prefixStr ->
#                     strLen = Str.countUtf8Bytes prefixStr
#                     if strLen == List.len pathBytes then
#                         # Grab the first N bytes of the list, where N = byte length of string.
#                         bytesPrefix = List.takeAt pathBytes 0 strLen
#                         # Compare the two for equality.
#                         Str.isEqUtf8 prefixStr bytesPrefix
#                     else
#                         False
#         FromStr pathStr ->
#             when InternalPath.unwrap prefix is
#                 FromOperatingSystem prefixBytes | ArbitraryBytes prefixBytes ->
#                     Str.startsWithUtf8 pathStr prefixBytes
#                 FromStr prefixStr ->
#                     Str.startsWith pathStr prefixStr
## Returns `True` if the first path ends with the second.
# endsWith : Path, Path -> Bool
# endsWith = \path, prefix ->
#     when InternalPath.unwrap path is
#         FromOperatingSystem pathBytes | ArbitraryBytes pathBytes ->
#             when InternalPath.unwrap suffix is
#                 FromOperatingSystem suffixBytes | ArbitraryBytes suffixBytes ->
#                     List.endsWith pathBytes suffixBytes
#                 FromStr suffixStr ->
#                     strLen = Str.countUtf8Bytes suffixStr
#                     if strLen == List.len pathBytes then
#                         # Grab the last N bytes of the list, where N = byte length of string.
#                         bytesSuffix = List.takeAt pathBytes (strLen - 1) strLen
#                         # Compare the two for equality.
#                         Str.startsWithUtf8 suffixStr bytesSuffix
#                     else
#                         False
#         FromStr pathStr ->
#             when InternalPath.unwrap suffix is
#                 FromOperatingSystem suffixBytes | ArbitraryBytes suffixBytes ->
#                     Str.endsWithUtf8 pathStr suffixBytes
#                 FromStr suffixStr ->
#                     Str.endsWith pathStr suffixStr
# TODO https://doc.rust-lang.org/std/path/struct.Path.html#method.strip_prefix
# TODO idea: what if it's File.openRead and File.openWrite? And then e.g. File.metadata,
# File.isDir, etc.
## If the last component of this path has no `.`, appends `.` followed by the given string.
## Otherwise, replaces everything after the last `.` with the given string.
##
## Examples:
##
##     Path.fromStr "foo/bar/baz" |> Path.withExtension "txt" #    foo/bar/baz.txt
##     Path.fromStr "foo/bar/baz." |> Path.withExtension "txt" #   foo/bar/baz.txt
##     Path.fromStr "foo/bar/baz.xz" |> Path.withExtension "txt" # foo/bar/baz.txt
withExtension : Path, Str -> Path
withExtension = \path, extension ->
    when InternalPath.unwrap path is
        FromOperatingSystem bytes | ArbitraryBytes bytes ->
            beforeDot =
                when List.splitLast bytes (Num.toU8 '.') is
                    Ok { before } -> before
                    Err NotFound -> bytes

            beforeDot
            |> List.reserve (1 + Str.countUtf8Bytes extension)
            |> List.append (Num.toU8 '.')
            |> List.concat (Str.toUtf8 extension)
            |> ArbitraryBytes
            |> InternalPath.wrap

        FromStr str ->
            beforeDot =
                when Str.splitLast str "." is
                    Ok { before } -> before
                    Err NotFound -> str

            beforeDot
            |> Str.reserve (1 + Str.countUtf8Bytes extension)
            |> Str.concat "."
            |> Str.concat extension
            |> FromStr
            |> InternalPath.wrap

# NOTE: no withExtensionBytes because it's too narrow. If you really need to get some
# non-Unicode in there, do it with
