interface Path
    exposes [Path, PathComponent, WindowsRoot, toComponents, walkComponents]
    imports [Locale, CharsetErr]

## There are two types of paths:
## * _Canonical_ paths have the type `Path [Canonical]`. They are absolute paths (so, no ".." or "." path components) and have all symlinks resolved.
## * _Raw_ paths have the type `Path [Raw]`. They come from Roc strings. They may be absolute or relative, they may contain symlinks, and the operating system may not consider them syntactically valid paths.
##
## A [Path] can be either of these, depending on its type parameter. For convenience,
## there are the type aliases `RawPath : Path [Raw]` and `CanPath : Path [Canonical]`.
##
## File operations (such as reading from a file) will typically accept either type of [Path],
## but some operations in this module work differently with one or the other. For example,
## [rootRaw] sometimes returns `None` because the path might be relative, whereas [rootCan]
## always returns a root because it's guaranteed to be an absolute path, which must have a root.
##
## You can get a [RawPath] from a [Str] using [Path.fromStr], and you can convert it to
## a canonical path using [Path.canonicalize].
##
## Note that comparing canonical paths is typically more reliable than comparing raw ones.
## For example, `Path.fromStr "foo/bar/../baz" == Path.fromStr "foo/baz"` will return `False`,
## because those are different [RawPath]s even though they would [canonicalize] to the same
## [CanPath]s.
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
Path canonical := [
    # We store these separately for two reasons:
    # 1. If I'm calling an OS API, passing a path I got from the OS is definitely safe.
    #    However, passing a Path I got from a RocStr might be unsafe; it may contain \0
    #    characters, which would result in the operation happening on a totally different
    #    path. As such, we need to check for \0s and fail without calling the OS API if we
    #    find one in the path.
    # 2. If I'm converting the Path to a Str, doing that conversion on a Path that was
    #    created from a RocStr needs no further processing. However, if it came from the OS,
    #    then we need to know what charset to assume it had, in order to decode it properly.
    FromOs : List U8,
    FromRoc : Str,
]
# Both UNIX and Windows paths will be UTF-8, because on Windows the host calls
# `_setmbcp(_MB_CP_UTF8);` to set the process's Code Page to UTF-8 before doing anything else.
# See https://docs.microsoft.com/en-us/windows/apps/design/globalizing/use-utf8-code-page#-a-vs--w-apis
# and https://docs.microsoft.com/en-us/cpp/c-runtime-library/reference/setmbcp?view=msvc-170
# for more details on the UTF-8 Code Page in Windows.
#
# Note that if type == Raw, this str may contain \0 characters, which means that before
# we can use this as a char* in the host (either Windows or UNIX), we have to verify that
# it has no \0 characters.

CanPath : Path [Canonical]

RawPath : Path [Raw]

## ## Creating and transforming

fromStr : Str -> RawPath
fromStr = \str -> @Path (FromRoc str)

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
canonicalize : Path * -> Task CanPath (CanonicalizeErr *) [Metadata, Cwd]*

## Assumes the given [Path] is encoded in the given [Charset], and attempts to decode
## it as such. This decoding can fail if the [Path] is not in the given [Charset].
##
## Unfortunately, operating system paths do not include information about which charset
## they were originally encoded with. It's most common (but not guaranteed) that they will
## have been encoded with the same charset as the operating system's curent locale (which
## typically does not change after it is set during installation of the OS), so
## [Locale.displayPath] should convert a [Path] to a valid string as long as the path
## was created in the given [Locale]. (Use [Env.locale] to get the current locale.)
toStr : Path *, Locale -> Result Str (CharsetErr *)
toStr = \@Path path, locale ->
    when path is
        FromOs bytes -> Locale.toStr locale bytes
        FromRoc str -> str

## Assumes the path was encoded using the same locale as the one returned by
## [Env.locale], and converts it to a [Str] while converting any characters which are invalid
## in that locale into the [Unicode replacement character](https://unicode.org/glossary/#replacement_character)
## (`"�"`).
##
## TODO: since this will be commonly used, explain again here about what the problem is
##       and what edge cases can occur as a result.
display : Path * -> Task Str * [Read [Env]]*
display = \@Path path ->
    when path is
        FromOs bytes -> Env.locale |> Task.map Locale.display
        FromRoc str -> Task.succeed str

## Converts a path that we know something about (e.g. [CanPath] or [RawPath]) into
## a path that we know nothing about.
##
## This can be useful for operations which require two paths of the same type. For example,
## we can't pass [Bool.isEq] a [CanPath] and a [RawPath] because those aren't the same type.
generalize : Path * -> Path *
generalize = \@Path unwrapped -> @Path unwrapped

isEq : Path a, Path a -> Bool
isEq = @Path p1, @Path p2 ->
    when (p1, p2) is
        (FromOs bytes, FromOs bytes) -> bytes == bytes,
        (FromRoc str, FromRoc str) -> str == str,
        (FromOs bytes, FromRoc str)
        | (FromRoc str, FromOs bytes) ->
            # We can't know the encoding that was originally used in the path, so we convert
            # the string to bytes and see if those bytes are equal to the path's bytes.
            #
            # This may sound unreliable, but it's how all paths are compared; since the OS
            # doesn't record which encoding was used to encode the path name, the only
            # reasonable# definition for path equality is byte-for-byte equality.
            Str.isEqUtf8 str bytes

## Returns `True` if the path is absolute.
##
## For a [CanPath], this is always `True`.
##
## For a [RawPath], this returns [True] only if the path begins with an absolute root
## (see [CanPathRoot] for examples of roots) and it also contains no `..` or `.` path
## components.
##
## Note that an absolute path may contain unresolved symlinks, so it may not be canonical!
isAbsolute : Path * -> Bool

## ## Path Components

# ParentDir, # e.g. ".." on UNIX or Windows
# CurrentDir, # e.g. "." on UNIX
PathComponent : [ParentDir, CurrentDir]CanPathComponent

PathRoot : [
    WindowsRoot WindowsRoot, # e.g. "C:" on Windows
    UnixRoot, # "/" on UNIX
]

CanPathComponent : [
    Named Str, # e.g. "stuff" on UNIX
    DirSep Str, # e.g. "/" on UNIX, "\" or "/" on Windows.
                # This is included as an option so if you're transforming part of a path,
                # you can write back whatever separator was originally used.
]

WindowsRoot : [
    # TODO see https://doc.rust-lang.org/std/path/enum.Root.html
]

# None means it's a relative path
root : Path * -> [None]PathRoot

# None means it's a relative path
toComponents : Path * -> ([None]PathRoot, List PathComponent)

walkComponents :
    Path *,
    # None means it's a relative path
    ([None]PathRoot -> state),
    (state, PathComponent -> state)
    -> state
