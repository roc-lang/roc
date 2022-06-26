interface Path
    exposes [Path, PathComponent, WindowsRoot, toComponents, walkComponents]
    imports [Locale, CharsetErr]

Path : Str

## ## Canonicalizing

## Comparing canonical paths is typically more reliable than comparing raw ones.
## For example, `"foo/bar/../baz" == "foo/baz"` will return `False`, because those are different
## paths even though the canonical versions of both would be equal.
##
## Note that canonicalization reads from the file system (in order to resolve symbolic
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

# Both UNIX and Windows paths will be UTF-8, because on Windows the host calls
# `_setmbcp(_MB_CP_UTF8);` to set the process's Code Page to UTF-8 before doing anything else.
# See https://docs.microsoft.com/en-us/windows/apps/design/globalizing/use-utf8-code-page#-a-vs--w-apis
# and https://docs.microsoft.com/en-us/cpp/c-runtime-library/reference/setmbcp?view=msvc-170
# for more details on the UTF-8 Code Page in Windows.
#
# Note that if type == Raw, this str may contain \0 characters, which means that before
# we can use this as a char* in the host (either Windows or UNIX), we have to verify that
# it has no \0 characters.
##
## Returns an effect type of `[Metadata, Cwd]` because it can resolve symbolic links
## and can access the current working directory by turning a relative path into an
## absolute one (which can prepend the absolute path of the current working directory to
## the relative path).
canonicalize : Path -> Task Path (CanonicalizeErr *) [Metadata, Cwd]*

## Returns `True` if the path is absolute.
##
## A path is absolute only if it begins with an absolute root (see [PathRoot] for examples
## of roots) and it also contains no `..` or `.` path components.
##
## Note that an absolute path may contain unresolved symlinks, so it may not be canonical!

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
