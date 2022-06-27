interface DirEntry
    exposes [DirEntry, displayPath, pathToStr]

## A [Path] can represent one of two types of path:
## * _Canonical_ paths have the type `Path [Canonical]`. They are absolute paths (so, no ".." or "." path components) and have all symlinks resolved.
## * _Unresolved_ paths have the type `Path *`. They come from Roc strings. They may be absolute or relative, and they may contain unresolved symlinks.
##
## A [Path] can be either of these, depending on its type parameter. For convenience,
## there is a type alias `CanPath : Path [Canonical]`. This way you have `Path *` for unresolved
## paths, and [CanPath] for canonical ones.
##
## File operations (such as reading from a file) will typically accept either type of [Path],
## but some operations in this module work differently with one or the other. For example,
## [rootRaw] sometimes returns `None` because the path might be relative, whereas [rootCan]
## always returns a root because it's guaranteed to be an absolute path, which must have a root.
##
## You can get a [Path] from a [Str] using [Path.fromStr], and you can convert it to
## a [CanPath] using [Path.canonicalize].
##


## Assumes the path was encoded using the same locale as the one returned by
## [Env.locale], and converts it to a [Str] while converting any characters which are invalid
## in that locale into the [Unicode replacement character](https://unicode.org/glossary/#replacement_character)
## (`"�"`).
##
## TODO: since this will be commonly used, explain again here about what the problem is
##       and what edge cases can occur as a result.
displayPath : DirEntry -> Task Str * [Read [Env]]*
displayPath = \@DirEntry entry ->
    Task.map Env.locale \locale ->
        Locale.display locale entry.path

## Assumes the given [Path] is encoded in the given [Charset], and attempts to decode
## it as such. This decoding can fail if the [Path] is not in the given [Charset].
##
## Unfortunately, operating system paths do not include information about which charset
## they were originally encoded with. It's most common (but not guaranteed) that they will
## have been encoded with the same charset as the operating system's curent locale (which
## typically does not change after it is set during installation of the OS), so
## [Locale.displayPath] should convert a [Path] to a valid string as long as the path
## was created in the given [Locale]. (Use [Env.locale] to get the current locale.)
pathToStr : DirEnty, Locale -> Result Str (CharsetErr *)
pathToStr = \@DirEntry entry, locale ->
    Locale.toStr locale entry.path # TODO move this function to DirEnty.roc
