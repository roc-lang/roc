interface Path
    exposes [Path, fromStr, toStr]
    imports []

Path := [
    # Modern Windows uses "widechar" (UTF-16) paths
    FromWindows (List U16),
    # UNIX uses UTF-8 paths, but they may not contain \0 bytes, whereas Roc strings can.
    FromUnix (List U8),
    # Created from a Roc Str, so might contain \0 bytes.
    FromStr Str,
]

fromStr : Str -> Path
fromStr = \str ->
    @Path (FromStr str)

toStr : Path -> Result Str (Str.Utf8Err *)
toStr = \@Path raw ->
    when raw is
        FromWindows utf16 -> Str.fromUtf16 utf16
        FromUnix utf8 -> Str.fromUtf8 utf8
        FromStr str -> str
