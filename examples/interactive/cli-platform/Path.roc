interface Path
    exposes [Path, fromStr, toStr]
    imports []

Path := [
    # Modern Windows uses "widechar" (UTF-16) paths
    FromWindows (List U16),
    # Created from a Roc Str, so might contain \0 bytes.
    # When sending these to OS functions, we have to validate that they have no \0 bytes,
    # as otherwise we might end up doing I/O on the wrong file - a security concern as
    # well as a correctness concern.
    FromStr Str,
    # UNIX uses UTF-8 paths, but they may not contain \0 bytes, whereas Roc strings may.
    # So we don't need to check these for \0 bytes when sending them to OS functions,
    # like we do for
    FromUnix Str,
]

isEq : Path, Path -> Bool
isEq = \p1, p2 ->
    when (p1, p2) is
        # Compatible representations
        (FromWindows data1, FromWindows data2) -> data1 == data2
        (FromStr str1, FromStr str2)
        | (FromUnix str1, FromUnix str2)
        | (FromUnix str1, FromStr str2)
        | (FromStr str1, FromUnix str2) -> str1 == str2
        # Incompatible representations
        (FromWindows utf16, FromUnix str)
        | (FromWindows utf16, FromStr str)
        | (FromUnix str, FromWindows utf16)
        | (FromStr str, FromWindows utf16) -> isEqUtf8Utf16 (Str.toUtf8 str) utf16

isEqUtf8Utf16 : List U8, List U16 -> Bool
isEqUtf8Utf16 = \utf8, utf16 ->
    Str.walkCodePoints utf8

fromStr : Str -> Path
fromStr = \str ->
    @Path (FromStr str)

toStr : Path -> Result Str (Str.Utf8Err *)
toStr = \@Path raw ->
    when raw is
        FromWindows utf16 -> Str.fromUtf16 utf16
        FromUnix utf8 -> Str.fromUtf8 utf8
        FromStr str -> str
