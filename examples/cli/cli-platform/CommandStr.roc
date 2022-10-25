interface CommandStr
    exposes [
        CommandStr,
        BytesError,
        display,
    ] imports []

CommandStr := [FromStr Str, FromOperatingSystem (List U8)]

BytesError : [
    InvalidCommandStr { reason : Str },
]

# Prints a human-readable representation of the command string decoded as UTF-8.
# If the decoding is lossy - bytes that cannot be faithfully decoded as UTF-8 will be
# substitued by the U+FFFD replacement character.
display : CommandStr -> Str
