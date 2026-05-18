app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Test that primitive types have encode methods for static dispatch encoding.
# This matches the pattern established for Str.encode and List.encode.

# Define a simple format type that converts to UTF-8 bytes.
# This format type provides encode methods for various types.
# We use [] (empty tag union) as the wrapped type since it has a unique value.
Utf8Fmt := [Fmt].{
    encode_bool : Utf8Fmt, Bool -> Try(List(U8), [])
    encode_bool = |_fmt, b| {
        if b {
            Ok([116, 114, 117, 101])  # "true" as bytes
        } else {
            Ok([102, 97, 108, 115, 101])  # "false" as bytes
        }
    }

    encode_u64 : Utf8Fmt, U64 -> Try(List(U8), [])
    encode_u64 = |_fmt, n| {
        Ok(n.to_str().to_utf8())
    }

    encode_str : Utf8Fmt, Str -> Try(List(U8), [])
    encode_str = |_fmt, s| {
        Ok(s.to_utf8())
    }
}

main! = || {
    fmt : Utf8Fmt
    fmt = Fmt

    # Test Bool.encode
    my_bool : Bool
    my_bool = True
    bool_bytes = my_bool.encode(fmt).ok_or([])
    Stdout.line!("Bool.encode(True): ${Str.from_utf8_lossy(bool_bytes)}")

    # Test U64.encode
    num : U64
    num = 42
    num_bytes = num.encode(fmt).ok_or([])
    Stdout.line!("U64.encode(42): ${Str.from_utf8_lossy(num_bytes)}")

    Stdout.line!("Done")
}
