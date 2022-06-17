#[cfg(feature = "gen-llvm")]
use crate::helpers::llvm::assert_evals_to;

#[cfg(feature = "gen-dev")]
use crate::helpers::dev::assert_evals_to;

#[cfg(feature = "gen-wasm")]
use crate::helpers::wasm::assert_evals_to;

#[cfg(test)]
use indoc::indoc;

#[cfg(all(test, feature = "gen-llvm"))]
use roc_std::RocList;
#[cfg(all(test, feature = "gen-llvm"))]
use roc_std::RocStr;

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn hash_specialization() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            Hash has
                hash : a -> U64 | a has Hash

            Id := U64

            hash = \@Id n -> n

            main = hash (@Id 1234)
            "#
        ),
        1234,
        u64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn hash_specialization_multiple_add() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            Hash has
                hash : a -> U64 | a has Hash

            Id := U64

            hash = \@Id n -> n

            One := {}

            hash = \@One _ -> 1

            main = hash (@Id 1234) + hash (@One {})
            "#
        ),
        1235,
        u64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn alias_member_specialization() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            Hash has
                hash : a -> U64 | a has Hash

            Id := U64

            hash = \@Id n -> n

            main =
                aliasedHash = hash
                aliasedHash (@Id 1234)
            "#
        ),
        1234,
        u64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn ability_constrained_in_non_member_usage() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [result] to "./platform"

            Hash has
                hash : a -> U64 | a has Hash

            mulHashes : a, a -> U64 | a has Hash
            mulHashes = \x, y -> hash x * hash y

            Id := U64
            hash = \@Id n -> n

            result = mulHashes (@Id 5) (@Id 7)
            "#
        ),
        35,
        u64
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn ability_constrained_in_non_member_usage_inferred() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [result] to "./platform"

            Hash has
                hash : a -> U64 | a has Hash

            mulHashes = \x, y -> hash x * hash y

            Id := U64
            hash = \@Id n -> n

            result = mulHashes (@Id 5) (@Id 7)
            "#
        ),
        35,
        u64
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn ability_constrained_in_non_member_multiple_specializations() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [result] to "./platform"

            Hash has
                hash : a -> U64 | a has Hash

            mulHashes : a, b -> U64 | a has Hash, b has Hash
            mulHashes = \x, y -> hash x * hash y

            Id := U64
            hash = \@Id n -> n

            Three := {}
            hash = \@Three _ -> 3

            result = mulHashes (@Id 100) (@Three {})
            "#
        ),
        300,
        u64
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn ability_constrained_in_non_member_multiple_specializations_inferred() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [result] to "./platform"

            Hash has
                hash : a -> U64 | a has Hash

            mulHashes = \x, y -> hash x * hash y

            Id := U64
            hash = \@Id n -> n

            Three := {}
            hash = \@Three _ -> 3

            result = mulHashes (@Id 100) (@Three {})
            "#
        ),
        300,
        u64
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn ability_used_as_type_still_compiles() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [result] to "./platform"

            Hash has
                hash : a -> U64 | a has Hash

            mulHashes : Hash, Hash -> U64
            mulHashes = \x, y -> hash x * hash y

            Id := U64
            hash = \@Id n -> n

            Three := {}
            hash = \@Three _ -> 3

            result = mulHashes (@Id 100) (@Three {})
            "#
        ),
        300,
        u64
    )
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn encode() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [myU8Bytes] to "./platform"

            Encoder fmt := List U8, fmt -> List U8 | fmt has Format

            Encoding has
              toEncoder : val -> Encoder fmt | val has Encoding, fmt has Format

            Format has
              u8 : U8 -> Encoder fmt | fmt has Format

            appendWith : List U8, Encoder fmt, fmt -> List U8 | fmt has Format
            appendWith = \lst, (@Encoder doFormat), fmt -> doFormat lst fmt

            toBytes : val, fmt -> List U8 | val has Encoding, fmt has Format
            toBytes = \val, fmt -> appendWith [] (toEncoder val) fmt


            Linear := {}

            # impl Format for Linear
            u8 = \n -> @Encoder (\lst, @Linear {} -> List.append lst n)

            Rgba := { r : U8, g : U8, b : U8, a : U8 }

            # impl Encoding for Rgba
            toEncoder = \@Rgba {r, g, b, a} ->
                @Encoder \lst, fmt -> lst
                    |> appendWith (u8 r) fmt
                    |> appendWith (u8 g) fmt
                    |> appendWith (u8 b) fmt
                    |> appendWith (u8 a) fmt

            myU8Bytes = toBytes (@Rgba { r: 106, g: 90, b: 205, a: 255 }) (@Linear {})
            "#
        ),
        RocList::from_slice(&[106, 90, 205, 255]),
        RocList<u8>
    )
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn decode() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [myU8] to "./platform"

            DecodeError : [TooShort, Leftover (List U8)]

            Decoder val fmt := List U8, fmt -> { result: Result val DecodeError, rest: List U8 } | fmt has DecoderFormatting

            Decoding has
                decoder : Decoder val fmt | val has Decoding, fmt has DecoderFormatting

            DecoderFormatting has
                u8 : Decoder U8 fmt | fmt has DecoderFormatting

            decodeWith : List U8, Decoder val fmt, fmt -> { result: Result val DecodeError, rest: List U8 } | fmt has DecoderFormatting
            decodeWith = \lst, (@Decoder doDecode), fmt -> doDecode lst fmt

            fromBytes : List U8, fmt -> Result val DecodeError
                        | fmt has DecoderFormatting, val has Decoding
            fromBytes = \lst, fmt ->
                when decodeWith lst decoder fmt is
                    { result, rest } ->
                        Result.after result \val ->
                            if List.isEmpty rest
                            then Ok val
                            else Err (Leftover rest)


            Linear := {}

            # impl DecoderFormatting for Linear
            u8 = @Decoder \lst, @Linear {} ->
                    when List.first lst is
                        Ok n -> { result: Ok n, rest: List.dropFirst lst }
                        Err _ -> { result: Err TooShort, rest: [] }

            MyU8 := U8

            # impl Decoding for MyU8
            decoder = @Decoder \lst, fmt ->
                { result, rest } = decodeWith lst u8 fmt
                { result: Result.map result (\n -> @MyU8 n), rest }

            myU8 =
                when fromBytes [15] (@Linear {}) is
                    Ok (@MyU8 n) -> n
                    _ -> 27u8
            "#
        ),
        15,
        u8
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn encode_use_stdlib() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test"
                imports [Encode.{ toEncoder }, Json]
                provides [main] to "./platform"

            HelloWorld := {}
            toEncoder = \@HelloWorld {} ->
                Encode.custom \bytes, fmt ->
                    bytes
                        |> Encode.appendWith (Encode.string "Hello, World!\n") fmt

            main =
                result = Str.fromUtf8 (Encode.toBytes (@HelloWorld {}) Json.format)
                when result is
                    Ok s -> s
                    _ -> "<bad>"
            "#
        ),
        RocStr::from("\"Hello, World!\n\""),
        RocStr
    )
}
