#[cfg(feature = "gen-llvm")]
use crate::helpers::llvm::assert_evals_to;

#[cfg(feature = "gen-wasm")]
use crate::helpers::wasm::assert_evals_to;

#[cfg(all(test, any(feature = "gen-llvm", feature = "gen-wasm")))]
use indoc::indoc;

#[cfg(all(test, any(feature = "gen-llvm", feature = "gen-wasm")))]
use roc_std::RocList;
#[cfg(all(test, any(feature = "gen-llvm", feature = "gen-wasm")))]
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

            Id := U64 has [Hash {hash}]

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

            Id := U64 has [ Hash {hash: hashId} ]

            hashId = \@Id n -> n

            One := {} has [ Hash {hash: hashOne} ]

            hashOne = \@One _ -> 1

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

            Id := U64 has [Hash {hash}]

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

            Id := U64 has [Hash {hash}]
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

            Id := U64 has [Hash {hash}]
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

            Id := U64 has [Hash { hash: hashId }]
            hashId = \@Id n -> n

            Three := {} has [Hash { hash: hashThree }]
            hashThree = \@Three _ -> 3

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

            Id := U64 has [Hash { hash: hashId }]
            hashId = \@Id n -> n

            Three := {} has [Hash { hash: hashThree }]
            hashThree = \@Three _ -> 3

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

            Id := U64 has [Hash { hash: hashId }]
            hashId = \@Id n -> n

            Three := {} has [Hash { hash: hashThree }]
            hashThree = \@Three _ -> 3

            result = mulHashes (@Id 100) (@Three {})
            "#
        ),
        300,
        u64
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
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


            Linear := {} has [Format {u8}]

            u8 = \n -> @Encoder (\lst, @Linear {} -> List.append lst n)

            Rgba := { r : U8, g : U8, b : U8, a : U8 } has [Encoding {toEncoder}]

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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
#[ignore = "running into weird let-generalization issue when a variable is only in output position, see #3660"]
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
                        Result.try result \val ->
                            if List.isEmpty rest
                            then Ok val
                            else Err (Leftover rest)


            Linear := {} has [DecoderFormatting {u8}]

            u8 = @Decoder \lst, @Linear {} ->
                    when List.first lst is
                        Ok n -> { result: Ok n, rest: List.dropFirst lst }
                        Err _ -> { result: Err TooShort, rest: [] }

            MyU8 := U8 has [Decoding {decoder}]

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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn encode_use_stdlib() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test"
                imports [Encode.{ Encoding, toEncoder }, Json]
                provides [main] to "./platform"

            HelloWorld := {} has [Encoding {toEncoder}]
            toEncoder = \@HelloWorld {} ->
                Encode.custom \bytes, fmt ->
                    bytes
                        |> Encode.appendWith (Encode.string "Hello, World!\n") fmt

            main =
                result = Str.fromUtf8 (Encode.toBytes (@HelloWorld {}) Json.toUtf8)
                when result is
                    Ok s -> s
                    _ -> "<bad>"
            "#
        ),
        RocStr::from("\"Hello, World!\n\""),
        RocStr
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn encode_use_stdlib_without_wrapping_custom() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test"
                imports [Encode.{ Encoding, toEncoder }, Json]
                provides [main] to "./platform"

            HelloWorld := {} has [Encoding {toEncoder}]
            toEncoder = \@HelloWorld {} -> Encode.string "Hello, World!\n"

            main =
                result = Str.fromUtf8 (Encode.toBytes (@HelloWorld {}) Json.toUtf8)
                when result is
                    Ok s -> s
                    _ -> "<bad>"
            "#
        ),
        RocStr::from("\"Hello, World!\n\""),
        RocStr
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn to_encoder_encode_custom_has_capture() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test"
                imports [Encode.{ Encoding, toEncoder }, Json]
                provides [main] to "./platform"

            HelloWorld := Str has [Encoding {toEncoder}]
            toEncoder = \@HelloWorld s1 ->
                Encode.custom \bytes, fmt ->
                    bytes
                        |> Encode.appendWith (Encode.string s1) fmt

            main =
                result = Str.fromUtf8 (Encode.toBytes (@HelloWorld "Hello, World!\n") Json.toUtf8)
                when result is
                    Ok s -> s
                    _ -> "<bad>"
            "#
        ),
        RocStr::from("\"Hello, World!\n\""),
        RocStr
    )
}

mod encode_immediate {
    #[cfg(feature = "gen-llvm")]
    use crate::helpers::llvm::assert_evals_to;

    #[cfg(feature = "gen-wasm")]
    use crate::helpers::wasm::assert_evals_to;

    #[cfg(all(test, any(feature = "gen-llvm", feature = "gen-wasm")))]
    use indoc::indoc;

    #[cfg(all(test, any(feature = "gen-llvm", feature = "gen-wasm")))]
    use roc_std::RocStr;

    #[test]
    #[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
    fn string() {
        assert_evals_to!(
            indoc!(
                r#"
                app "test" imports [Encode.{ toEncoder }, Json] provides [main] to "./platform"

                main =
                    when Str.fromUtf8 (Encode.toBytes "foo" Json.toUtf8) is
                        Ok s -> s
                        _ -> "<bad>"
                "#
            ),
            RocStr::from("\"foo\""),
            RocStr
        )
    }

    macro_rules! num_immediate {
        ($($num:expr, $typ:ident)*) => {$(
            #[test]
            #[cfg(any(feature = "gen-llvm"))]
            fn $typ() {
                assert_evals_to!(
                    &format!(indoc!(
                        r#"
                        app "test" imports [Encode.{{ toEncoder }}, Json] provides [main] to "./platform"

                        main =
                            when Str.fromUtf8 (Encode.toBytes {}{} Json.toUtf8) is
                                Ok s -> s
                                _ -> "<bad>"
                        "#
                    ), $num, stringify!($typ)),
                    RocStr::from(format!(r#"{}"#, $num).as_str()),
                    RocStr
                )
            }
        )*}
    }

    num_immediate! {
        17, i8
        17, i16
        17, i32
        17, i64
        17, i128
        17, u8
        17, u16
        17, u32
        17, u64
        17, u128
        // 17.23, f32 TODO https://github.com/rtfeldman/roc/issues/3522
        17.23, f64
        // 17.23, dec TODO https://github.com/rtfeldman/roc/issues/3522
    }
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn encode_derived_record_one_field_string() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test"
                imports [Encode.{ toEncoder }, Json]
                provides [main] to "./platform"

            main =
                result = Str.fromUtf8 (Encode.toBytes {a: "foo"} Json.toUtf8)
                when result is
                    Ok s -> s
                    _ -> "<bad>"
            "#
        ),
        RocStr::from(r#"{"a":"foo"}"#),
        RocStr
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn encode_derived_record_two_fields_strings() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test"
                imports [Encode.{ toEncoder }, Json]
                provides [main] to "./platform"

            main =
                rcd = {a: "foo", b: "bar"}
                result = Str.fromUtf8 (Encode.toBytes rcd Json.toUtf8)
                when result is
                    Ok s -> s
                    _ -> "<bad>"
            "#
        ),
        RocStr::from(r#"{"a":"foo","b":"bar"}"#),
        RocStr
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn encode_derived_nested_record_string() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test"
                imports [Encode.{ toEncoder }, Json]
                provides [main] to "./platform"

            main =
                rcd = {a: {b: "bar"}}
                encoded = Encode.toBytes rcd Json.toUtf8
                result = Str.fromUtf8 encoded
                when result is
                    Ok s -> s
                    _ -> "<bad>"
            "#
        ),
        RocStr::from(r#"{"a":{"b":"bar"}}"#),
        RocStr
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn encode_derived_tag_one_payload_string() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test"
                imports [Encode.{ toEncoder }, Json]
                provides [main] to "./platform"

            main =
                x : [A Str]
                x = A "foo"
                result = Str.fromUtf8 (Encode.toBytes x Json.toUtf8)
                when result is
                    Ok s -> s
                    _ -> "<bad>"
            "#
        ),
        RocStr::from(r#"{"A":["foo"]}"#),
        RocStr
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn encode_derived_tag_two_payloads_string() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test"
                imports [Encode.{ toEncoder }, Json]
                provides [main] to "./platform"

            main =
                x : [A Str Str]
                x = A "foo" "bar"
                result = Str.fromUtf8 (Encode.toBytes x Json.toUtf8)
                when result is
                    Ok s -> s
                    _ -> "<bad>"
            "#
        ),
        RocStr::from(r#"{"A":["foo","bar"]}"#),
        RocStr
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn encode_derived_nested_tag_string() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test"
                imports [Encode.{ toEncoder }, Json]
                provides [main] to "./platform"

            main =
                x : [A [B Str Str]]
                x = A (B "foo" "bar")
                encoded = Encode.toBytes x Json.toUtf8
                result = Str.fromUtf8 encoded
                when result is
                    Ok s -> s
                    _ -> "<bad>"
            "#
        ),
        RocStr::from(r#"{"A":[{"B":["foo","bar"]}]}"#),
        RocStr
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn encode_derived_nested_record_tag_record() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test"
                imports [Encode.{ toEncoder }, Json]
                provides [main] to "./platform"

            main =
                x : {a: [B {c: Str}]}
                x = {a: (B ({c: "foo"}))}
                encoded = Encode.toBytes x Json.toUtf8
                result = Str.fromUtf8 encoded
                when result is
                    Ok s -> s
                    _ -> "<bad>"
            "#
        ),
        RocStr::from(r#"{"a":{"B":[{"c":"foo"}]}}"#),
        RocStr
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn encode_derived_list_string() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test"
                imports [Encode.{ toEncoder }, Json]
                provides [main] to "./platform"

            main =
                lst = ["foo", "bar", "baz"]
                encoded = Encode.toBytes lst Json.toUtf8
                result = Str.fromUtf8 encoded
                when result is
                    Ok s -> s
                    _ -> "<bad>"
            "#
        ),
        RocStr::from(r#"["foo","bar","baz"]"#),
        RocStr
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn encode_derived_list_of_records() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test"
                imports [Encode.{ toEncoder }, Json]
                provides [main] to "./platform"

            main =
                lst = [{a: "foo"}, {a: "bar"}, {a: "baz"}]
                encoded = Encode.toBytes lst Json.toUtf8
                result = Str.fromUtf8 encoded
                when result is
                    Ok s -> s
                    _ -> "<bad>"
            "#
        ),
        RocStr::from(r#"[{"a":"foo"},{"a":"bar"},{"a":"baz"}]"#),
        RocStr
    )
}

#[test]
#[cfg(all(
    any(feature = "gen-llvm", feature = "gen-wasm"),
    not(feature = "gen-llvm-wasm") // hits a stack limit in wasm3
))]
fn encode_derived_record_with_many_types() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test"
                imports [Encode.{ toEncoder }, Json]
                provides [main] to "./platform"

            main =
                fresh : [Fresh Str, Rotten Str]
                fresh = Fresh "tomatoes"
                rcd = {actors: ["Idris Elba", "Mila Kunis"], year: 2004u16, rating: {average: 7u8, min: 1u8, max: 10u8, sentiment: fresh}}
                result = Str.fromUtf8 (Encode.toBytes rcd Json.toUtf8)
                when result is
                    Ok s -> s
                    _ -> "<bad>"
            "#
        ),
        RocStr::from(
            r#"{"actors":["Idris Elba","Mila Kunis"],"rating":{"average":7,"max":10,"min":1,"sentiment":{"Fresh":["tomatoes"]}},"year":2004}"#
        ),
        RocStr
    )
}
