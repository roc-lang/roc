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

            MHash has
                hash : a -> U64 | a has MHash

            Id := U64 has [MHash {hash}]

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

            MHash has
                hash : a -> U64 | a has MHash

            Id := U64 has [ MHash {hash: hashId} ]

            hashId = \@Id n -> n

            One := {} has [ MHash {hash: hashOne} ]

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

            MHash has
                hash : a -> U64 | a has MHash

            Id := U64 has [MHash {hash}]

            hash = \@Id n -> n

            main =
                aliasedMHash = hash
                aliasedMHash (@Id 1234)
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

            MHash has
                hash : a -> U64 | a has MHash

            mulMHashes : a, a -> U64 | a has MHash
            mulMHashes = \x, y -> hash x * hash y

            Id := U64 has [MHash {hash}]
            hash = \@Id n -> n

            result = mulMHashes (@Id 5) (@Id 7)
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

            MHash has
                hash : a -> U64 | a has MHash

            mulMHashes = \x, y -> hash x * hash y

            Id := U64 has [MHash {hash}]
            hash = \@Id n -> n

            result = mulMHashes (@Id 5) (@Id 7)
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

            MHash has
                hash : a -> U64 | a has MHash

            mulMHashes : a, b -> U64 | a has MHash, b has MHash
            mulMHashes = \x, y -> hash x * hash y

            Id := U64 has [MHash { hash: hashId }]
            hashId = \@Id n -> n

            Three := {} has [MHash { hash: hashThree }]
            hashThree = \@Three _ -> 3

            result = mulMHashes (@Id 100) (@Three {})
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

            MHash has
                hash : a -> U64 | a has MHash

            mulMHashes = \x, y -> hash x * hash y

            Id := U64 has [MHash { hash: hashId }]
            hashId = \@Id n -> n

            Three := {} has [MHash { hash: hashThree }]
            hashThree = \@Three _ -> 3

            result = mulMHashes (@Id 100) (@Three {})
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

            MHash has
                hash : a -> U64 | a has MHash

            mulMHashes : MHash, MHash -> U64
            mulMHashes = \x, y -> hash x * hash y

            Id := U64 has [MHash { hash: hashId }]
            hashId = \@Id n -> n

            Three := {} has [MHash { hash: hashThree }]
            hashThree = \@Three _ -> 3

            result = mulMHashes (@Id 100) (@Three {})
            "#
        ),
        300,
        u64
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn bounds_to_multiple_abilities() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            Idempot has idempot : a -> a | a has Idempot
            Consume has consume : a -> Str | a has Consume

            Hello := Str has [Idempot { idempot: idempotHello }, Consume { consume: consumeHello }]

            idempotHello = \@Hello msg -> @Hello msg
            consumeHello = \@Hello msg -> msg

            lifecycle : a -> Str | a has Idempot & Consume
            lifecycle = \x -> idempot x |> consume

            main = lifecycle (@Hello "hello world")
            "#
        ),
        RocStr::from("hello world"),
        RocStr
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn encode() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [myU8Bytes] to "./platform"

            MEncoder fmt := List U8, fmt -> List U8 | fmt has Format

            MEncoding has
              toEncoder : val -> MEncoder fmt | val has MEncoding, fmt has Format

            Format has
              u8 : U8 -> MEncoder fmt | fmt has Format

            appendWith : List U8, MEncoder fmt, fmt -> List U8 | fmt has Format
            appendWith = \lst, (@MEncoder doFormat), fmt -> doFormat lst fmt

            toBytes : val, fmt -> List U8 | val has MEncoding, fmt has Format
            toBytes = \val, fmt -> appendWith [] (toEncoder val) fmt


            Linear := {} has [Format {u8}]

            u8 = \n -> @MEncoder (\lst, @Linear {} -> List.append lst n)

            Rgba := { r : U8, g : U8, b : U8, a : U8 } has [MEncoding {toEncoder}]

            toEncoder = \@Rgba {r, g, b, a} ->
                @MEncoder \lst, fmt -> lst
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

            MDecodeError : [TooShort, Leftover (List U8)]

            MDecoder val fmt := List U8, fmt -> { result: Result val MDecodeError, rest: List U8 } | fmt has MDecoderFormatting

            MDecoding has
                decoder : MDecoder val fmt | val has MDecoding, fmt has MDecoderFormatting

            MDecoderFormatting has
                u8 : MDecoder U8 fmt | fmt has MDecoderFormatting

            decodeWith : List U8, MDecoder val fmt, fmt -> { result: Result val MDecodeError, rest: List U8 } | fmt has MDecoderFormatting
            decodeWith = \lst, (@MDecoder doDecode), fmt -> doDecode lst fmt

            fromBytes : List U8, fmt -> Result val MDecodeError
                        | fmt has MDecoderFormatting, val has MDecoding
            fromBytes = \lst, fmt ->
                when decodeWith lst decoder fmt is
                    { result, rest } ->
                        Result.try result \val ->
                            if List.isEmpty rest
                            then Ok val
                            else Err (Leftover rest)


            Linear := {} has [MDecoderFormatting {u8}]

            u8 = @MDecoder \lst, @Linear {} ->
                    when List.first lst is
                        Ok n -> { result: Ok n, rest: List.dropFirst lst }
                        Err _ -> { result: Err TooShort, rest: [] }

            MyU8 := U8 has [MDecoding {decoder}]

            # impl MDecoding for MyU8
            decoder = @MDecoder \lst, fmt ->
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
                imports [Encode, Json]
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
                imports [Encode, Json]
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
fn encode_derive_to_encoder_for_opaque() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test"
                imports [Json]
                provides [main] to "./platform"

            HelloWorld := { a: Str } has [Encoding]

            main =
                result = Str.fromUtf8 (Encode.toBytes (@HelloWorld { a: "Hello, World!" }) Json.toUtf8)
                when result is
                    Ok s -> s
                    _ -> "<bad>"
            "#
        ),
        RocStr::from(r#"{"a":"Hello, World!"}"#),
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
                imports [Encode, Json]
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
                app "test" imports [Encode, Json] provides [main] to "./platform"

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
            #[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
            fn $typ() {
                assert_evals_to!(
                    &format!(indoc!(
                        r#"
                        app "test" imports [Encode, Json] provides [main] to "./platform"

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
        17.25, f32
        17.23, f64
        17.23, dec
    }
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn encode_derived_record_one_field_string() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test"
                imports [Encode, Json]
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
                imports [Encode, Json]
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
                imports [Encode, Json]
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
                imports [Encode, Json]
                provides [main] to "./platform"

            main =
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
                imports [Encode, Json]
                provides [main] to "./platform"

            main =
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
                imports [Encode, Json]
                provides [main] to "./platform"

            main =
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
                imports [Encode, Json]
                provides [main] to "./platform"

            main =
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
                imports [Encode, Json]
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
                imports [Encode, Json]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn encode_derived_list_of_lists_of_strings() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test"
                imports [Encode, Json]
                provides [main] to "./platform"

            main =
                lst = [["a", "b"], ["c", "d", "e"], ["f"]]
                encoded = Encode.toBytes lst Json.toUtf8
                result = Str.fromUtf8 encoded
                when result is
                    Ok s -> s
                    _ -> "<bad>"
            "#
        ),
        RocStr::from(r#"[["a","b"],["c","d","e"],["f"]]"#),
        RocStr
    )
}

#[test]
#[cfg(all(any(feature = "gen-llvm", feature = "gen-wasm")))]
fn encode_derived_record_with_many_types() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test"
                imports [Encode, Json]
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

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn decode_use_stdlib() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test"
                imports [Json]
                provides [main] to "./platform"

            MyNum := U8 has [Decoding {decoder: myDecoder}]

            myDecoder =
                Decode.custom \bytes, fmt ->
                    when Decode.decodeWith bytes Decode.u8 fmt is
                        {result, rest} ->
                            when result is
                                Ok n -> {result: Ok (@MyNum n), rest}
                                Err e -> {result: Err e, rest}

            main =
                when Decode.fromBytes [49, 53] Json.fromUtf8 is
                    Ok (@MyNum n) -> n
                    _ -> 101
            "#
        ),
        15,
        u8
    )
}

#[test]
#[cfg(all(
    any(feature = "gen-llvm", feature = "gen-wasm"),
    not(debug_assertions) // https://github.com/roc-lang/roc/issues/3898
))]
fn decode_derive_decoder_for_opaque() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test"
                imports [Json]
                provides [main] to "./platform"

            HelloWorld := { a: Str } has [Decoding]

            main =
                when Str.toUtf8 """{"a":"Hello, World!"}""" |> Decode.fromBytes Json.fromUtf8 is
                    Ok (@HelloWorld {a}) -> a
                    _ -> "FAIL"
            "#
        ),
        RocStr::from(r#"Hello, World!"#),
        RocStr
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn decode_use_stdlib_json_list() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test"
                imports [Json]
                provides [main] to "./platform"

            MyNumList := List U8 has [Decoding {decoder: myDecoder}]

            myDecoder =
                Decode.custom \bytes, fmt ->
                    when Decode.decodeWith bytes (Decode.list Decode.u8) fmt is
                        {result, rest} ->
                            when result is
                                Ok lst -> {result: Ok (@MyNumList lst), rest}
                                Err e -> {result: Err e, rest}

            main =
                when Str.toUtf8 "[1,2,3]" |> Decode.fromBytes Json.fromUtf8 is
                    Ok (@MyNumList lst) -> lst
                    _ -> []
            "#
        ),
        RocList::from_slice(&[1u8, 2u8, 3u8]),
        RocList<u8>
    )
}

mod decode_immediate {
    #[cfg(feature = "gen-llvm")]
    use crate::helpers::llvm::assert_evals_to;

    #[cfg(feature = "gen-wasm")]
    use crate::helpers::wasm::assert_evals_to;

    #[cfg(all(test, any(feature = "gen-llvm", feature = "gen-wasm")))]
    use indoc::indoc;

    #[cfg(all(test, any(feature = "gen-llvm", feature = "gen-wasm")))]
    use roc_std::RocStr;

    #[test]
    #[cfg(any(feature = "gen-llvm"))]
    fn string() {
        assert_evals_to!(
            indoc!(
                r#"
                app "test" imports [Json] provides [main] to "./platform"

                main =
                    when Str.toUtf8 "\"foo\"" |> Decode.fromBytes Json.fromUtf8 is
                        Ok s -> s
                        _ -> "<bad>"
                "#
            ),
            RocStr::from("foo"),
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
                        app "test" imports [Json] provides [main] to "./platform"

                        main =
                            when Num.toStr {}{} |> Str.toUtf8 |> Decode.fromBytes Json.fromUtf8 is
                                Ok n -> n
                                _ -> 101{}
                        "#
                    ), $num, stringify!($typ), stringify!($typ)),
                    $num,
                    $typ
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
        17.23, f32
        17.23, f64
    }

    #[test]
    #[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
    fn dec() {
        use roc_std::RocDec;

        assert_evals_to!(
            indoc!(
                r#"
                app "test" imports [Json] provides [main] to "./platform"

                main =
                    when Num.toStr 17.23dec |> Str.toUtf8 |> Decode.fromBytes Json.fromUtf8 is
                        Ok n -> n
                        _ -> 101dec
                "#
            ),
            RocDec::from_str("17.23").unwrap(),
            RocDec
        )
    }
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn decode_list_of_strings() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" imports [Json] provides [main] to "./platform"

            main =
                when Str.toUtf8 "[\"a\",\"b\",\"c\"]" |> Decode.fromBytes Json.fromUtf8 is
                    Ok l -> Str.joinWith l ","
                    _ -> "<bad>"
            "#
        ),
        RocStr::from("a,b,c"),
        RocStr
    )
}

#[test]
#[cfg(all(any(feature = "gen-llvm", feature = "gen-wasm")))]
fn encode_then_decode_list_of_strings() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" imports [Json] provides [main] to "./platform"

            main =
                when Encode.toBytes ["a", "b", "c"] Json.fromUtf8 |> Decode.fromBytes Json.fromUtf8 is
                    Ok l -> Str.joinWith l ","
                    _ -> "something went wrong"
            "#
        ),
        RocStr::from("a,b,c"),
        RocStr
    )
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
#[ignore = "#3696: Currently hits some weird panic in borrow checking, not sure if it's directly related to abilities."]
fn encode_then_decode_list_of_lists_of_strings() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" imports [Json] provides [main] to "./platform"

            main =
                when Encode.toBytes [["a", "b"], ["c", "d", "e"], ["f"]] Json.fromUtf8 |> Decode.fromBytes Json.fromUtf8 is
                    Ok list -> (List.map list \inner -> Str.joinWith inner ",") |> Str.joinWith l ";"
                    _ -> "something went wrong"
            "#
        ),
        RocStr::from("a,b;c,d,e;f"),
        RocStr
    )
}

#[test]
#[cfg(all(
    any(feature = "gen-llvm", feature = "gen-wasm"),
    not(debug_assertions) // https://github.com/roc-lang/roc/issues/3898
))]
fn decode_record_two_fields() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" imports [Json] provides [main] to "./platform"

            main =
                when Str.toUtf8 "{\"first\":\"ab\",\"second\":\"cd\"}" |> Decode.fromBytes Json.fromUtf8 is
                    Ok {first: "ab", second: "cd"} -> "abcd"
                    _ -> "something went wrong"
            "#
        ),
        RocStr::from("abcd"),
        RocStr
    )
}

#[test]
#[cfg(all(
    any(feature = "gen-llvm", feature = "gen-wasm"),
    not(debug_assertions) // https://github.com/roc-lang/roc/issues/3898
))]
fn decode_record_two_fields_string_and_int() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" imports [Json] provides [main] to "./platform"

            main =
                when Str.toUtf8 "{\"first\":\"ab\",\"second\":10}" |> Decode.fromBytes Json.fromUtf8 is
                    Ok {first: "ab", second: 10u8} -> "ab10"
                    _ -> "something went wrong"
            "#
        ),
        RocStr::from("ab10"),
        RocStr
    )
}

#[test]
#[cfg(all(
    any(feature = "gen-llvm", feature = "gen-wasm"),
    not(debug_assertions) // https://github.com/roc-lang/roc/issues/3898
))]
fn decode_record_two_fields_string_and_string_infer() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" imports [Json] provides [main] to "./platform"

            main =
                when Str.toUtf8 "{\"first\":\"ab\",\"second\":\"cd\"}" |> Decode.fromBytes Json.fromUtf8 is
                    Ok {first, second} -> Str.concat first second
                    _ -> "something went wrong"
            "#
        ),
        RocStr::from("abcd"),
        RocStr
    )
}

#[test]
#[cfg(all(
    any(feature = "gen-llvm", feature = "gen-wasm"),
    not(debug_assertions) // https://github.com/roc-lang/roc/issues/3898
))]
fn decode_record_two_fields_string_and_string_infer_local_var() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" imports [Json] provides [main] to "./platform"

            main =
                decoded = Str.toUtf8 "{\"first\":\"ab\",\"second\":\"cd\"}" |> Decode.fromBytes Json.fromUtf8
                when decoded is
                    Ok rcd -> Str.concat rcd.first rcd.second
                    _ -> "something went wrong"
            "#
        ),
        RocStr::from("abcd"),
        RocStr
    )
}

#[test]
#[cfg(all(
    any(feature = "gen-llvm", feature = "gen-wasm"),
    not(debug_assertions) // https://github.com/roc-lang/roc/issues/3898
))]
fn decode_record_two_fields_string_and_string_infer_local_var_destructured() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" imports [Json] provides [main] to "./platform"

            main =
                decoded = Str.toUtf8 "{\"first\":\"ab\",\"second\":\"cd\"}" |> Decode.fromBytes Json.fromUtf8
                when decoded is
                    Ok {first, second} -> Str.concat first second
                    _ -> "something went wrong"
            "#
        ),
        RocStr::from("abcd"),
        RocStr
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
#[ignore = "json parsing impl must be fixed first"]
fn decode_empty_record() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" imports [Json] provides [main] to "./platform"

            main =
                when Str.toUtf8 "{}" |> Decode.fromBytes Json.fromUtf8 is
                    Ok {} -> "empty"
                    _ -> "something went wrong"
            "#
        ),
        RocStr::from("empty"),
        RocStr
    )
}

#[test]
#[cfg(all(
    any(feature = "gen-llvm", feature = "gen-wasm"),
    not(feature = "gen-llvm-wasm"), // hits a wasm3 stack overflow
    not(debug_assertions) // https://github.com/roc-lang/roc/issues/3898
))]
fn decode_record_of_record() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" imports [Json] provides [main] to "./platform"

            main =
                when Str.toUtf8 "{\"outer\":{\"inner\":\"a\"},\"other\":{\"one\":\"b\",\"two\":10}}" |> Decode.fromBytes Json.fromUtf8 is
                    Ok {outer: {inner: "a"}, other: {one: "b", two: 10u8}} -> "ab10"
                    _ -> "something went wrong"
            "#
        ),
        RocStr::from("ab10"),
        RocStr
    )
}

#[cfg(all(test, any(feature = "gen-llvm", feature = "gen-wasm")))]
mod hash {
    #[cfg(feature = "gen-llvm")]
    use crate::helpers::llvm::assert_evals_to;

    #[cfg(feature = "gen-wasm")]
    use crate::helpers::wasm::assert_evals_to;

    use indoc::indoc;

    const TEST_HASHER: &str = indoc!(
        r#"
        THasher := List U8 has [Hasher {
            addBytes: tAddBytes,
            addU8: tAddU8,
            addU16: tAddU16,
            addU32: tAddU32,
            addU64: tAddU64,
            addU128: tAddU128,
            complete: tComplete,
        }]

        # ignores endian-ness
        byteAt = \n, shift ->
            Num.bitwiseAnd (Num.shiftRightBy n (shift * 8)) 0xFF
            |> Num.toU8

        do8 = \total, n ->
            total
            |> List.append (byteAt n 0)

        do16 = \total, n ->
            total
            |> do8 (n |> Num.toU8)
            |> do8 (Num.shiftRightBy n 8 |> Num.toU8)

        do32 = \total, n ->
            total
            |> do16 (n |> Num.toU16)
            |> do16 (Num.shiftRightBy n 16 |> Num.toU16)

        do64 = \total, n ->
            total
            |> do32 (n |> Num.toU32)
            |> do32 (Num.shiftRightBy n 32 |> Num.toU32)

        do128 = \total, n ->
            total
            |> do64 (n |> Num.toU64)
            |> do64 (Num.shiftRightBy n 64 |> Num.toU64)

        tAddBytes = \@THasher total, bytes -> @THasher (List.concat total bytes)
        tAddU8 = \@THasher total, n -> @THasher (do8 total n)
        tAddU16 = \@THasher total, n -> @THasher (do16 total n)
        tAddU32 = \@THasher total, n -> @THasher (do32 total n)
        tAddU64 = \@THasher total, n -> @THasher (do64 total n)
        tAddU128 = \@THasher total, n -> @THasher (do128 total n)
        tComplete = \@THasher _ -> Num.maxU64

        tRead = \@THasher bytes -> bytes
        "#
    );

    fn build_test(input: &str) -> String {
        format!(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                {}

                main =
                    @THasher []
                    |> Hash.hash ({})
                    |> tRead
                "#
            ),
            TEST_HASHER, input,
        )
    }

    mod immediate {
        use super::{assert_evals_to, build_test};
        use roc_std::RocList;

        #[test]
        fn i8() {
            assert_evals_to!(
                &build_test("-2i8"),
                RocList::from_slice(&[254]),
                RocList<u8>
            )
        }

        #[test]
        fn u8() {
            assert_evals_to!(
                &build_test("254u8"),
                RocList::from_slice(&[254]),
                RocList<u8>
            )
        }

        #[test]
        fn i16() {
            assert_evals_to!(
                &build_test("-2i16"),
                RocList::from_slice(&[254, 255]),
                RocList<u8>
            )
        }

        #[test]
        fn u16() {
            assert_evals_to!(
                &build_test("Num.maxU16 - 1"),
                RocList::from_slice(&[254, 255]),
                RocList<u8>
            )
        }

        #[test]
        fn i32() {
            assert_evals_to!(
                &build_test("-2i32"),
                RocList::from_slice(&[254, 255, 255, 255]),
                RocList<u8>
            )
        }

        #[test]
        fn u32() {
            assert_evals_to!(
                &build_test("Num.maxU32 - 1"),
                RocList::from_slice(&[254, 255, 255, 255]),
                RocList<u8>
            )
        }

        #[test]
        fn i64() {
            assert_evals_to!(
                &build_test("-2i64"),
                RocList::from_slice(&[254, 255, 255, 255, 255, 255, 255, 255]),
                RocList<u8>
            )
        }

        #[test]
        fn u64() {
            assert_evals_to!(
                &build_test("Num.maxU64 - 1"),
                RocList::from_slice(&[254, 255, 255, 255, 255, 255, 255, 255]),
                RocList<u8>
            )
        }

        #[test]
        #[cfg(not(feature = "gen-wasm"))] // shr not implemented for U128
        fn i128() {
            assert_evals_to!(
                &build_test("-2i128"),
                RocList::from_slice(&[
                    254, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
                ]),
                RocList<u8>
            )
        }

        #[test]
        #[cfg(not(feature = "gen-wasm"))] // shr not implemented for U128
        fn u128() {
            assert_evals_to!(
                &build_test("Num.maxU128 - 1"),
                RocList::from_slice(&[
                    254, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255
                ]),
                RocList<u8>
            )
        }

        #[test]
        fn string() {
            assert_evals_to!(
                &build_test(r#""ab☃AB""#),
                RocList::from_slice(&[97, 98, 226, 152, 131, 65, 66]),
                RocList<u8>
            )
        }

        #[test]
        fn list_u8() {
            assert_evals_to!(
                &build_test(r#"[15u8, 23u8, 37u8]"#),
                RocList::from_slice(&[15, 23, 37]),
                RocList<u8>
            )
        }

        #[test]
        fn list_string() {
            assert_evals_to!(
                &build_test(r#"["ab", "cd", "ef"]"#),
                RocList::from_slice(&[97, 98, 99, 100, 101, 102]),
                RocList<u8>
            )
        }

        #[test]
        fn list_list_string() {
            assert_evals_to!(
                &build_test(r#"[[ "ab", "cd" ], [ "ef" ]]"#),
                RocList::from_slice(&[97, 98, 99, 100, 101, 102]),
                RocList<u8>
            )
        }
    }

    mod derived {
        use super::{assert_evals_to, build_test, indoc, TEST_HASHER};
        use roc_std::RocList;

        #[test]
        fn empty_record() {
            assert_evals_to!(
                &build_test(r#"{}"#),
                RocList::from_slice(&[] as &[u8]),
                RocList<u8>
            )
        }

        #[test]
        fn record_of_u8_and_str() {
            assert_evals_to!(
                &build_test(r#"{ a: 15u8, b: "bc" }"#),
                RocList::from_slice(&[15, 98, 99]),
                RocList<u8>
            )
        }

        #[test]
        fn record_of_records() {
            assert_evals_to!(
                &build_test(r#"{ a: { b: 15u8, c: "bc" }, d: { b: 23u8, e: "ef" } }"#),
                RocList::from_slice(&[15, 98, 99, 23, 101, 102]),
                RocList<u8>
            )
        }

        #[test]
        fn record_of_list_of_records() {
            assert_evals_to!(
                &build_test(
                    r#"{ a: [ { b: 15u8 }, { b: 23u8 } ], b: [ { c: 45u8 }, { c: 73u8 } ] }"#
                ),
                RocList::from_slice(&[15, 23, 45, 73]),
                RocList<u8>
            )
        }

        #[test]
        fn hash_singleton_union() {
            assert_evals_to!(
                &format!(
                    indoc!(
                        r#"
                        app "test" provides [main] to "./platform"

                        {}

                        a : [A]
                        a = A

                        main =
                            @THasher []
                            |> Hash.hash a
                            |> tRead
                        "#
                    ),
                    TEST_HASHER,
                ),
                RocList::from_slice(&[
                    // hash nothing because this is a newtype of a unit layout.
                ] as &[u8]),
                RocList<u8>
            )
        }

        #[test]
        fn hash_bool_tag_union() {
            assert_evals_to!(
                &format!(
                    indoc!(
                        r#"
                        app "test" provides [main] to "./platform"

                        {}

                        a : [A, B]
                        a = A

                        b : [A, B]
                        b = B

                        main =
                            @THasher []
                            |> Hash.hash a
                            |> Hash.hash b
                            |> tRead
                        "#
                    ),
                    TEST_HASHER,
                ),
                RocList::from_slice(&[
                    0, // A
                    1, // B
                ]),
                RocList<u8>
            )
        }

        #[test]
        fn hash_byte_tag_union() {
            assert_evals_to!(
                &format!(
                    indoc!(
                        r#"
                        app "test" provides [main] to "./platform"

                        {}

                        l : List [A, B, C, D, E, F, G, H]
                        l = [A, B, C, D, E, F, G, H]

                        main =
                            @THasher []
                            |> Hash.hash l
                            |> tRead
                        "#
                    ),
                    TEST_HASHER,
                ),
                RocList::from_slice(&[
                    0, // A
                    1, // B
                    2, // C
                    3, // D
                    4, // E
                    5, // F
                    6, // G
                    7, // H
                ]),
                RocList<u8>
            )
        }

        #[test]
        fn hash_newtype_tag_union() {
            assert_evals_to!(
                &format!(
                    indoc!(
                        r#"
                        app "test" provides [main] to "./platform"

                        {}

                        a : [A U8 U8 U8]
                        a = A 15 23 47

                        main =
                            @THasher []
                            |> Hash.hash a
                            |> tRead
                        "#
                    ),
                    TEST_HASHER,
                ),
                RocList::from_slice(&[
                    // discriminant is skipped because it's a newtype
                    15, 23, 47
                ]),
                RocList<u8>
            )
        }

        #[test]
        fn hash_newtype_by_void_tag_union() {
            assert_evals_to!(
                &format!(
                    indoc!(
                        r#"
                        app "test" provides [main] to "./platform"

                        {}

                        a : Result [A U8 U8 U8] []
                        a = Ok (A 15 23 47)

                        main =
                            @THasher []
                            |> Hash.hash a
                            |> tRead
                        "#
                    ),
                    TEST_HASHER,
                ),
                RocList::from_slice(&[
                    1, // Ok
                    // A is skipped because it is a newtype
                    15, 23, 47
                ]),
                RocList<u8>
            )
        }

        #[test]
        fn hash_heterogenous_tags() {
            assert_evals_to!(
                &format!(
                    indoc!(
                        r#"
                        app "test" provides [main] to "./platform"

                        {}

                        a : [A U8 U8, B {{ a: U8 }}, C Str]
                        a = A 15 23

                        b : [A U8 U8, B {{ a: U8 }}, C Str]
                        b = B {{ a: 37 }}

                        c : [A U8 U8, B {{ a: U8 }}, C Str]
                        c = C "abc"

                        main =
                            @THasher []
                            |> Hash.hash a
                            |> Hash.hash b
                            |> Hash.hash c
                            |> tRead
                        "#
                    ),
                    TEST_HASHER,
                ),
                RocList::from_slice(&[
                    0, // dicsr A
                    15, 23, // payloads A
                    1,  // discr B
                    37, // payloads B
                    2,  // discr C
                    97, 98, 99 // payloads C
                ]),
                RocList<u8>
            )
        }

        #[test]
        fn hash_recursive_tag_union() {
            assert_evals_to!(
                &format!(
                    indoc!(
                        r#"
                        app "test" provides [main] to "./platform"

                        {}

                        ConsList : [Cons U8 ConsList, Nil]

                        c : ConsList
                        c = Cons 1 (Cons 2 Nil)

                        main =
                            @THasher []
                            |> Hash.hash c
                            |> tRead
                        "#
                    ),
                    TEST_HASHER,
                ),
                RocList::from_slice(&[
                    0, 1, // Cons 1
                    0, 2, // Cons 2
                    1, // Nil
                ]),
                RocList<u8>
            )
        }

        #[test]
        fn derived_hash_for_opaque_record() {
            assert_evals_to!(
                &format!(
                    indoc!(
                        r#"
                        app "test" provides [main] to "./platform"

                        {}

                        Q := {{ a: U8, b: U8, c: U8 }} has [Hash]

                        q = @Q {{ a: 15, b: 27, c: 31 }}

                        main =
                            @THasher []
                            |> Hash.hash q
                            |> tRead
                        "#
                    ),
                    TEST_HASHER,
                ),
                RocList::from_slice(&[15, 27, 31]),
                RocList<u8>
            )
        }
    }
}

#[cfg(all(test, any(feature = "gen-llvm", feature = "gen-wasm")))]
mod eq {
    #[cfg(feature = "gen-llvm")]
    use crate::helpers::llvm::assert_evals_to;

    #[cfg(feature = "gen-wasm")]
    use crate::helpers::wasm::assert_evals_to;

    use indoc::indoc;
    use roc_std::RocStr;

    #[test]
    fn custom_eq_impl() {
        assert_evals_to!(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                LyingEq := U8 has [Eq {isEq}]

                isEq = \@LyingEq m, @LyingEq n -> m != n

                main =
                    a = @LyingEq 10
                    b = @LyingEq 5
                    c = @LyingEq 5
                    if Bool.isEq a b && !(Bool.isEq b c) then
                        "okay"
                    else
                        "fail"
                "#
            ),
            RocStr::from("okay"),
            RocStr
        )
    }

    #[test]
    fn custom_eq_impl_for_fn_opaque() {
        assert_evals_to!(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                Q := ({} -> Str) has [Eq {isEq: isEqQ}]

                isEqQ = \@Q _, @Q _ -> Bool.true

                main = isEqQ (@Q \{} -> "a") (@Q \{} -> "a")
                "#
            ),
            true,
            bool
        )
    }

    #[test]
    #[ignore = "needs https://github.com/roc-lang/roc/issues/4557 first"]
    fn custom_eq_impl_for_fn_opaque_material() {
        assert_evals_to!(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                Q := ({} -> Str) has [Eq {isEq: isEqQ}]

                isEqQ = \@Q f1, @Q f2 -> (f1 {} == f2 {})

                main = isEqQ (@Q \{} -> "a") (@Q \{} -> "a")
                "#
            ),
            true,
            bool
        )
    }

    #[test]
    fn derive_structural_eq() {
        assert_evals_to!(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                main = Bool.isEq 10u8 10u8
                "#
            ),
            true,
            bool
        )
    }

    #[test]
    fn derive_structural_eq_for_opaque() {
        assert_evals_to!(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                Q := U8 has [Eq]

                main = (@Q 15) == (@Q 15)
                "#
            ),
            true,
            bool
        )
    }
}
