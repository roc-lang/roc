#[cfg(feature = "gen-llvm")]
use crate::helpers::llvm::assert_evals_to;

#[cfg(feature = "gen-wasm")]
use crate::helpers::wasm::assert_evals_to;

#[cfg(all(test, any(feature = "gen-llvm", feature = "gen-wasm")))]
use indoc::{formatdoc, indoc};

#[cfg(all(test, any(feature = "gen-llvm", feature = "gen-wasm")))]
use roc_std::RocList;
#[cfg(all(test, any(feature = "gen-llvm", feature = "gen-wasm")))]
use roc_std::RocStr;

use roc_test_utils::TAG_LEN_ENCODER_FMT;

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn hash_specialization() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            MHash implements
                hash : a -> U64 where a implements MHash

            Id := U64 implements [MHash {hash}]

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

            MHash implements
                hash : a -> U64 where a implements MHash

            Id := U64 implements [ MHash {hash: hash_id} ]

            hash_id = \@Id n -> n

            One := {} implements [ MHash {hash: hash_one} ]

            hash_one = \@One _ -> 1

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

            MHash implements
                hash : a -> U64 where a implements MHash

            Id := U64 implements [MHash {hash}]

            hash = \@Id n -> n

            main =
                aliased_m_hash = hash
                aliased_m_hash (@Id 1234)
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

            MHash implements
                hash : a -> U64 where a implements MHash

            mul_m_hashes : a, a -> U64 where a implements MHash
            mul_m_hashes = \x, y -> hash x * hash y

            Id := U64 implements [MHash {hash}]
            hash = \@Id n -> n

            result = mul_m_hashes (@Id 5) (@Id 7)
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

            MHash implements
                hash : a -> U64 where a implements MHash

            mul_m_hashes = \x, y -> hash x * hash y

            Id := U64 implements [MHash {hash}]
            hash = \@Id n -> n

            result = mul_m_hashes (@Id 5) (@Id 7)
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

            MHash implements
                hash : a -> U64 where a implements MHash

            mul_m_hashes : a, b -> U64 where a implements MHash, b implements MHash
            mul_m_hashes = \x, y -> hash x * hash y

            Id := U64 implements [MHash { hash: hash_id }]
            hash_id = \@Id n -> n

            Three := {} implements [MHash { hash: hash_three }]
            hash_three = \@Three _ -> 3

            result = mul_m_hashes (@Id 100) (@Three {})
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

            MHash implements
                hash : a -> U64 where a implements MHash

            mul_m_hashes = \x, y -> hash x * hash y

            Id := U64 implements [MHash { hash: hash_id }]
            hash_id = \@Id n -> n

            Three := {} implements [MHash { hash: hash_three }]
            hash_three = \@Three _ -> 3

            result = mul_m_hashes (@Id 100) (@Three {})
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

            MHash implements
                hash : a -> U64 where a implements MHash

            mul_m_hashes : MHash, MHash -> U64
            mul_m_hashes = \x, y -> hash x * hash y

            Id := U64 implements [MHash { hash: hash_id }]
            hash_id = \@Id n -> n

            Three := {} implements [MHash { hash: hash_three }]
            hash_three = \@Three _ -> 3

            result = mul_m_hashes (@Id 100) (@Three {})
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

            Idempot implements idempot : a -> a where a implements Idempot
            Consume implements consume : a -> Str where a implements Consume

            Hello := Str implements [Idempot { idempot: idempot_hello }, Consume { consume: consume_hello }]

            idempot_hello = \@Hello msg -> @Hello msg
            consume_hello = \@Hello msg -> msg

            lifecycle : a -> Str where a implements Idempot & Consume
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
            app "test" provides [my_u8_bytes] to "./platform"

            MEncoder fmt := List U8, fmt -> List U8 where fmt implements Format

            MEncoding implements
              to_encoder : val -> MEncoder fmt where val implements MEncoding, fmt implements Format

            Format implements
              u8 : U8 -> MEncoder fmt where fmt implements Format

            append_with : List U8, MEncoder fmt, fmt -> List U8 where fmt implements Format
            append_with = \lst, (@MEncoder do_format), fmt -> do_format lst fmt

            to_bytes : val, fmt -> List U8 where val implements MEncoding, fmt implements Format
            to_bytes = \val, fmt -> append_with [] (to_encoder val) fmt


            Linear := {} implements [Format {u8}]

            u8 = \n -> @MEncoder (\lst, @Linear {} -> List.append lst n)

            Rgba := { r : U8, g : U8, b : U8, a : U8 } implements [MEncoding {to_encoder}]

            to_encoder = \@Rgba {r, g, b, a} ->
                @MEncoder \lst, fmt -> lst
                    |> append_with (u8 r) fmt
                    |> append_with (u8 g) fmt
                    |> append_with (u8 b) fmt
                    |> append_with (u8 a) fmt

            my_u8_bytes = to_bytes (@Rgba { r: 106, g: 90, b: 205, a: 255 }) (@Linear {})
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

            MDecoder val fmt := List U8, fmt -> { result: Result val MDecodeError, rest: List U8 } where fmt implements MDecoderFormatting

            MDecoding implements
                decoder : MDecoder val fmt where val implements MDecoding, fmt implements MDecoderFormatting

            MDecoderFormatting implements
                u8 : MDecoder U8 fmt where fmt implements MDecoderFormatting

            decode_with : List U8, MDecoder val fmt, fmt -> { result: Result val MDecodeError, rest: List U8 } where fmt implements MDecoderFormatting
            decode_with = \lst, (@MDecoder doDecode), fmt -> doDecode lst fmt

            from_bytes : List U8, fmt -> Result val MDecodeError
                        where fmt implements MDecoderFormatting, val implements MDecoding
            from_bytes = \lst, fmt ->
                when decode_with lst decoder fmt is
                    { result, rest } ->
                        Result.try result \val ->
                            if List.is_empty rest
                            then Ok val
                            else Err (Leftover rest)


            Linear := {} implements [MDecoderFormatting {u8}]

            u8 = @MDecoder \lst, @Linear {} ->
                    when List.first lst is
                        Ok n -> { result: Ok n, rest: List.drop_first lst 1 }
                        Err _ -> { result: Err TooShort, rest: [] }

            MyU8 := U8 implements [MDecoding {decoder}]

            # impl MDecoding for MyU8
            decoder = @MDecoder \lst, fmt ->
                { result, rest } = decode_with lst u8 fmt
                { result: Result.map_ok result (\n -> @MyU8 n), rest }

            myU8 =
                when from_bytes [15] (@Linear {}) is
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
        &formatdoc!(
            r#"
            app "test" provides [main] to "./platform"

            {TAG_LEN_ENCODER_FMT}

            HelloWorld := {{}} implements [Encoding {{to_encoder}}]
            to_encoder = \@HelloWorld {{}} ->
                Encode.custom \bytes, fmt ->
                    bytes
                        |> Encode.append_with (Encode.string "Hello, World!\n") fmt

            main =
                result = Str.from_utf8 (Encode.to_bytes (@HelloWorld {{}}) tag_len_fmt)
                when result is
                    Ok s -> s
                    _ -> "<bad>"
            "#
        ),
        RocStr::from("s14 Hello, World!\n "),
        RocStr
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn encode_use_stdlib_without_wrapping_custom() {
    assert_evals_to!(
        &formatdoc!(
            r#"
            app "test" provides [main] to "./platform"

            {TAG_LEN_ENCODER_FMT}

            HelloWorld := {{}} implements [Encoding {{to_encoder}}]
            to_encoder = \@HelloWorld {{}} -> Encode.string "Hello, World!\n"

            main =
                result = Str.from_utf8 (Encode.to_bytes (@HelloWorld {{}}) tag_len_fmt)
                when result is
                    Ok s -> s
                    _ -> "<bad>"
            "#
        ),
        RocStr::from("s14 Hello, World!\n "),
        RocStr
    )
}

#[test]
#[cfg(not(debug_assertions))]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn encode_derive_to_encoder_for_opaque() {
    assert_evals_to!(
        &formatdoc!(
            r#"
            app "test" provides [main] to "./platform"

            {TAG_LEN_ENCODER_FMT}

            HelloWorld := {{ a: Str }} implements [Encoding]

            main =
                result = Str.from_utf8 (Encode.to_bytes (@HelloWorld {{ a: "Hello, World!" }}) tag_len_fmt)
                when result is
                    Ok s -> s
                    _ -> "<bad>"
            "#
        ),
        RocStr::from("r1 s1 a s13 Hello, World! "),
        RocStr
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn to_encoder_encode_custom_has_capture() {
    assert_evals_to!(
        &formatdoc!(
            r#"
            app "test" provides [main] to "./platform"

            {TAG_LEN_ENCODER_FMT}

            HelloWorld := Str implements [Encoding {{to_encoder}}]
            to_encoder = \@HelloWorld s1 ->
                Encode.custom \bytes, fmt ->
                    bytes
                        |> Encode.append_with (Encode.string s1) fmt

            main =
                result = Str.from_utf8 (Encode.to_bytes (@HelloWorld "Hello, World!\n") tag_len_fmt)
                when result is
                    Ok s -> s
                    _ -> "<bad>"
            "#
        ),
        RocStr::from("s14 Hello, World!\n "),
        RocStr
    )
}

mod encode_immediate {
    use super::TAG_LEN_ENCODER_FMT;

    #[cfg(feature = "gen-llvm")]
    use crate::helpers::llvm::assert_evals_to;

    #[cfg(feature = "gen-wasm")]
    use crate::helpers::wasm::assert_evals_to;

    #[cfg(all(test, any(feature = "gen-llvm", feature = "gen-wasm")))]
    use indoc::formatdoc;

    #[cfg(all(test, any(feature = "gen-llvm", feature = "gen-wasm")))]
    use roc_std::RocStr;

    #[test]
    #[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
    fn string() {
        assert_evals_to!(
            &formatdoc!(
                r#"
                app "test" provides [main] to "./platform"

                {TAG_LEN_ENCODER_FMT}

                main =
                    when Str.from_utf8 (Encode.to_bytes "foo" tag_len_fmt) is
                        Ok s -> s
                        _ -> "<bad>"
                "#
            ),
            RocStr::from("s3 foo "),
            RocStr
        )
    }

    #[test]
    #[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
    fn ranged_number() {
        assert_evals_to!(
            &formatdoc!(
                r#"
                app "test" provides [main] to "./platform"

                {TAG_LEN_ENCODER_FMT}

                main =
                    when Str.from_utf8 (Encode.to_bytes [1, 2, 3] tag_len_fmt) is
                        Ok s -> s
                        _ -> "<bad>"
                "#
            ),
            RocStr::from("l3 n1 n2 n3 "),
            RocStr
        )
    }

    #[test]
    #[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
    fn bool() {
        assert_evals_to!(
            &formatdoc!(
                r#"
                app "test" provides [main] to "./platform"

                {TAG_LEN_ENCODER_FMT}

                main =
                    when Str.from_utf8 (Encode.to_bytes Bool.false tag_len_fmt) is
                        Ok s -> s
                        _ -> "<bad>"
                "#
            ),
            RocStr::from(r"n0 "),
            RocStr
        )
    }

    macro_rules! num_immediate {
        ($($num:expr, $typ:ident)*) => {$(
            #[test]
            #[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
            fn $typ() {
                assert_evals_to!(
                    &formatdoc!(
                        r#"
                        app "test" provides [main] to "./platform"

                        {TAG_LEN_ENCODER_FMT}

                        main =
                            when Str.from_utf8 (Encode.to_bytes {}{} tag_len_fmt) is
                                Ok s -> s
                                _ -> "<bad>"
                        "#, $num, stringify!($typ)),
                    RocStr::from(format!(r"n{} ", $num).as_str()),
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
#[cfg(not(debug_assertions))]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn encode_derived_record_one_field_string() {
    assert_evals_to!(
        &formatdoc!(
            r#"
            app "test" provides [main] to "./platform"

            {TAG_LEN_ENCODER_FMT}

            main =
                result = Str.from_utf8 (Encode.to_bytes {{a: "foo"}} tag_len_fmt)
                when result is
                    Ok s -> s
                    _ -> "<bad>"
            "#
        ),
        RocStr::from("r1 s1 a s3 foo "),
        RocStr
    )
}

#[test]
#[cfg(not(debug_assertions))]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn encode_derived_record_two_fields_strings() {
    assert_evals_to!(
        &formatdoc!(
            r#"
            app "test" provides [main] to "./platform"

            {TAG_LEN_ENCODER_FMT}

            main =
                rcd = {{a: "foo", b: "bar"}}
                result = Str.from_utf8 (Encode.to_bytes rcd tag_len_fmt)
                when result is
                    Ok s -> s
                    _ -> "<bad>"
            "#
        ),
        RocStr::from("r2 s1 a s3 foo s1 b s3 bar "),
        RocStr
    )
}

#[test]
#[cfg(not(debug_assertions))]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn encode_derived_nested_record_string() {
    assert_evals_to!(
        &formatdoc!(
            r#"
            app "test" provides [main] to "./platform"

            {TAG_LEN_ENCODER_FMT}

            main =
                rcd = {{a: {{b: "bar"}}}}
                encoded = Encode.to_bytes rcd tag_len_fmt
                result = Str.from_utf8 encoded
                when result is
                    Ok s -> s
                    _ -> "<bad>"
            "#
        ),
        RocStr::from("r1 s1 a r1 s1 b s3 bar "),
        RocStr
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn encode_derived_tag_one_payload_string() {
    assert_evals_to!(
        &formatdoc!(
            r#"
            app "test" provides [main] to "./platform"

            {TAG_LEN_ENCODER_FMT}

            main =
                x = A "foo"
                result = Str.from_utf8 (Encode.to_bytes x tag_len_fmt)
                when result is
                    Ok s -> s
                    _ -> "<bad>"
            "#
        ),
        RocStr::from("l2 s1 A s3 foo "),
        RocStr
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn encode_derived_tag_two_payloads_string() {
    assert_evals_to!(
        &formatdoc!(
            r#"
            app "test" provides [main] to "./platform"

            {TAG_LEN_ENCODER_FMT}

            main =
                x = A "foo" "bar"
                result = Str.from_utf8 (Encode.to_bytes x tag_len_fmt)
                when result is
                    Ok s -> s
                    _ -> "<bad>"
            "#
        ),
        RocStr::from("l3 s1 A s3 foo s3 bar "),
        RocStr
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn encode_derived_nested_tag_string() {
    assert_evals_to!(
        &formatdoc!(
            r#"
            app "test" provides [main] to "./platform"

            {TAG_LEN_ENCODER_FMT}

            main =
                x = A (B "foo" "bar")
                encoded = Encode.to_bytes x tag_len_fmt
                result = Str.from_utf8 encoded
                when result is
                    Ok s -> s
                    _ -> "<bad>"
            "#
        ),
        RocStr::from("l2 s1 A l3 s1 B s3 foo s3 bar "),
        RocStr
    )
}

#[test]
#[cfg(not(debug_assertions))]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn encode_derived_nested_record_tag_record() {
    assert_evals_to!(
        &formatdoc!(
            r#"
            app "test" provides [main] to "./platform"

            {TAG_LEN_ENCODER_FMT}

            main =
                x = {{a: (B ({{c: "foo"}}))}}
                encoded = Encode.to_bytes x tag_len_fmt
                result = Str.from_utf8 encoded
                when result is
                    Ok s -> s
                    _ -> "<bad>"
            "#
        ),
        RocStr::from("r1 s1 a l2 s1 B r1 s1 c s3 foo "),
        RocStr
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn encode_derived_list_string() {
    assert_evals_to!(
        &formatdoc!(
            r#"
            app "test" provides [main] to "./platform"

            {TAG_LEN_ENCODER_FMT}

            main =
                lst = ["foo", "bar", "baz"]
                encoded = Encode.to_bytes lst tag_len_fmt
                result = Str.from_utf8 encoded
                when result is
                    Ok s -> s
                    _ -> "<bad>"
            "#
        ),
        RocStr::from("l3 s3 foo s3 bar s3 baz "),
        RocStr
    )
}

#[test]
#[cfg(not(debug_assertions))]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn encode_derived_list_of_records() {
    assert_evals_to!(
        &formatdoc!(
            r#"
            app "test" provides [main] to "./platform"

            {TAG_LEN_ENCODER_FMT}

            main =
                lst = [{{a: "foo"}}, {{a: "bar"}}, {{a: "baz"}}]
                encoded = Encode.to_bytes lst tag_len_fmt
                result = Str.from_utf8 encoded
                when result is
                    Ok s -> s
                    _ -> "<bad>"
            "#
        ),
        RocStr::from("l3 r1 s1 a s3 foo r1 s1 a s3 bar r1 s1 a s3 baz "),
        RocStr
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn encode_derived_list_of_lists_of_strings() {
    assert_evals_to!(
        &formatdoc!(
            r#"
            app "test" provides [main] to "./platform"

            {TAG_LEN_ENCODER_FMT}

            main =
                lst = [["a", "b"], ["c", "d", "e"], ["f"]]
                encoded = Encode.to_bytes lst tag_len_fmt
                result = Str.from_utf8 encoded
                when result is
                    Ok s -> s
                    _ -> "<bad>"
            "#
        ),
        RocStr::from("l3 l2 s1 a s1 b l3 s1 c s1 d s1 e l1 s1 f "),
        RocStr
    )
}

#[test]
#[cfg(not(debug_assertions))]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn encode_derived_record_with_many_types() {
    assert_evals_to!(
        &formatdoc!(
            r#"
            app "test" provides [main] to "./platform"

            {TAG_LEN_ENCODER_FMT}

            main =
                fresh : [Fresh Str, Rotten Str]
                fresh = Fresh "tomatoes"
                rcd = {{actors: ["Idris Elba", "Mila Kunis"], year: 2004u16, rating: {{average: 7u8, min: 1u8, max: 10u8, sentiment: fresh}}}}
                result = Str.from_utf8 (Encode.to_bytes rcd tag_len_fmt)
                when result is
                    Ok s -> s
                    _ -> "<bad>"
            "#
        ),
        RocStr::from(
            "r3 s6 actors l2 s10 Idris Elba s10 Mila Kunis s6 rating r4 s7 average n7 s3 max n10 s3 min n1 s9 sentiment l2 s5 Fresh s8 tomatoes s4 year n2004 "
        ),
        RocStr
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn encode_derived_tuple_two_fields() {
    assert_evals_to!(
        &formatdoc!(
            r#"
            app "test" provides [main] to "./platform"

            {TAG_LEN_ENCODER_FMT}

            main =
                tup = ("foo", 10u8)
                result = Str.from_utf8 (Encode.to_bytes tup tag_len_fmt)
                when result is
                    Ok s -> s
                    _ -> "<bad>"
            "#
        ),
        RocStr::from("l2 s3 foo n10 "),
        RocStr
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn encode_derived_tuple_of_tuples() {
    assert_evals_to!(
        &formatdoc!(
            r#"
            app "test" provides [main] to "./platform"

            {TAG_LEN_ENCODER_FMT}

            main =
                tup = ( ("foo", 10u8), (23u8, "bar", 15u8) )
                result = Str.from_utf8 (Encode.to_bytes tup tag_len_fmt)
                when result is
                    Ok s -> s
                    _ -> "<bad>"
            "#
        ),
        RocStr::from("l2 l2 s3 foo n10 l3 n23 s3 bar n15 "),
        RocStr
    )
}

#[test]
#[cfg(not(debug_assertions))]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn encode_derived_generic_record_with_different_field_types() {
    assert_evals_to!(
        &formatdoc!(
            r#"
            app "test" provides [main] to "./platform"

            {TAG_LEN_ENCODER_FMT}

            Q a b := {{a: a, b: b}} implements [Encoding]

            q = @Q {{a: 10u32, b: "fieldb"}}

            main =
                result = Str.from_utf8 (Encode.to_bytes q tag_len_fmt)
                when result is
                    Ok s -> s
                    _ -> "<bad>"
            "#
        ),
        RocStr::from("r2 s1 a n10 s1 b s6 fieldb "),
        RocStr
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn encode_derived_generic_tag_with_different_field_types() {
    assert_evals_to!(
        &formatdoc!(
            r#"
            app "test" provides [main] to "./platform"

            {TAG_LEN_ENCODER_FMT}

            Q a b := [A a, B b] implements [Encoding]

            q : Q Str U32
            q = @Q (B 67)

            main =
                result = Str.from_utf8 (Encode.to_bytes q tag_len_fmt)
                when result is
                    Ok s -> s
                    _ -> "<bad>"
            "#
        ),
        RocStr::from("l2 s1 B n67 "),
        RocStr
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn specialize_unique_newtype_records() {
    crate::helpers::with_larger_debug_stack(|| {
        assert_evals_to!(
            &formatdoc!(
                r#"
                app "test" provides [main] to "./platform"

                {TAG_LEN_ENCODER_FMT}

                main =
                    when Str.from_utf8 (Encode.to_bytes {{a: Bool.true}} tag_len_fmt) is
                        Ok s -> when Str.from_utf8 (Encode.to_bytes {{b: Bool.true}} tag_len_fmt) is
                            Ok t -> "$(s)$(t)"
                            _ -> "<bad>"
                        _ -> "<bad>"
                "#
            ),
            RocStr::from("r1 s1 a n1 r1 s1 b n1 "),
            RocStr
        )
    });
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn decode_use_stdlib() {
    assert_evals_to!(
        &formatdoc!(
            r#"
            app "test" provides [main] to "./platform"

            {TAG_LEN_ENCODER_FMT}

            MyNum := U8 implements [Decoding {{decoder: myDecoder}}]

            myDecoder =
                Decode.custom \bytes, fmt ->
                    when Decode.decode_with bytes Decode.u8 fmt is
                        {{result, rest}} ->
                            when result is
                                Ok n -> {{result: Ok (@MyNum n), rest}}
                                Err e -> {{result: Err e, rest}}

            main =
                when Decode.from_bytes [110, 49, 53, 32] tag_len_fmt is
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
        &formatdoc!(
            r#"
            app "test" provides [main] to "./platform"

            {TAG_LEN_ENCODER_FMT}

            HelloWorld := {{ a: Str }} implements [Decoding]

            main =
                when Str.to_utf8 "r1 s1 a s13 Hello, World! " |> Decode.from_bytes tag_len_fmt is
                    Ok (@HelloWorld {{a}}) -> a
                    _ -> "FAIL"
            "#
        ),
        RocStr::from(r"Hello, World!"),
        RocStr
    )
}
#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn decode_use_stdlib_custom_list() {
    assert_evals_to!(
        &formatdoc!(
            r#"
            app "test" provides [main] to "./platform"

            {TAG_LEN_ENCODER_FMT}

            MyNumList := List U8 implements [Decoding {{decoder: myDecoder}}]
            myDecoder =
                Decode.custom \bytes, fmt ->
                    when Decode.decode_with bytes (Decode.list Decode.u8) fmt is
                        {{result, rest}} ->
                            when result is
                                Ok lst -> {{result: Ok (@MyNumList lst), rest}}
                                Err e -> {{result: Err e, rest}}
            main =
                when Str.to_utf8 "l3 n1 n2 n3 " |> Decode.from_bytes tag_len_fmt is
                    Ok (@MyNumList lst) -> lst
                    _ -> []
            "#
        ),
        RocList::from_slice(&[1u8, 2u8, 3u8]),
        RocList<u8>
    )
}

mod decode_immediate {
    use super::TAG_LEN_ENCODER_FMT;

    #[cfg(feature = "gen-llvm")]
    use crate::helpers::llvm::assert_evals_to;

    #[cfg(feature = "gen-wasm")]
    use crate::helpers::wasm::assert_evals_to;

    #[cfg(all(test, any(feature = "gen-llvm", feature = "gen-wasm")))]
    use indoc::formatdoc;

    #[cfg(all(test, feature = "gen-llvm"))]
    use roc_std::{RocStr, I128, U128};

    #[test]
    #[cfg(feature = "gen-llvm")]
    fn string() {
        crate::helpers::with_larger_debug_stack(|| {
            assert_evals_to!(
                &formatdoc!(
                    r#"
                    app "test" provides [main] to "./platform"

                    {TAG_LEN_ENCODER_FMT}

                    main =
                        when Str.to_utf8 "s3 foo " |> Decode.from_bytes tag_len_fmt is
                            Ok s -> s
                            _ -> "<bad>"
                    "#
                ),
                RocStr::from("foo"),
                RocStr
            )
        });
    }

    #[test]
    #[cfg(feature = "gen-llvm")]
    fn ranged_number() {
        assert_evals_to!(
            &formatdoc!(
                r#"
                app "test" provides [main] to "./platform"

                {TAG_LEN_ENCODER_FMT}

                main =
                    input = Str.to_utf8 "l3 n1 n2 n3 "
                    expected = [1,2,3]

                    actual = Decode.from_bytes input tag_len_fmt |> Result.with_default []

                    actual == expected
                "#
            ),
            true,
            bool
        )
    }

    #[test]
    #[cfg(feature = "gen-llvm")]
    fn bool() {
        assert_evals_to!(
            &formatdoc!(
                r#"
                app "test" provides [main] to "./platform"

                {TAG_LEN_ENCODER_FMT}

                main =
                    when Str.to_utf8 "n0 " |> Decode.from_bytes tag_len_fmt is
                        Ok s -> s
                        _ -> Bool.true
                "#
            ),
            false,
            bool
        )
    }

    macro_rules! num_immediate {
        ($($num:expr, $typ:ident, $expected_type:ident)*) => {$(
            #[test]
            #[cfg(feature = "gen-llvm")]
            fn $typ() {
                assert_evals_to!(
                    &formatdoc!(
                        r#"
                        app "test" provides [main] to "./platform"

                        {TAG_LEN_ENCODER_FMT}

                        main =
                            when Str.to_utf8 "n{} " |> Decode.from_bytes tag_len_fmt is
                                Ok n -> n
                                _ -> 101{}
                        "#,
                        $num, stringify!($typ)),
                    $num,
                    $expected_type
                )
            }
        )*}
    }

    num_immediate! {
        17, i8, i8
        17, i16, i16
        17, i32, i32
        17, i64, i64
        I128::from(17), i128, I128
        17, u8, u8
        17, u16, u16
        17, u32, u32
        17, u64, u64
        U128::from(17), u128, U128
        17.23, f32, f32
        17.23, f64, f64
    }

    #[test]
    #[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
    fn dec() {
        use roc_std::RocDec;

        assert_evals_to!(
            &formatdoc!(
                r#"
                app "test" provides [main] to "./platform"

                {TAG_LEN_ENCODER_FMT}

                main =
                    when Str.to_utf8 "n17.23 " |> Decode.from_bytes tag_len_fmt is
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
    crate::helpers::with_larger_debug_stack(|| {
        assert_evals_to!(
            &formatdoc!(
                r#"
                app "test" provides [main] to "./platform"

                {TAG_LEN_ENCODER_FMT}

                main =
                    when Str.to_utf8 "l3 s1 a s1 b s1 c " |> Decode.from_bytes tag_len_fmt is
                        Ok l -> Str.join_with l ","
                        _ -> "<bad>"
                "#
            ),
            RocStr::from("a,b,c"),
            RocStr
        )
    });
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn encode_then_decode_list_of_strings() {
    crate::helpers::with_larger_debug_stack(|| {
        assert_evals_to!(
            &formatdoc!(
                r#"
                app "test" provides [main] to "./platform"

                {TAG_LEN_ENCODER_FMT}

                main =
                    when Encode.to_bytes ["a", "b", "c"] tag_len_fmt |> Decode.from_bytes tag_len_fmt is
                        Ok l -> Str.join_with l ","
                        _ -> "something went wrong"
                "#
            ),
            RocStr::from("a,b,c"),
            RocStr
        )
    });
}

#[test]
#[cfg(feature = "gen-llvm")]
fn encode_then_decode_list_of_lists_of_strings() {
    crate::helpers::with_larger_debug_stack(|| {
        assert_evals_to!(
            &formatdoc!(
                r#"
                app "test" provides [main] to "./platform"

                {TAG_LEN_ENCODER_FMT}

                main =
                    when Encode.to_bytes [["a", "b"], ["c", "d", "e"], ["f"]] tag_len_fmt |> Decode.from_bytes tag_len_fmt is
                        Ok list -> (List.map list \inner -> Str.join_with inner ",") |> Str.join_with ";"
                        _ -> "something went wrong"
                "#
            ),
            RocStr::from("a,b;c,d,e;f"),
            RocStr
        )
    })
}

#[test]
#[cfg(all(
    any(feature = "gen-llvm", feature = "gen-wasm"),
    not(debug_assertions) // https://github.com/roc-lang/roc/issues/3898
))]
fn decode_record_two_fields() {
    assert_evals_to!(
        &formatdoc!(
            r#"
            app "test" provides [main] to "./platform"

            {TAG_LEN_ENCODER_FMT}

            main =
                when Str.to_utf8 "r2 s6 second s2 cd s5 first s2 ab " |> Decode.from_bytes tag_len_fmt is
                    Ok {{first: "ab", second: "cd"}} -> "abcd"
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
        &formatdoc!(
            r#"
            app "test" provides [main] to "./platform"

            {TAG_LEN_ENCODER_FMT}

            main =
                when Str.to_utf8 "r2 s5 first s2 ab s6 second n10 " |> Decode.from_bytes tag_len_fmt is
                    Ok {{first: "ab", second: 10u8}} -> "ab10"
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
        &formatdoc!(
            r#"
            app "test" provides [main] to "./platform"

            {TAG_LEN_ENCODER_FMT}

            main =
                when Str.to_utf8 "r2 s5 first s2 ab s6 second s2 cd " |> Decode.from_bytes tag_len_fmt is
                    Ok {{first, second}} -> Str.concat first second
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
        &formatdoc!(
            r#"
            app "test" provides [main] to "./platform"

            {TAG_LEN_ENCODER_FMT}

            main =
                decoded = Str.to_utf8 "r2 s5 first s2 ab s6 second s2 cd " |> Decode.from_bytes tag_len_fmt
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
        &formatdoc!(
            r#"
            app "test" provides [main] to "./platform"

            {TAG_LEN_ENCODER_FMT}

            main =
                decoded = Str.to_utf8 "r2 s5 first s2 ab s6 second s2 cd " |> Decode.from_bytes tag_len_fmt
                when decoded is
                    Ok {{first, second}} -> Str.concat first second
                    _ -> "something went wrong"
            "#
        ),
        RocStr::from("abcd"),
        RocStr
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn decode_empty_record() {
    assert_evals_to!(
        &formatdoc!(
            r#"
            app "test" provides [main] to "./platform"

            {TAG_LEN_ENCODER_FMT}

            main =
                when Str.to_utf8 "r0 " |> Decode.from_bytes tag_len_fmt is
                    Ok {{}} -> "empty"
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
        &formatdoc!(
            r#"
            app "test" provides [main] to "./platform"

            {TAG_LEN_ENCODER_FMT}

            main =
                when Str.to_utf8 "r2 s5 other r2 s3 one s1 b s3 two n10 s5 outer r1 s5 inner s1 a " |> Decode.from_bytes tag_len_fmt is
                    Ok {{outer: {{inner: "a"}}, other: {{one: "b", two: 10u8}}}} -> "ab10"
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
fn decode_tuple_two_elements() {
    assert_evals_to!(
        &formatdoc!(
            r#"
            app "test" provides [main] to "./platform"

            {TAG_LEN_ENCODER_FMT}

            main =
                when Str.to_utf8 "l2 s2 ab n10 " |> Decode.from_bytes tag_len_fmt is
                    Ok ("ab", 10u8) -> "abcd"
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
fn decode_tuple_of_tuples() {
    assert_evals_to!(
        &formatdoc!(
            r#"
            app "test" provides [main] to "./platform"

            {TAG_LEN_ENCODER_FMT}

            main =
                when Str.to_utf8 "l2 l2 s2 ab n10 l2 s2 cd n25 " |> Decode.from_bytes tag_len_fmt is
                    Ok ( ("ab", 10u8), ("cd", 25u8) ) -> "abcd"
                    _ -> "something went wrong"
            "#
        ),
        RocStr::from("abcd"),
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
        r"
        THasher := List U8 implements [Hasher {
            add_bytes: t_add_bytes,
            add_u8: t_add_u8,
            add_u16: t_add_u16,
            add_u32: t_add_u32,
            add_u64: t_add_u64,
            add_u128: t_add_u128,
            complete: t_complete,
        }]

        # ignores endian-ness
        byte_at = \n, shift ->
            Num.bitwise_and (Num.shift_right_by n (shift * 8)) 0xFF
            |> Num.to_u8

        do8 = \total, n ->
            total
            |> List.append (byte_at n 0)

        do16 = \total, n ->
            total
            |> do8 (n |> Num.to_u8)
            |> do8 (Num.shift_right_by n 8 |> Num.to_u8)

        do32 = \total, n ->
            total
            |> do16 (n |> Num.to_u16)
            |> do16 (Num.shift_right_by n 16 |> Num.to_u16)

        do64 = \total, n ->
            total
            |> do32 (n |> Num.to_u32)
            |> do32 (Num.shift_right_by n 32 |> Num.to_u32)

        do128 = \total, n ->
            total
            |> do64 (n |> Num.to_u64)
            |> do64 (Num.shift_right_by n 64 |> Num.to_u64)

        t_add_bytes = \@THasher total, bytes -> @THasher (List.concat total bytes)
        t_add_u8 = \@THasher total, n -> @THasher (do8 total n)
        t_add_u16 = \@THasher total, n -> @THasher (do16 total n)
        t_add_u32 = \@THasher total, n -> @THasher (do32 total n)
        t_add_u64 = \@THasher total, n -> @THasher (do64 total n)
        t_add_u128 = \@THasher total, n -> @THasher (do128 total n)
        t_complete = \@THasher _ -> Num.max_u64

        t_read = \@THasher bytes -> bytes
        "
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
                    |> t_read
                "#
            ),
            TEST_HASHER, input,
        )
    }

    mod immediate {
        use super::{assert_evals_to, build_test};
        use roc_std::RocList;

        #[test]
        fn bool_false() {
            assert_evals_to!(
                &build_test("Bool.false"),
                RocList::from_slice(&[0]),
                RocList<u8>
            )
        }

        #[test]
        fn bool_true() {
            assert_evals_to!(
                &build_test("Bool.true"),
                RocList::from_slice(&[1]),
                RocList<u8>
            )
        }

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
                &build_test("Num.max_u16 - 1"),
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
                &build_test("Num.max_u32 - 1"),
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
                &build_test("Num.max_u64 - 1"),
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
                &build_test("Num.max_u128 - 1"),
                RocList::from_slice(&[
                    254, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255
                ]),
                RocList<u8>
            )
        }

        #[test]
        #[cfg(not(feature = "gen-wasm"))] // shr not implemented for U128
        fn dec() {
            assert_evals_to!(
                &build_test("1.1dec"),
                RocList::from_slice(&[0, 0, 238, 4, 44, 252, 67, 15, 0, 0, 0, 0, 0, 0, 0, 0]),
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
                &build_test(r"[15u8, 23u8, 37u8]"),
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
                &build_test(r"{}"),
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
                    r"{ a: [ { b: 15u8 }, { b: 23u8 } ], b: [ { c: 45u8 }, { c: 73u8 } ] }"
                ),
                RocList::from_slice(&[15, 23, 45, 73]),
                RocList<u8>
            )
        }

        #[test]
        fn tuple_of_u8_and_str() {
            assert_evals_to!(
                &build_test(r#"(15u8, "bc")"#),
                RocList::from_slice(&[15, 98, 99]),
                RocList<u8>
            )
        }

        #[test]
        fn tuple_of_tuples() {
            assert_evals_to!(
                &build_test(r#"( (15u8, "bc"), (23u8, "ef") )"#),
                RocList::from_slice(&[15, 98, 99, 23, 101, 102]),
                RocList<u8>
            )
        }

        #[test]
        fn tuple_of_list_of_tuples() {
            assert_evals_to!(
                &build_test(
                    r"( [ ( 15u8, 32u8 ), ( 23u8, 41u8 ) ], [ (45u8, 63u8), (58u8, 73u8) ] )"
                ),
                RocList::from_slice(&[15, 32, 23, 41, 45, 63, 58, 73]),
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
                            |> t_read
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
                            |> t_read
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
                            |> t_read
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
                            |> t_read
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
                            |> t_read
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
                            |> t_read
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
                            |> t_read
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

                        Q := {{ a: U8, b: U8, c: U8 }} implements [Hash]

                        q = @Q {{ a: 15, b: 27, c: 31 }}

                        main =
                            @THasher []
                            |> Hash.hash q
                            |> t_read
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
    fn eq_tuple() {
        assert_evals_to!(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                main =
                    ("a", "b") == ("a", "b")
                "#
            ),
            true,
            bool
        )
    }

    #[test]
    fn custom_eq_impl() {
        assert_evals_to!(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                LyingEq := U8 implements [Eq {is_eq}]

                is_eq = \@LyingEq m, @LyingEq n -> m != n

                main =
                    a = @LyingEq 10
                    b = @LyingEq 5
                    c = @LyingEq 5
                    if Bool.is_eq a b && !(Bool.is_eq b c) then
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

                Q := ({} -> Str) implements [Eq {is_eq: is_eq_q}]

                is_eq_q = \@Q _, @Q _ -> Bool.true

                main = is_eq_q (@Q \{} -> "a") (@Q \{} -> "a")
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

                Q := ({} -> Str) implements [Eq {is_eq: is_eq_q}]

                is_eq_q = \@Q f1, @Q f2 -> (f1 {} == f2 {})

                main = is_eq_q (@Q \{} -> "a") (@Q \{} -> "a")
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

                main = Bool.is_eq 10u8 10u8
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

                Q := U8 implements [Eq]

                main = (@Q 15) == (@Q 15)
                "#
            ),
            true,
            bool
        )
    }
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn issue_4772_weakened_monomorphic_destructure() {
    crate::helpers::with_larger_debug_stack(|| {
        assert_evals_to!(
            &formatdoc!(
                r#"
                app "test" provides [main] to "./platform"

                {TAG_LEN_ENCODER_FMT}

                get_number =
                    {{ result, rest }} = Decode.from_bytes_partial (Str.to_utf8 "s4 1234 ") tag_len_fmt

                    when result is
                        Ok val ->
                            when Str.to_i64 val is
                                Ok number ->
                                    Ok {{ val : number, input : rest }}
                                Err InvalidNumStr ->
                                    Err (ParsingFailure "not a number")

                        Err _ ->
                            Err (ParsingFailure "not a number")

                main =
                    get_number |> Result.map_ok .val |> Result.with_default 0
                "#
            ),
            1234i64,
            i64
        )
    })
}

mod inspect {
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
    fn bool() {
        assert_evals_to!(
            indoc!(
                r#"
            app "test" provides [main] to "./platform"

            main = [
                Inspect.to_str Bool.true,
                Inspect.to_str Bool.false,
            ] |> Str.join_with ", "
            "#
            ),
            RocStr::from("Bool.true, Bool.false"),
            RocStr
        );
    }

    #[test]
    #[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
    fn num() {
        assert_evals_to!(
            indoc!(
                r#"
            app "test" provides [main] to "./platform"

            main = [
                Inspect.to_str 0,              # Num a
                Inspect.to_str 1u8,            # U8
                Inspect.to_str 2i8,            # I8
                Inspect.to_str 3u16,           # U16
                Inspect.to_str 4i16,           # I16
                Inspect.to_str 5u32,           # U32
                Inspect.to_str 6i32,           # I32
                Inspect.to_str 7u64,           # U64
                Inspect.to_str 8i64,           # I64
                Inspect.to_str 9u128,          # U128
                Inspect.to_str 10i128,         # I128
                Inspect.to_str 0.5,            # Frac a
                Inspect.to_str 1.5f32,         # F32
                Inspect.to_str 2.2f64,         # F64
                Inspect.to_str (1.1dec + 2.2), # Dec
            ] |> Str.join_with ", "
            "#
            ),
            RocStr::from("0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 0.5, 1.5, 2.2, 3.3"),
            RocStr
        );
    }

    #[test]
    #[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
    fn list() {
        assert_evals_to!(
            indoc!(
                r#"
            app "test" provides [main] to "./platform"

            main = [
                Inspect.to_str [0, 1, 2],        # List (Num *)
                Inspect.to_str [1, 0x2, 3],      # List (Int *)
                Inspect.to_str [0.1 + 0.2, 0.4], # List (Frac *)
                Inspect.to_str [1u8, 2u8],       # List U8
                Inspect.to_str ["foo"],          # List Str
            ] |> Str.join_with ", "
            "#
            ),
            RocStr::from("[0, 1, 2], [1, 2, 3], [0.3, 0.4], [1, 2], [\"foo\"]"),
            RocStr
        );
    }

    #[test]
    #[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
    fn str() {
        assert_evals_to!(
            indoc!(
                r#"
            app "test" provides [main] to "./platform"

            main = [
                Inspect.to_str "",
                Inspect.to_str "a small string",
                Inspect.to_str "an extraordinarily long string - so long it's on the heap!",
            ] |> Str.join_with ", "
            "#
            ),
            RocStr::from(
                r#""", "a small string", "an extraordinarily long string - so long it's on the heap!""#
            ),
            RocStr
        );
    }

    #[test]
    #[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
    fn opaque_automatic() {
        assert_evals_to!(
            indoc!(
                r#"
            app "test" provides [main] to "./platform"

            Op := {}

            main = Inspect.to_str (@Op {})
            "#
            ),
            RocStr::from(r"<opaque>"),
            RocStr
        );
    }

    #[test]
    #[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
    fn opaque_automatic_with_polymorphic_call() {
        assert_evals_to!(
            indoc!(
                r#"
            app "test" provides [main] to "./platform"

            Op := {}

            late = \a -> Inspect.to_str a

            main = late (@Op {})
            "#
            ),
            RocStr::from(r"<opaque>"),
            RocStr
        );
    }
}
