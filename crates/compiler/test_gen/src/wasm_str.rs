// Wasm pointers are only 32bit. This effects RocStr.
// These are versions of the str tests assuming 32bit pointers.
#![cfg(feature = "gen-wasm")]

// TODO: We need to make these tests work with the llvm wasm backend.

// #[cfg(feature = "gen-llvm")]
// use crate::helpers::llvm::assert_wasm_evals_to as assert_evals_to;

#[cfg(feature = "gen-wasm")]
use crate::helpers::wasm::assert_evals_to;

#[allow(unused_imports)]
use indoc::indoc;
use roc_std::{RocList, RocStr, I128, U128};

#[test]
fn str_split_on_empty_delimiter() {
    assert_evals_to!(
        indoc!(
            r#"
                List.len (Str.split_on "hello" "")
            "#
        ),
        1,
        u64
    );
}

#[test]
fn str_split_on_bigger_delimiter_small_str() {
    assert_evals_to!(
        indoc!(
            r#"
                List.len (Str.split_on "hello" "JJJJ there")
            "#
        ),
        1,
        u64
    );
}

#[test]
fn str_split_on_str_concat_repeated() {
    assert_evals_to!(
        indoc!(
            r#"
                when List.first (Str.split_on "JJJJJ" "JJJJ there") is
                    Ok str ->
                        str
                            |> Str.concat str
                            |> Str.concat str
                            |> Str.concat str
                            |> Str.concat str

                    _ ->
                        "Not Str!"

            "#
        ),
        RocStr::from("JJJJJJJJJJJJJJJJJJJJJJJJJ"),
        RocStr
    );
}

#[test]
fn str_split_on_small_str_bigger_delimiter() {
    assert_evals_to!(
        indoc!(
            r#"
                when
                    List.first
                        (Str.split_on "JJJ" "0123456789abcdefghi")
                is
                    Ok str -> str
                    _ -> ""
            "#
        ),
        RocStr::from("JJJ"),
        RocStr
    );
}

#[test]
fn str_split_on_big_str_small_delimiter() {
    assert_evals_to!(
        indoc!(
            r#"
                Str.split_on "01234567789abcdefghi?01234567789abcdefghi" "?"
            "#
        ),
        RocList::from_slice(&[
            RocStr::from("01234567789abcdefghi"),
            RocStr::from("01234567789abcdefghi")
        ]),
        RocList<RocStr>
    );

    assert_evals_to!(
        indoc!(
            r#"
                Str.split_on "01234567789abcdefghi 3ch 01234567789abcdefghi" "3ch"
            "#
        ),
        RocList::from_slice(&[
            RocStr::from("01234567789abcdefghi "),
            RocStr::from(" 01234567789abcdefghi")
        ]),
        RocList<RocStr>
    );
}

#[test]
fn str_split_on_small_str_small_delimiter() {
    assert_evals_to!(
        indoc!(
            r#"
                Str.split_on "J!J!J" "!"
            "#
        ),
        RocList::from_slice(&[RocStr::from("J"), RocStr::from("J"), RocStr::from("J")]),
        RocList<RocStr>
    );
}

#[test]
fn str_split_on_bigger_delimiter_big_strs() {
    assert_evals_to!(
        indoc!(
            r#"
                Str.split_on
                    "string to split is shorter"
                    "than the delimiter which happens to be very very long"
            "#
        ),
        RocList::from_slice(&[RocStr::from("string to split is shorter")]),
        RocList<RocStr>
    );
}

#[test]
fn str_split_on_empty_strs() {
    assert_evals_to!(
        indoc!(
            r#"
                    Str.split_on "" ""
                "#
        ),
        RocList::from_slice(&[RocStr::from("")]),
        RocList<RocStr>
    );
}

#[test]
fn str_split_on_minimal_example() {
    assert_evals_to!(
        indoc!(
            r#"
                    Str.split_on "a," ","
                "#
        ),
        RocList::from_slice(&[RocStr::from("a"), RocStr::from("")]),
        RocList<RocStr>
    )
}

#[test]
fn str_split_on_small_str_big_delimiter() {
    assert_evals_to!(
        indoc!(
            r#"
                    Str.split_on
                        "1---- ---- ---- ---- ----2---- ---- ---- ---- ----"
                        "---- ---- ---- ---- ----"
                        |> List.len
                "#
        ),
        3,
        u64
    );

    assert_evals_to!(
        indoc!(
            r#"
                    Str.split_on
                        "1---- ---- ---- ---- ----2---- ---- ---- ---- ----"
                        "---- ---- ---- ---- ----"
                "#
        ),
        RocList::from_slice(&[RocStr::from("1"), RocStr::from("2"), RocStr::from("")]),
        RocList<RocStr>
    );
}

#[test]
fn str_split_on_small_str_20_char_delimiter() {
    assert_evals_to!(
        indoc!(
            r#"
                    Str.split_on
                        "3|-- -- -- -- -- -- |4|-- -- -- -- -- -- |"
                        "|-- -- -- -- -- -- |"
                "#
        ),
        RocList::from_slice(&[RocStr::from("3"), RocStr::from("4"), RocStr::from("")]),
        RocList<RocStr>
    );
}

#[test]
fn str_concat_big_to_big() {
    assert_evals_to!(
            indoc!(
                r#"
                    Str.concat
                        "First string that is fairly long. Longer strings make for different errors. "
                        "Second string that is also fairly long. Two long strings test things that might not appear with short strings."
                "#
            ),
            RocStr::from("First string that is fairly long. Longer strings make for different errors. Second string that is also fairly long. Two long strings test things that might not appear with short strings."),
            RocStr
        );
}

#[test]
#[cfg(feature = "gen-wasm")]
fn small_str_literal() {
    assert_evals_to!(
        "\"01234567890\"",
        [0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x30, 0x8b],
        [u8; 12]
    );
}

#[test]
#[cfg(feature = "gen-wasm")]
fn small_str_zeroed_literal() {
    // Verifies that we zero out unused bytes in the string.
    // This is important so that string equality tests don't randomly
    // fail due to unused memory being there!
    // Note: Ensure the memory is non-zero beforehand, or it's not a real test!
    // (It's trickier than it sounds!)
    assert_evals_to!(
        indoc!(
            r#"
                app "test" provides [main] to "./platform"

                create_str = \is_for_real_this_time ->
                    if is_for_real_this_time then
                        "J"
                    else
                        "xxxxxxx"

                function_with_reused_space = \is_for_real_this_time ->
                    # Different string value on each call, at the same memory location
                    # (Can't inline create_str without refcounting, which isn't implemented)
                    reused_space = create_str is_for_real_this_time

                    # Unoptimised 'if' ensures that we don't just allocate in the caller's frame
                    if Bool.true then
                        reused_space
                    else
                        reused_space

                main =
                    garbage = function_with_reused_space Bool.false
                    function_with_reused_space Bool.true
                 "#
        ),
        [
            0x4a,
            0x00,
            0x00,
            0x00,
            0x00,
            0x00,
            0x00,
            0x00,
            0x00,
            0x00,
            0x00,
            0b1000_0001
        ],
        [u8; 12]
    );
}

#[test]
fn long_str_literal() {
    assert_evals_to!(
        "\"0123456789 123456789 123456789\"",
        RocStr::from("0123456789 123456789 123456789"),
        RocStr
    );
}

#[test]
fn small_str_concat_empty_first_arg() {
    assert_evals_to!(
        r#"Str.concat "" "01234567890""#,
        [0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x30, 0x8b],
        [u8; 12]
    );
}

#[test]
fn small_str_concat_empty_second_arg() {
    assert_evals_to!(
        r#"Str.concat "01234567890" """#,
        [0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x30, 0x8b],
        [u8; 12]
    );
}

#[test]
fn small_str_concat_small_to_big() {
    assert_evals_to!(
        r#"Str.concat "abc" " this is longer than 7 chars""#,
        RocStr::from("abc this is longer than 7 chars"),
        RocStr
    );
}

#[test]
fn small_str_concat_small_to_small_staying_small() {
    assert_evals_to!(
        r#"Str.concat "0" "1234567890""#,
        [0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x30, 0x8b],
        [u8; 12]
    );
}

#[test]
fn small_str_concat_small_to_small_overflow_to_big() {
    assert_evals_to!(
        r#"Str.concat "abcdefg" "hijklmn""#,
        RocStr::from("abcdefghijklmn"),
        RocStr
    );
}

#[test]
fn str_concat_empty() {
    assert_evals_to!(r#"Str.concat "" """#, RocStr::default(), RocStr);
}

#[test]
fn small_str_is_empty() {
    assert_evals_to!(r#"Str.is_empty "abc""#, false, bool);
}

#[test]
fn big_str_is_empty() {
    assert_evals_to!(
        r#"Str.is_empty "this is more than 15 chars long""#,
        false,
        bool
    );
}

#[test]
fn empty_str_is_empty() {
    assert_evals_to!(r#"Str.is_empty """#, true, bool);
}

#[test]
fn str_starts_with() {
    assert_evals_to!(r#"Str.starts_with "hello world" "hell""#, true, bool);
    assert_evals_to!(r#"Str.starts_with "hello world" """#, true, bool);
    assert_evals_to!(r#"Str.starts_with "nope" "hello world""#, false, bool);
    assert_evals_to!(r#"Str.starts_with "hell" "hello world""#, false, bool);
    assert_evals_to!(r#"Str.starts_with "" "hello world""#, false, bool);
}

#[test]
fn str_ends_with() {
    assert_evals_to!(r#"Str.ends_with "hello world" "world""#, true, bool);
    assert_evals_to!(r#"Str.ends_with "nope" "hello world""#, false, bool);
    assert_evals_to!(r#"Str.ends_with "" "hello world""#, false, bool);
}

#[test]
fn str_starts_with_same_big_str() {
    assert_evals_to!(
        r#"Str.starts_with "123456789123456789" "123456789123456789""#,
        true,
        bool
    );
}

#[test]
fn str_starts_with_different_big_str() {
    assert_evals_to!(
        r#"Str.starts_with "12345678912345678910" "123456789123456789""#,
        true,
        bool
    );
}

#[test]
fn str_starts_with_same_small_str() {
    assert_evals_to!(r#"Str.starts_with "1234" "1234""#, true, bool);
}

#[test]
fn str_starts_with_different_small_str() {
    assert_evals_to!(r#"Str.starts_with "1234" "12""#, true, bool);
}
#[test]
fn str_starts_with_false_small_str() {
    assert_evals_to!(r#"Str.starts_with "1234" "23""#, false, bool);
}

#[test]
fn str_from_utf8_pass_single_ascii() {
    assert_evals_to!(
        indoc!(
            r#"
                    when Str.from_utf8 [97] is
                        Ok val -> val
                        Err _ -> ""
                "#
        ),
        roc_std::RocStr::from("a"),
        roc_std::RocStr
    );
}

#[test]
fn str_from_utf8_pass_many_ascii() {
    assert_evals_to!(
        indoc!(
            r#"
                    when Str.from_utf8 [97, 98, 99, 0x7E] is
                        Ok val -> val
                        Err _ -> ""
                "#
        ),
        roc_std::RocStr::from("abc~"),
        roc_std::RocStr
    );
}

#[test]
fn str_from_utf8_pass_single_unicode() {
    assert_evals_to!(
        indoc!(
            r#"
                    when Str.from_utf8 [0xE2, 0x88, 0x86] is
                        Ok val -> val
                        Err _ -> ""
                "#
        ),
        roc_std::RocStr::from("∆"),
        roc_std::RocStr
    );
}

#[test]
fn str_from_utf8_pass_many_unicode() {
    assert_evals_to!(
        indoc!(
            r#"
                    when Str.from_utf8 [0xE2, 0x88, 0x86, 0xC5, 0x93, 0xC2, 0xAC] is
                        Ok val -> val
                        Err _ -> ""
                "#
        ),
        roc_std::RocStr::from("∆œ¬"),
        roc_std::RocStr
    );
}

#[test]
fn str_from_utf8_pass_single_grapheme() {
    assert_evals_to!(
        indoc!(
            r#"
                    when Str.from_utf8 [0xF0, 0x9F, 0x92, 0x96] is
                        Ok val -> val
                        Err _ -> ""
                "#
        ),
        roc_std::RocStr::from("💖"),
        roc_std::RocStr
    );
}

#[test]
fn str_from_utf8_pass_many_grapheme() {
    assert_evals_to!(
        indoc!(
            r#"
                    when Str.from_utf8 [0xF0, 0x9F, 0x92, 0x96, 0xF0, 0x9F, 0xA4, 0xA0, 0xF0, 0x9F, 0x9A, 0x80] is
                        Ok val -> val
                        Err _ -> ""
                "#
        ),
        roc_std::RocStr::from("💖🤠🚀"),
        roc_std::RocStr
    );
}

#[test]
fn str_from_utf8_pass_all() {
    assert_evals_to!(
        indoc!(
            r#"
                    when Str.from_utf8 [0xF0, 0x9F, 0x92, 0x96, 98, 0xE2, 0x88, 0x86] is
                        Ok val -> val
                        Err _ -> ""
                "#
        ),
        roc_std::RocStr::from("💖b∆"),
        roc_std::RocStr
    );
}

#[test]
fn str_from_utf8_fail_invalid_start_byte() {
    assert_evals_to!(
        indoc!(
            r#"
                    when Str.from_utf8 [97, 98, 0x80, 99] is
                        Err (BadUtf8 {problem: InvalidStartByte, index: byte_index}) ->
                            if byte_index == 2 then
                                "a"
                            else
                                "b"
                        _ -> ""
                "#
        ),
        roc_std::RocStr::from("a"),
        roc_std::RocStr
    );
}

#[test]
fn str_from_utf8_fail_unexpected_end_of_sequence() {
    assert_evals_to!(
        indoc!(
            r#"
                    when Str.from_utf8 [97, 98, 99, 0xC2] is
                        Err (BadUtf8 {problem: UnexpectedEndOfSequence, index: byte_index}) ->
                            if byte_index == 3 then
                                "a"
                            else
                                "b"
                        _ -> ""
                "#
        ),
        roc_std::RocStr::from("a"),
        roc_std::RocStr
    );
}

#[test]
fn str_from_utf8_fail_expected_continuation() {
    assert_evals_to!(
        indoc!(
            r#"
                    when Str.from_utf8 [97, 98, 99, 0xC2, 0x00] is
                        Err (BadUtf8 {problem: ExpectedContinuation, index: byte_index}) ->
                            if byte_index == 3 then
                                "a"
                            else
                                "b"
                        _ -> ""
                "#
        ),
        roc_std::RocStr::from("a"),
        roc_std::RocStr
    );
}

#[test]
fn str_from_utf8_fail_overlong_encoding() {
    assert_evals_to!(
        indoc!(
            r#"
                    when Str.from_utf8 [97, 0xF0, 0x80, 0x80, 0x80] is
                        Err (BadUtf8 {problem: OverlongEncoding, index: byte_index}) ->
                            if byte_index == 1 then
                                "a"
                            else
                                "b"
                        _ -> ""
                "#
        ),
        roc_std::RocStr::from("a"),
        roc_std::RocStr
    );
}

#[test]
fn str_from_utf8_fail_codepoint_too_large() {
    assert_evals_to!(
        indoc!(
            r#"
                    when Str.from_utf8 [97, 0xF4, 0x90, 0x80, 0x80] is
                        Err (BadUtf8 {problem: CodepointTooLarge, index: byte_index}) ->
                            if byte_index == 1 then
                                "a"
                            else
                                "b"
                        _ -> ""
                "#
        ),
        roc_std::RocStr::from("a"),
        roc_std::RocStr
    );
}

#[test]
fn str_from_utf8_fail_surrogate_half() {
    assert_evals_to!(
        indoc!(
            r#"
                    when Str.from_utf8 [97, 98, 0xED, 0xA0, 0x80] is
                        Err (BadUtf8 {problem: EncodesSurrogateHalf, index: byte_index}) ->
                            if byte_index == 2 then
                                "a"
                            else
                                "b"
                        _ -> ""
                "#
        ),
        roc_std::RocStr::from("a"),
        roc_std::RocStr
    );
}

#[test]
fn str_from_utf8_lossy_expected_continuation() {
    assert_evals_to!(
        r#"Str.from_utf8_lossy [97, 98, 0xC2, 99]"#,
        roc_std::RocStr::from("ab�c"),
        roc_std::RocStr
    );
}

#[test]
fn str_from_utf16() {
    assert_evals_to!(
        indoc!(
            r#"
                when Str.from_utf16 [0x72, 0x6f, 0x63] is
                    Ok val -> val
                    _ -> ""
            "#
        ),
        roc_std::RocStr::from("roc"),
        roc_std::RocStr
    )
}

// Marking this as should_panic, because it *does* panic and it is not clear why?
// If some change magically fixes this, great, remove the should_panic attribute.
#[test]
#[should_panic(expected = r#"Roc failed with message: "Integer multiplication overflowed!"#)]
fn str_from_utf16_emoji() {
    assert_evals_to!(
        indoc!(
            r#"
                when Str.from_utf16 [0x72, 0xd83d, 0xdc96, 0x63] is
                    Ok val -> val
                    _ -> ""
            "#
        ),
        roc_std::RocStr::from("r💖c"),
        roc_std::RocStr
    )
}

#[test]
fn str_from_utf16_err_expected_second_surrogate_half() {
    assert_evals_to!(
        indoc!(
            r#"
                when Str.from_utf16 [0x72, 0xd83d, 0x63] is
                    Err (BadUtf16 {problem: EncodesSurrogateHalf, index: index }) -> index
                    _ -> 42
            "#
        ),
        1u64,
        u64
    )
}

#[test]
fn str_from_utf16_err_unexpected_second_surrogate_half() {
    assert_evals_to!(
        indoc!(
            r#"
                when Str.from_utf16 [0x72, 0xdc96, 0x63] is
                    Err (BadUtf16 {problem: EncodesSurrogateHalf, index: index }) -> index
                    _ -> 42
            "#
        ),
        1u64,
        u64
    )
}

#[test]
fn str_from_utf16_lossy() {
    assert_evals_to!(
        r#"Str.from_utf16_lossy [0x72, 0xdc96, 0x63]"#,
        roc_std::RocStr::from("r�c"),
        roc_std::RocStr
    )
}

#[test]
fn str_from_utf32() {
    assert_evals_to!(
        indoc!(
            r#"
                when Str.from_utf32 [0x72, 0x6f, 0x63] is
                    Ok val -> val
                    _ -> ""
            "#
        ),
        roc_std::RocStr::from("roc"),
        roc_std::RocStr
    )
}

#[test]
fn str_from_utf32_emoji() {
    assert_evals_to!(
        indoc!(
            r#"
                when Str.from_utf32 [0x72, 0x1f496, 0x63] is
                    Ok val -> val
                    _ -> ""
            "#
        ),
        roc_std::RocStr::from("r💖c"),
        roc_std::RocStr
    )
}

#[test]
fn str_from_utf32_err_codepoint_too_large() {
    assert_evals_to!(
        indoc!(
            r#"
                when Str.from_utf32 [0x72, 0x123456, 0x63] is
                    Err (BadUtf32 {problem: CodepointTooLarge, index: index }) -> index
                    _ -> 42
            "#
        ),
        1u64,
        u64
    )
}

#[test]
fn str_from_utf32_err_utf8_cannot_encode_surrogate_half() {
    assert_evals_to!(
        indoc!(
            r#"
                when Str.from_utf32 [0x72, 0xd83d, 0x63] is
                    Err (BadUtf32 {problem: EncodesSurrogateHalf, index: index }) -> index
                    _ -> 42
            "#
        ),
        1u64,
        u64
    )
}

#[test]
fn str_from_utf32_lossy() {
    assert_evals_to!(
        r#"Str.from_utf32_lossy [0x72, 0x123456, 0x63]"#,
        roc_std::RocStr::from("r�c"),
        roc_std::RocStr
    )
}

#[test]
fn str_equality() {
    assert_evals_to!(r#""a" == "a""#, true, bool);
    assert_evals_to!(
        r#""loremipsumdolarsitamet" == "loremipsumdolarsitamet""#,
        true,
        bool
    );
    assert_evals_to!(r#""a" != "b""#, true, bool);
    assert_evals_to!(r#""a" == "b""#, false, bool);
}

#[test]
fn str_join_comma_small() {
    assert_evals_to!(
        r#"Str.join_with ["1", "2"] ", " "#,
        RocStr::from("1, 2"),
        RocStr
    );
}

#[test]
fn str_join_comma_big() {
    assert_evals_to!(
        r#"Str.join_with ["10000000", "2000000", "30000000"] ", " "#,
        RocStr::from("10000000, 2000000, 30000000"),
        RocStr
    );
}

#[test]
fn str_join_comma_single() {
    assert_evals_to!(r#"Str.join_with ["1"] ", " "#, RocStr::from("1"), RocStr);
}

#[test]
fn str_to_utf8() {
    assert_evals_to!(
        r#"Str.to_utf8 "hello""#,
        RocList::from_slice(&[104, 101, 108, 108, 111]),
        RocList<u8>
    );
    assert_evals_to!(
        r#"Str.to_utf8 "this is a long string""#,
        RocList::from_slice(&[
            116, 104, 105, 115, 32, 105, 115, 32, 97, 32, 108, 111, 110, 103, 32, 115, 116, 114,
            105, 110, 103
        ]),
        RocList<u8>
    );
}

#[test]
fn str_from_utf8() {
    assert_evals_to!(
        indoc!(
            r#"
            bytes =
                Str.to_utf8 "hello"

            when Str.from_utf8 bytes is
                   Ok utf8_string -> utf8_string
                   _ -> ""
            "#
        ),
        RocStr::from("hello"),
        RocStr
    );
}

#[test]
fn str_from_utf8_slice() {
    assert_evals_to!(
        indoc!(
            r#"
            bytes =
                Str.to_utf8 "hello"
                |> List.sublist { start: 1, len: 4 }

            when Str.from_utf8 bytes is
                   Ok utf8_string -> utf8_string
                   _ -> ""
            "#
        ),
        RocStr::from("ello"),
        RocStr
    );
}

#[test]
fn str_from_utf8_slice_not_end() {
    assert_evals_to!(
        indoc!(
            r#"
            bytes =
                Str.to_utf8 "hello"
                |> List.sublist { start: 1, len: 3 }

            when Str.from_utf8 bytes is
                   Ok utf8_string -> utf8_string
                   _ -> ""
            "#
        ),
        RocStr::from("ell"),
        RocStr
    );
}

#[test]
fn str_from_utf8_order_does_not_matter() {
    assert_evals_to!(
        indoc!(
            r#"
            bytes =
                Str.to_utf8 "hello"
                |> List.sublist { start: 1, len: 3 }

            when Str.from_utf8 bytes is
                   Ok utf8_string -> utf8_string
                   _ -> ""
            "#
        ),
        RocStr::from("ell"),
        RocStr
    );
}

#[test]
fn str_repeat_small() {
    assert_evals_to!(
        indoc!(r#"Str.repeat "Roc" 2"#),
        RocStr::from("RocRoc"),
        RocStr
    );
}

#[test]
fn str_repeat_big() {
    assert_evals_to!(
        indoc!(r#"Str.repeat "more than 16 characters" 2"#),
        RocStr::from("more than 16 charactersmore than 16 characters"),
        RocStr
    );
}

#[test]
fn str_repeat_empty_string() {
    assert_evals_to!(indoc!(r#"Str.repeat "" 3"#), RocStr::from(""), RocStr);
}

#[test]
fn str_repeat_zero_times() {
    assert_evals_to!(indoc!(r#"Str.repeat "Roc" 0"#), RocStr::from(""), RocStr);
}

#[test]
fn str_trim_empty_string() {
    assert_evals_to!(indoc!(r#"Str.trim """#), RocStr::from(""), RocStr);
}

#[test]
fn str_trim_small_blank_string() {
    assert_evals_to!(indoc!(r#"Str.trim " ""#), RocStr::from(""), RocStr);
}

#[test]
fn str_trim_small_to_small() {
    assert_evals_to!(
        indoc!(r#"Str.trim " hello ""#),
        RocStr::from("hello"),
        RocStr
    );
}

#[test]
fn str_trim_large_to_large_unique() {
    assert_evals_to!(
        indoc!(r#"Str.trim (Str.concat "  " "hello world from a large string ")"#),
        RocStr::from("hello world from a large string"),
        RocStr
    );
}

#[test]
fn str_trim_large_to_small_unique() {
    assert_evals_to!(
        indoc!(r#"Str.trim (Str.concat "  " "hello        ")"#),
        RocStr::from("hello"),
        RocStr
    );
}

#[test]
fn str_trim_large_to_large_shared() {
    assert_evals_to!(
        indoc!(
            r#"
               original : Str
               original = " hello world world "

               { trimmed: Str.trim original, original: original }
               "#
        ),
        (
            RocStr::from(" hello world world "),
            RocStr::from("hello world world"),
        ),
        (RocStr, RocStr)
    );
}

#[test]
fn str_trim_large_to_small_shared() {
    assert_evals_to!(
        indoc!(
            r#"
               original : Str
               original = " hello             "

               { trimmed: Str.trim original, original: original }
               "#
        ),
        (RocStr::from(" hello             "), RocStr::from("hello"),),
        (RocStr, RocStr)
    );
}

#[test]
fn str_trim_small_to_small_shared() {
    assert_evals_to!(
        indoc!(
            r#"
               original : Str
               original = " hello "

               { trimmed: Str.trim original, original: original }
               "#
        ),
        (RocStr::from(" hello "), RocStr::from("hello"),),
        (RocStr, RocStr)
    );
}

#[test]
fn str_trim_start_small_blank_string() {
    assert_evals_to!(indoc!(r#"Str.trim_start " ""#), RocStr::from(""), RocStr);
}

#[test]
fn str_trim_start_small_to_small() {
    assert_evals_to!(
        indoc!(r#"Str.trim_start "  hello  ""#),
        RocStr::from("hello  "),
        RocStr
    );
}

#[test]
fn str_trim_start_large_to_large_unique() {
    assert_evals_to!(
        indoc!(r#"Str.trim_start (Str.concat "    " "hello world from a large string ")"#),
        RocStr::from("hello world from a large string "),
        RocStr
    );
}

#[test]
fn str_trim_start_large_to_small_unique() {
    assert_evals_to!(
        indoc!(r#"Str.trim_start (Str.concat "  " "hello  ")"#),
        RocStr::from("hello  "),
        RocStr
    );
}

#[test]
fn str_trim_end_small_blank_string() {
    assert_evals_to!(indoc!(r#"Str.trim_end " ""#), RocStr::from(""), RocStr);
}

#[test]
fn str_trim_end_small_to_small() {
    assert_evals_to!(
        indoc!(r#"Str.trim_end " hello ""#),
        RocStr::from(" hello"),
        RocStr
    );
}

#[test]
fn str_trim_end_large_to_large_unique() {
    assert_evals_to!(
        indoc!(r#"Str.trim_end (Str.concat " hello world from a large string" "    ")"#),
        RocStr::from(" hello world from a large string"),
        RocStr
    );
}

#[test]
fn str_trim_end_large_to_small_unique() {
    assert_evals_to!(
        indoc!(r#"Str.trim_end (Str.concat "  hello" "  ")"#),
        RocStr::from("  hello"),
        RocStr
    );
}

#[test]
fn str_trim_end_large_to_large_shared() {
    assert_evals_to!(
        indoc!(
            r#"
               original : Str
               original = " hello world world "

               { trimmed: Str.trim_end original, original: original }
               "#
        ),
        (
            RocStr::from(" hello world world "),
            RocStr::from(" hello world world"),
        ),
        (RocStr, RocStr)
    );
}

#[test]
fn str_trim_end_large_to_small_shared() {
    assert_evals_to!(
        indoc!(
            r#"
               original : Str
               original = "  hello "

               { trimmed: Str.trim_end original, original: original }
               "#
        ),
        (RocStr::from("  hello "), RocStr::from("  hello"),),
        (RocStr, RocStr)
    );
}

#[test]
fn str_trim_end_small_to_small_shared() {
    assert_evals_to!(
        indoc!(
            r#"
               original : Str
               original = " hello "

               { trimmed: Str.trim_end original, original: original }
               "#
        ),
        (RocStr::from(" hello "), RocStr::from(" hello"),),
        (RocStr, RocStr)
    );
}

#[test]
fn str_to_i128() {
    assert_evals_to!(
        indoc!(
            r#"
             when Str.to_i128 "1" is
                 Ok n -> n
                 Err _ -> 0
                "#
        ),
        I128::from(1),
        I128
    );
}

#[test]
fn str_to_u128() {
    assert_evals_to!(
        indoc!(
            r#"
             when Str.to_u128 "1" is
                 Ok n -> n
                 Err _ -> 0
                "#
        ),
        U128::from(1),
        U128
    );
}

#[test]
fn str_to_i64() {
    assert_evals_to!(
        indoc!(
            r#"
             when Str.to_i64 "1" is
                 Ok n -> n
                 Err _ -> 0
                "#
        ),
        1,
        i64
    );
}

#[test]
fn str_to_u64() {
    assert_evals_to!(
        indoc!(
            r#"
             when Str.to_u64 "1" is
                 Ok n -> n
                 Err _ -> 0
                "#
        ),
        1,
        u64
    );
}

#[test]
fn str_to_i32() {
    assert_evals_to!(
        indoc!(
            r#"
             when Str.to_i32 "1" is
                 Ok n -> n
                 Err _ -> 0
                "#
        ),
        1,
        i32
    );
}

#[test]
fn str_to_u32() {
    assert_evals_to!(
        indoc!(
            r#"
             when Str.to_u32 "1" is
                 Ok n -> n
                 Err _ -> 0
                "#
        ),
        1,
        u32
    );
}

#[test]
fn str_to_i16() {
    assert_evals_to!(
        indoc!(
            r#"
             when Str.to_i16 "1" is
                 Ok n -> n
                 Err _ -> 0
                "#
        ),
        1,
        i16
    );
}

#[test]
fn str_to_u16() {
    assert_evals_to!(
        indoc!(
            r#"
             when Str.to_u16 "1" is
                 Ok n -> n
                 Err _ -> 0
                "#
        ),
        1,
        u16
    );
}

#[test]
fn str_to_i8() {
    assert_evals_to!(
        indoc!(
            r#"
             when Str.to_i8 "1" is
                 Ok n -> n
                 Err _ -> 0
                "#
        ),
        1,
        i8
    );
}

#[test]
fn str_to_u8() {
    assert_evals_to!(
        indoc!(
            r#"
             when Str.to_u8 "1" is
                 Ok n -> n
                 Err _ -> 0
                "#
        ),
        1,
        u8
    );
}

#[test]
fn str_to_f64() {
    assert_evals_to!(
        indoc!(
            r#"
             when Str.to_f64 "1.0" is
                 Ok n -> n
                 Err _ -> 0
             "#
        ),
        1.0,
        f64
    );
}

#[test]
fn str_to_f32() {
    assert_evals_to!(
        indoc!(
            r#"
             when Str.to_f32 "1.0" is
                 Ok n -> n
                 Err _ -> 0
             "#
        ),
        1.0,
        f32
    );
}

#[test]
fn str_to_dec() {
    use roc_std::RocDec;

    assert_evals_to!(
        indoc!(
            r#"
             when Str.to_dec "1.0" is
                 Ok n -> n
                 Err _ -> 0
             "#
        ),
        RocDec::from_str("1.0").unwrap(),
        RocDec
    );
}
