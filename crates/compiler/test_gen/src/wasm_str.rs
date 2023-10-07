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
use roc_std::{RocList, RocStr};

#[test]
fn str_split_empty_delimiter() {
    assert_evals_to!(
        indoc!(
            r#"
                List.len (Str.split "hello" "")
            "#
        ),
        1,
        usize
    );
}

// This test produces an app that exposes nothing to the host!
#[test]
#[ignore]
fn str_split_empty_delimiter_broken() {
    assert_evals_to!(
        indoc!(
            r#"
                when List.first (Str.split "JJJ" "") is
                    Ok str ->
                        Str.countGraphemes str

                    _ ->
                        -1

            "#
        ),
        3,
        usize
    );
}

#[test]
fn str_split_bigger_delimiter_small_str() {
    assert_evals_to!(
        indoc!(
            r#"
                List.len (Str.split "hello" "JJJJ there")
            "#
        ),
        1,
        usize
    );
}

// This test produces an app that exposes nothing to the host!
#[test]
#[ignore]
fn str_split_bigger_delimiter_small_str_broken() {
    assert_evals_to!(
        indoc!(
            r#"
                when List.first (Str.split "JJJ" "JJJJ there") is
                    Ok str ->
                        Str.countGraphemes str

                    _ ->
                        -1

            "#
        ),
        3,
        usize
    );
}

#[test]
fn str_split_str_concat_repeated() {
    assert_evals_to!(
        indoc!(
            r#"
                when List.first (Str.split "JJJJJ" "JJJJ there") is
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
fn str_split_small_str_bigger_delimiter() {
    assert_evals_to!(
        indoc!(
            r#"
                when
                    List.first
                        (Str.split "JJJ" "0123456789abcdefghi")
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
fn str_split_big_str_small_delimiter() {
    assert_evals_to!(
        indoc!(
            r#"
                Str.split "01234567789abcdefghi?01234567789abcdefghi" "?"
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
                Str.split "01234567789abcdefghi 3ch 01234567789abcdefghi" "3ch"
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
fn str_split_small_str_small_delimiter() {
    assert_evals_to!(
        indoc!(
            r#"
                Str.split "J!J!J" "!"
            "#
        ),
        RocList::from_slice(&[RocStr::from("J"), RocStr::from("J"), RocStr::from("J")]),
        RocList<RocStr>
    );
}

#[test]
fn str_split_bigger_delimiter_big_strs() {
    assert_evals_to!(
        indoc!(
            r#"
                Str.split
                    "string to split is shorter"
                    "than the delimiter which happens to be very very long"
            "#
        ),
        RocList::from_slice(&[RocStr::from("string to split is shorter")]),
        RocList<RocStr>
    );
}

#[test]
fn str_split_empty_strs() {
    assert_evals_to!(
        indoc!(
            r#"
                    Str.split "" ""
                "#
        ),
        RocList::from_slice(&[RocStr::from("")]),
        RocList<RocStr>
    );
}

#[test]
fn str_split_minimal_example() {
    assert_evals_to!(
        indoc!(
            r#"
                    Str.split "a," ","
                "#
        ),
        RocList::from_slice(&[RocStr::from("a"), RocStr::from("")]),
        RocList<RocStr>
    )
}

#[test]
fn str_split_small_str_big_delimiter() {
    assert_evals_to!(
        indoc!(
            r#"
                    Str.split
                        "1---- ---- ---- ---- ----2---- ---- ---- ---- ----"
                        "---- ---- ---- ---- ----"
                        |> List.len
                "#
        ),
        3,
        usize
    );

    assert_evals_to!(
        indoc!(
            r#"
                    Str.split
                        "1---- ---- ---- ---- ----2---- ---- ---- ---- ----"
                        "---- ---- ---- ---- ----"
                "#
        ),
        RocList::from_slice(&[RocStr::from("1"), RocStr::from("2"), RocStr::from("")]),
        RocList<RocStr>
    );
}

#[test]
fn str_split_small_str_20_char_delimiter() {
    assert_evals_to!(
        indoc!(
            r#"
                    Str.split
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

                createStr = \isForRealThisTime ->
                    if isForRealThisTime then
                        "J"
                    else
                        "xxxxxxx"

                functionWithReusedSpace = \isForRealThisTime ->
                    # Different string value on each call, at the same memory location
                    # (Can't inline createStr without refcounting, which isn't implemented)
                    reusedSpace = createStr isForRealThisTime

                    # Unoptimised 'if' ensures that we don't just allocate in the caller's frame
                    if Bool.true then
                        reusedSpace
                    else
                        reusedSpace

                main =
                    garbage = functionWithReusedSpace Bool.false
                    functionWithReusedSpace Bool.true
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
    assert_evals_to!(r#"Str.isEmpty "abc""#, false, bool);
}

#[test]
fn big_str_is_empty() {
    assert_evals_to!(
        r#"Str.isEmpty "this is more than 15 chars long""#,
        false,
        bool
    );
}

#[test]
fn empty_str_is_empty() {
    assert_evals_to!(r#"Str.isEmpty """#, true, bool);
}

#[test]
fn str_starts_with() {
    assert_evals_to!(r#"Str.startsWith "hello world" "hell""#, true, bool);
    assert_evals_to!(r#"Str.startsWith "hello world" """#, true, bool);
    assert_evals_to!(r#"Str.startsWith "nope" "hello world""#, false, bool);
    assert_evals_to!(r#"Str.startsWith "hell" "hello world""#, false, bool);
    assert_evals_to!(r#"Str.startsWith "" "hello world""#, false, bool);
}

#[test]
fn str_starts_with_scalar() {
    assert_evals_to!(
        &format!(r#"Str.startsWithScalar "foobar" {}"#, 'f' as u32),
        true,
        bool
    );
    assert_evals_to!(
        &format!(r#"Str.startsWithScalar "zoobar" {}"#, 'f' as u32),
        false,
        bool
    );
}

#[test]
fn str_ends_with() {
    assert_evals_to!(r#"Str.endsWith "hello world" "world""#, true, bool);
    assert_evals_to!(r#"Str.endsWith "nope" "hello world""#, false, bool);
    assert_evals_to!(r#"Str.endsWith "" "hello world""#, false, bool);
}

#[test]
fn str_count_graphemes_small_str() {
    assert_evals_to!(r#"Str.countGraphemes "å🤔""#, 2, usize);
}

#[test]
fn str_count_graphemes_three_js() {
    assert_evals_to!(r#"Str.countGraphemes "JJJ""#, 3, usize);
}

#[test]
fn str_count_graphemes_big_str() {
    assert_evals_to!(
        r#"Str.countGraphemes "6🤔å🤔e¥🤔çppkd🙃1jdal🦯asdfa∆ltråø˚waia8918.,🏅jjc""#,
        45,
        usize
    );
}

#[test]
fn str_starts_with_same_big_str() {
    assert_evals_to!(
        r#"Str.startsWith "123456789123456789" "123456789123456789""#,
        true,
        bool
    );
}

#[test]
fn str_starts_with_different_big_str() {
    assert_evals_to!(
        r#"Str.startsWith "12345678912345678910" "123456789123456789""#,
        true,
        bool
    );
}

#[test]
fn str_starts_with_same_small_str() {
    assert_evals_to!(r#"Str.startsWith "1234" "1234""#, true, bool);
}

#[test]
fn str_starts_with_different_small_str() {
    assert_evals_to!(r#"Str.startsWith "1234" "12""#, true, bool);
}
#[test]
fn str_starts_with_false_small_str() {
    assert_evals_to!(r#"Str.startsWith "1234" "23""#, false, bool);
}

#[test]
fn str_from_utf8_pass_single_ascii() {
    assert_evals_to!(
        indoc!(
            r#"
                    when Str.fromUtf8 [97] is
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
                    when Str.fromUtf8 [97, 98, 99, 0x7E] is
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
                    when Str.fromUtf8 [0xE2, 0x88, 0x86] is
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
                    when Str.fromUtf8 [0xE2, 0x88, 0x86, 0xC5, 0x93, 0xC2, 0xAC] is
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
                    when Str.fromUtf8 [0xF0, 0x9F, 0x92, 0x96] is
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
                    when Str.fromUtf8 [0xF0, 0x9F, 0x92, 0x96, 0xF0, 0x9F, 0xA4, 0xA0, 0xF0, 0x9F, 0x9A, 0x80] is
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
                    when Str.fromUtf8 [0xF0, 0x9F, 0x92, 0x96, 98, 0xE2, 0x88, 0x86] is
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
                    when Str.fromUtf8 [97, 98, 0x80, 99] is
                        Err (BadUtf8 InvalidStartByte byteIndex) ->
                            if byteIndex == 2 then
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
                    when Str.fromUtf8 [97, 98, 99, 0xC2] is
                        Err (BadUtf8 UnexpectedEndOfSequence byteIndex) ->
                            if byteIndex == 3 then
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
                    when Str.fromUtf8 [97, 98, 99, 0xC2, 0x00] is
                        Err (BadUtf8 ExpectedContinuation byteIndex) ->
                            if byteIndex == 3 then
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
                    when Str.fromUtf8 [97, 0xF0, 0x80, 0x80, 0x80] is
                        Err (BadUtf8 OverlongEncoding byteIndex) ->
                            if byteIndex == 1 then
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
                    when Str.fromUtf8 [97, 0xF4, 0x90, 0x80, 0x80] is
                        Err (BadUtf8 CodepointTooLarge byteIndex) ->
                            if byteIndex == 1 then
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
                    when Str.fromUtf8 [97, 98, 0xED, 0xA0, 0x80] is
                        Err (BadUtf8 EncodesSurrogateHalf byteIndex) ->
                            if byteIndex == 2 then
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
        r#"Str.joinWith ["1", "2"] ", " "#,
        RocStr::from("1, 2"),
        RocStr
    );
}

#[test]
fn str_join_comma_big() {
    assert_evals_to!(
        r#"Str.joinWith ["10000000", "2000000", "30000000"] ", " "#,
        RocStr::from("10000000, 2000000, 30000000"),
        RocStr
    );
}

#[test]
fn str_join_comma_single() {
    assert_evals_to!(r#"Str.joinWith ["1"] ", " "#, RocStr::from("1"), RocStr);
}

#[test]
fn str_to_utf8() {
    assert_evals_to!(
        r#"Str.toUtf8 "hello""#,
        RocList::from_slice(&[104, 101, 108, 108, 111]),
        RocList<u8>
    );
    assert_evals_to!(
        r#"Str.toUtf8 "this is a long string""#,
        RocList::from_slice(&[
            116, 104, 105, 115, 32, 105, 115, 32, 97, 32, 108, 111, 110, 103, 32, 115, 116, 114,
            105, 110, 103
        ]),
        RocList<u8>
    );
}

#[test]
fn str_from_utf8_range() {
    assert_evals_to!(
        indoc!(
            r#"
            bytes = Str.toUtf8 "hello"
            when Str.fromUtf8Range bytes { count: 5,  start: 0 }  is
                   Ok utf8String -> utf8String
                   _ -> ""
            "#
        ),
        RocStr::from("hello"),
        RocStr
    );
}

#[test]
fn str_from_utf8_range_slice() {
    assert_evals_to!(
        indoc!(
            r#"
            bytes = Str.toUtf8 "hello"
            when Str.fromUtf8Range bytes { count: 4,  start: 1 }  is
                   Ok utf8String -> utf8String
                   _ -> ""
            "#
        ),
        RocStr::from("ello"),
        RocStr
    );
}

#[test]
fn str_from_utf8_range_slice_not_end() {
    assert_evals_to!(
        indoc!(
            r#"
            bytes = Str.toUtf8 "hello"
            when Str.fromUtf8Range bytes { count: 3,  start: 1 }  is
                   Ok utf8String -> utf8String
                   _ -> ""
            "#
        ),
        RocStr::from("ell"),
        RocStr
    );
}

#[test]
fn str_from_utf8_range_order_does_not_matter() {
    assert_evals_to!(
        indoc!(
            r#"
            bytes = Str.toUtf8 "hello"
            when Str.fromUtf8Range bytes { start: 1,  count: 3 }  is
                   Ok utf8String -> utf8String
                   _ -> ""
            "#
        ),
        RocStr::from("ell"),
        RocStr
    );
}

#[test]
fn str_from_utf8_range_out_of_bounds_start_value() {
    assert_evals_to!(
        indoc!(
            r#"
            bytes = Str.toUtf8 "hello"
            when Str.fromUtf8Range bytes { start: 7,  count: 3 }  is
                   Ok _ -> ""
                   Err (BadUtf8 _ _) -> ""
                   Err OutOfBounds -> "out of bounds"
            "#
        ),
        RocStr::from("out of bounds"),
        RocStr
    );
}

#[test]
fn str_from_utf8_range_count_too_high() {
    assert_evals_to!(
        indoc!(
            r#"
            bytes = Str.toUtf8 "hello"
            when Str.fromUtf8Range bytes { start: 0,  count: 6 }  is
                   Ok _ -> ""
                   Err (BadUtf8 _ _) -> ""
                   Err OutOfBounds -> "out of bounds"
            "#
        ),
        RocStr::from("out of bounds"),
        RocStr
    );
}

#[test]
fn str_from_utf8_range_count_too_high_for_start() {
    assert_evals_to!(
        indoc!(
            r#"
            bytes = Str.toUtf8 "hello"
            when Str.fromUtf8Range bytes { start: 4,  count: 3 }  is
                   Ok _ -> ""
                   Err (BadUtf8 _ _) -> ""
                   Err OutOfBounds -> "out of bounds"
            "#
        ),
        RocStr::from("out of bounds"),
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
    assert_evals_to!(indoc!(r#"Str.trimStart " ""#), RocStr::from(""), RocStr);
}

#[test]
fn str_trim_start_small_to_small() {
    assert_evals_to!(
        indoc!(r#"Str.trimStart "  hello  ""#),
        RocStr::from("hello  "),
        RocStr
    );
}

#[test]
fn str_trim_start_large_to_large_unique() {
    assert_evals_to!(
        indoc!(r#"Str.trimStart (Str.concat "    " "hello world from a large string ")"#),
        RocStr::from("hello world from a large string "),
        RocStr
    );
}

#[test]
fn str_trim_start_large_to_small_unique() {
    assert_evals_to!(
        indoc!(r#"Str.trimStart (Str.concat "  " "hello  ")"#),
        RocStr::from("hello  "),
        RocStr
    );
}

#[test]
fn str_trim_end_small_blank_string() {
    assert_evals_to!(indoc!(r#"Str.trimEnd " ""#), RocStr::from(""), RocStr);
}

#[test]
fn str_trim_end_small_to_small() {
    assert_evals_to!(
        indoc!(r#"Str.trimEnd " hello ""#),
        RocStr::from(" hello"),
        RocStr
    );
}

#[test]
fn str_trim_end_large_to_large_unique() {
    assert_evals_to!(
        indoc!(r#"Str.trimEnd (Str.concat " hello world from a large string" "    ")"#),
        RocStr::from(" hello world from a large string"),
        RocStr
    );
}

#[test]
fn str_trim_end_large_to_small_unique() {
    assert_evals_to!(
        indoc!(r#"Str.trimEnd (Str.concat "  hello" "  ")"#),
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

               { trimmed: Str.trimEnd original, original: original }
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

               { trimmed: Str.trimEnd original, original: original }
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

               { trimmed: Str.trimEnd original, original: original }
               "#
        ),
        (RocStr::from(" hello "), RocStr::from(" hello"),),
        (RocStr, RocStr)
    );
}

#[test]
fn str_to_nat() {
    assert_evals_to!(
        indoc!(
            r#"
             when Str.toNat "1" is
                 Ok n -> n
                 Err _ -> 0
                "#
        ),
        1,
        usize
    );
}

#[test]
fn str_to_i128() {
    assert_evals_to!(
        indoc!(
            r#"
             when Str.toI128 "1" is
                 Ok n -> n
                 Err _ -> 0
                "#
        ),
        1,
        i128
    );
}

#[test]
fn str_to_u128() {
    assert_evals_to!(
        indoc!(
            r#"
             when Str.toU128 "1" is
                 Ok n -> n
                 Err _ -> 0
                "#
        ),
        1,
        u128
    );
}

#[test]
fn str_to_i64() {
    assert_evals_to!(
        indoc!(
            r#"
             when Str.toI64 "1" is
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
             when Str.toU64 "1" is
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
             when Str.toI32 "1" is
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
             when Str.toU32 "1" is
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
             when Str.toI16 "1" is
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
             when Str.toU16 "1" is
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
             when Str.toI8 "1" is
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
             when Str.toU8 "1" is
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
             when Str.toF64 "1.0" is
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
             when Str.toF32 "1.0" is
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
             when Str.toDec "1.0" is
                 Ok n -> n
                 Err _ -> 0
             "#
        ),
        RocDec::from_str("1.0").unwrap(),
        RocDec
    );
}

#[test]
fn str_walk_scalars() {
    assert_evals_to!(
        indoc!(
            r#"
            Str.walkScalars "abcd" [] List.append
            "#
        ),
        RocList::from_slice(&['a', 'b', 'c', 'd']),
        RocList<char>
    );
}
