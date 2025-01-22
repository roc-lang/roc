#[cfg(feature = "gen-llvm")]
use crate::helpers::llvm::assert_evals_to;
#[cfg(feature = "gen-llvm")]
use crate::helpers::llvm::assert_llvm_evals_to;

#[cfg(feature = "gen-dev")]
use crate::helpers::dev::assert_evals_to;
#[cfg(feature = "gen-dev")]
use crate::helpers::dev::assert_evals_to as assert_llvm_evals_to;

#[cfg(feature = "gen-wasm")]
use crate::helpers::wasm::assert_evals_to;

#[allow(unused_imports)]
use indoc::indoc;
#[allow(unused_imports)]
use roc_std::{RocList, RocResult, RocStr, I128, U128};

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn string_eq() {
    // context: the dev backend did not correctly mask the boolean that zig returns here
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"
            main : I64
            main = if "*" == "*" then 123 else 456
            "#
        ),
        123,
        u64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn string_neq() {
    // context: the dev backend did not correctly mask the boolean that zig returns here
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"
            main : I64
            main = if "*" != "*" then 123 else 456
            "#
        ),
        456,
        u64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
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

    assert_evals_to!(
        indoc!(
            r#"
                    when List.first (Str.split_on "JJJ" "") is
                        Ok str ->
                            Str.count_utf8_bytes str

                        _ ->
                            1729

                "#
        ),
        3,
        u64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
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

    assert_evals_to!(
        indoc!(
            r#"
                    when List.first (Str.split_on "JJJ" "JJJJ there") is
                        Ok str ->
                            Str.count_utf8_bytes str

                        _ ->
                            1729

                "#
        ),
        3,
        u64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn str_split_on_small_str_bigger_delimiter() {
    assert_evals_to!(
        indoc!(r#"Str.split_on "JJJ" "0123456789abcdefghi""#),
        RocList::from_slice(&[RocStr::from("JJJ")]),
        RocList<RocStr>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn long_str_literal() {
    assert_evals_to!(
        "\"0123456789 123456789 123456789\"",
        RocStr::from("0123456789 123456789 123456789"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev"))]
fn small_str_literal() {
    assert_llvm_evals_to!(
        "\"JJJJJJJJJJJJJJJJJJJJJJJ\"",
        [
            b'J',
            b'J',
            b'J',
            b'J',
            b'J',
            b'J',
            b'J',
            b'J',
            b'J',
            b'J',
            b'J',
            b'J',
            b'J',
            b'J',
            b'J',
            b'J',
            b'J',
            b'J',
            b'J',
            b'J',
            b'J',
            b'J',
            b'J',
            0b1000_0000 | 23
        ],
        [u8; 24]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-dev"))]
fn small_str_zeroed_literal() {
    // Verifies that we zero out unused bytes in the string.
    // This is important so that string equality tests don't randomly
    // fail due to unused memory being there!
    assert_llvm_evals_to!(
        "\"J\"",
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
            0x00,
            0x00,
            0b1000_0001
        ],
        [u8; 24]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-dev"))]
fn small_str_concat_empty_first_arg() {
    assert_llvm_evals_to!(
        r#"Str.concat "" "JJJJJJJJJJJJJJJ""#,
        [
            b'J',
            b'J',
            b'J',
            b'J',
            b'J',
            b'J',
            b'J',
            b'J',
            b'J',
            b'J',
            b'J',
            b'J',
            b'J',
            b'J',
            b'J',
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0b1000_0000 | 15
        ],
        [u8; 24]
    );
}

#[test]
#[cfg(feature = "gen-wasm")]
fn small_str_concat_empty_first_arg() {
    assert_evals_to!(
        r#"Str.concat "" "01234567890""#,
        [0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x30, 0x8b],
        [u8; 12]
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev"))]
fn small_str_concat_empty_second_arg() {
    assert_llvm_evals_to!(
        r#"Str.concat "JJJJJJJJJJJJJJJ" """#,
        [
            b'J',
            b'J',
            b'J',
            b'J',
            b'J',
            b'J',
            b'J',
            b'J',
            b'J',
            b'J',
            b'J',
            b'J',
            b'J',
            b'J',
            b'J',
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0b1000_0000 | 15
        ],
        [u8; 24]
    );
}

#[test]
#[cfg(feature = "gen-wasm")]
fn small_str_concat_empty_second_arg() {
    assert_evals_to!(
        r#"Str.concat "01234567890" """#,
        [0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x30, 0x8b],
        [u8; 12]
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn small_str_concat_small_to_big() {
    assert_evals_to!(
        r#"Str.concat "abc" " this is longer than 15 chars""#,
        RocStr::from("abc this is longer than 15 chars"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev"))]
fn small_str_concat_small_to_small_staying_small() {
    assert_llvm_evals_to!(
        r#"Str.concat "J" "JJJJJJJJJJJJJJ""#,
        [
            b'J',
            b'J',
            b'J',
            b'J',
            b'J',
            b'J',
            b'J',
            b'J',
            b'J',
            b'J',
            b'J',
            b'J',
            b'J',
            b'J',
            b'J',
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0b1000_0000 | 15
        ],
        [u8; 24]
    );
}

#[test]
#[cfg(feature = "gen-wasm")]
fn small_str_concat_small_to_small_staying_small() {
    assert_evals_to!(
        r#"Str.concat "0" "1234567890""#,
        [0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x30, 0x8b],
        [u8; 12]
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn small_str_concat_small_to_small_overflow_to_big() {
    assert_evals_to!(
        r#"Str.concat "abcdefghijklm" "nopqrstuvwxyz""#,
        RocStr::from("abcdefghijklmnopqrstuvwxyz"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn str_concat_empty() {
    assert_evals_to!(r#"Str.concat "" """#, RocStr::default(), RocStr);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn small_str_is_empty() {
    assert_evals_to!(r#"Str.is_empty "abc""#, false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn big_str_is_empty() {
    assert_evals_to!(
        r#"Str.is_empty "this is more than 23 chars long""#,
        false,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn empty_str_is_empty() {
    assert_evals_to!(r#"Str.is_empty """#, true, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn str_starts_with() {
    assert_evals_to!(r#"Str.starts_with "hello world" "hell""#, true, bool);
    assert_evals_to!(r#"Str.starts_with "hello world" """#, true, bool);
    assert_evals_to!(r#"Str.starts_with "nope" "hello world""#, false, bool);
    assert_evals_to!(r#"Str.starts_with "hell" "hello world""#, false, bool);
    assert_evals_to!(r#"Str.starts_with "" "hello world""#, false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn str_ends_with() {
    assert_evals_to!(r#"Str.ends_with "hello world" "world""#, true, bool);
    assert_evals_to!(r#"Str.ends_with "nope" "hello world""#, false, bool);
    assert_evals_to!(r#"Str.ends_with "" "hello world""#, false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn str_starts_with_same_big_str() {
    assert_evals_to!(
        r#"Str.starts_with "123456789123456789" "123456789123456789""#,
        true,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn str_starts_with_different_big_str() {
    assert_evals_to!(
        r#"Str.starts_with "12345678912345678910" "123456789123456789""#,
        true,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn str_starts_with_same_small_str() {
    assert_evals_to!(r#"Str.starts_with "1234" "1234""#, true, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn str_starts_with_different_small_str() {
    assert_evals_to!(r#"Str.starts_with "1234" "12""#, true, bool);
}
#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn str_starts_with_false_small_str() {
    assert_evals_to!(r#"Str.starts_with "1234" "23""#, false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn str_from_utf8_lossy_expected_continuation() {
    assert_evals_to!(
        r#"Str.from_utf8_lossy [97, 98, 0xC2, 99]"#,
        roc_std::RocStr::from("ab�c"),
        roc_std::RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
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

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev"))]
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

// Marking this as should_panic, because it *does* panic and it is not clear why?
// If some change magically fixes this, great, merge with above.
#[test]
#[cfg(feature = "gen-wasm")]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn str_from_utf16_lossy() {
    assert_evals_to!(
        r#"Str.from_utf16_lossy [0x72, 0xdc96, 0x63]"#,
        roc_std::RocStr::from("r�c"),
        roc_std::RocStr
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn str_from_utf32_lossy() {
    assert_evals_to!(
        r#"Str.from_utf32_lossy [0x72, 0x123456, 0x63]"#,
        roc_std::RocStr::from("r�c"),
        roc_std::RocStr
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
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
fn str_clone() {
    use roc_std::RocStr;
    let long = RocStr::from("loremipsumdolarsitamet");
    let short = RocStr::from("x");
    let empty = RocStr::from("");

    debug_assert_eq!(long.clone(), long);
    debug_assert_eq!(short.clone(), short);
    debug_assert_eq!(empty.clone(), empty);
}

#[test]
#[cfg(feature = "gen-llvm")]
fn nested_recursive_literal() {
    assert_evals_to!(
        indoc!(
            r#"
                Expr : [Add Expr Expr, Val I64, Var I64]

                expr : Expr
                expr = Add (Add (Val 3) (Val 1)) (Add (Val 1) (Var 1))

                print_expr : Expr -> Str
                print_expr = \e ->
                    when e is
                        Add a b ->
                            "Add ("
                                |> Str.concat (print_expr a)
                                |> Str.concat ") ("
                                |> Str.concat (print_expr b)
                                |> Str.concat ")"
                        Val v -> "Val " |> Str.concat (Num.to_str v)
                        Var v -> "Var " |> Str.concat (Num.to_str v)

                print_expr expr
                "#
        ),
        RocStr::from("Add (Add (Val 3) (Val 1)) (Add (Val 1) (Var 1))"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn str_join_comma_small() {
    assert_evals_to!(
        r#"Str.join_with ["1", "2"] ", " "#,
        RocStr::from("1, 2"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn str_join_comma_big() {
    assert_evals_to!(
        r#"Str.join_with ["10000000", "2000000", "30000000"] ", " "#,
        RocStr::from("10000000, 2000000, 30000000"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn str_join_comma_single() {
    assert_evals_to!(r#"Str.join_with ["1"] ", " "#, RocStr::from("1"), RocStr);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn str_from_utf8_order_does_not_matter() {
    assert_evals_to!(
        indoc!(
            r#"
            bytes =
                Str.to_utf8 "hello"
                |> List.sublist { start: 1, len: 3 }

            when Str.from_utf8 bytes is
                   Ok utf8_string -> utf8_string
                   Err _ -> "Str.from_utf8 returned Err instead of Ok!"
            "#
        ),
        RocStr::from("ell"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn str_repeat_small_stays_small() {
    assert_evals_to!(
        indoc!(r#"Str.repeat "Roc" 3"#),
        RocStr::from("RocRocRoc"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn str_repeat_small_becomes_big() {
    assert_evals_to!(
        indoc!(r#"Str.repeat "less than 23 characters" 2"#),
        RocStr::from("less than 23 charactersless than 23 characters"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn str_repeat_small() {
    assert_evals_to!(
        indoc!(r#"Str.repeat "Roc" 2"#),
        RocStr::from("RocRoc"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn str_repeat_big() {
    assert_evals_to!(
        indoc!(r#"Str.repeat "more than 23 characters now" 2"#),
        RocStr::from("more than 23 characters nowmore than 23 characters now"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn str_repeat_empty_string() {
    let a = indoc!(r#"Str.repeat "" 3"#);
    assert_evals_to!(a, RocStr::from(""), RocStr);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn str_repeat_zero_times() {
    assert_evals_to!(indoc!(r#"Str.repeat "Roc" 0"#), RocStr::from(""), RocStr);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn str_trim_empty_string() {
    assert_evals_to!(indoc!(r#"Str.trim """#), RocStr::from(""), RocStr);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn str_trim_null_byte() {
    assert_evals_to!(
        indoc!(r#"Str.trim (Str.reserve "\u(0000)" 40)"#),
        RocStr::from("\0"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn str_trim_small_blank_string() {
    assert_evals_to!(indoc!(r#"Str.trim " ""#), RocStr::from(""), RocStr);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn str_trim_small_to_small() {
    assert_evals_to!(
        indoc!(r#"Str.trim "  hello world  ""#),
        RocStr::from("hello world"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn str_trim_large_to_large_unique() {
    assert_evals_to!(
        indoc!(r#"Str.trim (Str.concat "  " "hello world from a large string ")"#),
        RocStr::from("hello world from a large string"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn str_trim_large_to_small_unique() {
    assert_evals_to!(
        indoc!(r#"Str.trim (Str.concat "  " "hello world        ")"#),
        RocStr::from("hello world"),
        RocStr
    );
}

#[test]
#[cfg(feature = "gen-llvm")]
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
#[cfg(feature = "gen-llvm")]
fn str_trim_large_to_small_shared() {
    assert_evals_to!(
        indoc!(
            r#"
               original : Str
               original = " hello world             "

               { trimmed: Str.trim original, original: original }
               "#
        ),
        (
            RocStr::from(" hello world             "),
            RocStr::from("hello world"),
        ),
        (RocStr, RocStr)
    );
}

#[test]
#[cfg(feature = "gen-llvm")]
fn str_trim_small_to_small_shared() {
    assert_evals_to!(
        indoc!(
            r#"
               original : Str
               original = " hello world "

               { trimmed: Str.trim original, original: original }
               "#
        ),
        (RocStr::from(" hello world "), RocStr::from("hello world"),),
        (RocStr, RocStr)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn str_trim_start_small_blank_string() {
    assert_evals_to!(indoc!(r#"Str.trim_start " ""#), RocStr::from(""), RocStr);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn str_trim_start_small_to_small() {
    assert_evals_to!(
        indoc!(r#"Str.trim_start "  hello world  ""#),
        RocStr::from("hello world  "),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn str_trim_start_large_to_large_unique() {
    assert_evals_to!(
        indoc!(r#"Str.trim_start (Str.concat "    " "hello world from a large string ")"#),
        RocStr::from("hello world from a large string "),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn str_trim_start_large_to_small_unique() {
    assert_evals_to!(
        indoc!(r#"Str.trim_start (Str.concat "  " "hello world        ")"#),
        RocStr::from("hello world        "),
        RocStr
    );
}

#[test]
#[cfg(feature = "gen-llvm")]
fn str_trim_start_large_to_large_shared() {
    assert_evals_to!(
        indoc!(
            r#"
               original : Str
               original = " hello world world "

               { trimmed: Str.trim_start original, original: original }
               "#
        ),
        (
            RocStr::from(" hello world world "),
            RocStr::from("hello world world "),
        ),
        (RocStr, RocStr)
    );
}

#[test]
#[cfg(feature = "gen-llvm")]
fn str_trim_start_large_to_small_shared() {
    assert_evals_to!(
        indoc!(
            r#"
               original : Str
               original = " hello world             "

               { trimmed: Str.trim_start original, original: original }
               "#
        ),
        (
            RocStr::from(" hello world             "),
            RocStr::from("hello world             "),
        ),
        (RocStr, RocStr)
    );
}

#[test]
#[cfg(feature = "gen-llvm")]
fn str_trim_start_small_to_small_shared() {
    assert_evals_to!(
        indoc!(
            r#"
               original : Str
               original = " hello world "

               { trimmed: Str.trim_start original, original: original }
               "#
        ),
        (RocStr::from(" hello world "), RocStr::from("hello world "),),
        (RocStr, RocStr)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn str_trim_end_small_blank_string() {
    assert_evals_to!(indoc!(r#"Str.trim_end " ""#), RocStr::from(""), RocStr);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn str_trim_end_small_to_small() {
    assert_evals_to!(
        indoc!(r#"Str.trim_end "  hello world  ""#),
        RocStr::from("  hello world"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn str_trim_end_large_to_large_unique() {
    assert_evals_to!(
        indoc!(r#"Str.trim_end (Str.concat " hello world from a large string" "    ")"#),
        RocStr::from(" hello world from a large string"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn str_trim_end_large_to_small_unique() {
    assert_evals_to!(
        indoc!(r#"Str.trim_end (Str.concat "        hello world" "  ")"#),
        RocStr::from("        hello world"),
        RocStr
    );
}

#[test]
#[cfg(feature = "gen-llvm")]
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
#[cfg(feature = "gen-llvm")]
fn str_trim_end_large_to_small_shared() {
    assert_evals_to!(
        indoc!(
            r#"
               original : Str
               original = "             hello world "

               { trimmed: Str.trim_end original, original: original }
               "#
        ),
        (
            RocStr::from("             hello world "),
            RocStr::from("             hello world"),
        ),
        (RocStr, RocStr)
    );
}

#[test]
#[cfg(feature = "gen-llvm")]
fn str_trim_end_small_to_small_shared() {
    assert_evals_to!(
        indoc!(
            r#"
               original : Str
               original = " hello world "

               { trimmed: Str.trim_end original, original: original }
               "#
        ),
        (RocStr::from(" hello world "), RocStr::from(" hello world"),),
        (RocStr, RocStr)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn str_to_nat() {
    assert_evals_to!(
        indoc!(
            r#"
            Str.to_u64 "1"
            "#
        ),
        RocResult::ok(1),
        RocResult<u64, ()>
    );
}

#[test]
#[cfg(feature = "gen-llvm")]
fn str_to_i128() {
    assert_evals_to!(
        indoc!(
            r#"
            Str.to_i128 "1"
            "#
        ),
        RocResult::ok(I128::from(1)),
        RocResult<I128, ()>
    );
}

#[test]
#[cfg(feature = "gen-llvm")]
fn str_to_u128() {
    assert_evals_to!(
        indoc!(
            r#"
            Str.to_u128 "1"
            "#
        ),
        RocResult::ok(U128::from(1)),
        RocResult<U128, ()>
    );
}

// TODO add alignment check between i64 and I64 somewhere
#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn str_to_i64() {
    assert_evals_to!(
        indoc!(
            r#"
            Str.to_i64 "1"
            "#
        ),
        RocResult::ok(1),
        RocResult<i64, ()>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn str_to_u64() {
    assert_evals_to!(
        indoc!(
            r#"
            Str.to_u64 "1"
            "#
        ),
        RocResult::ok(1),
        RocResult<u64, ()>
    );
}

#[test]
#[cfg(feature = "gen-llvm")]
fn str_to_i32() {
    assert_evals_to!(
        indoc!(
            r#"
            Str.to_i32 "1"
            "#
        ),
        RocResult::ok(1),
        RocResult<i32, ()>
    );
}

#[test]
#[cfg(feature = "gen-llvm")]
fn str_to_u32() {
    assert_evals_to!(
        indoc!(
            r#"
            Str.to_u32 "1"
            "#
        ),
        RocResult::ok(1),
        RocResult<u32, ()>
    );
}

#[test]
#[cfg(feature = "gen-llvm")]
fn str_to_i16() {
    assert_evals_to!(
        indoc!(
            r#"
            Str.to_i16 "1"
            "#
        ),
        RocResult::ok(1),
        RocResult<i16, ()>
    );
}

#[test]
#[cfg(feature = "gen-llvm")]
fn str_to_u16() {
    assert_evals_to!(
        indoc!(
            r#"
            Str.to_u16 "1"
            "#
        ),
        RocResult::ok(1),
        RocResult<u16, ()>
    );
}

#[test]
#[cfg(feature = "gen-llvm")]
fn str_to_i8() {
    assert_evals_to!(
        indoc!(
            r#"
            Str.to_i8 "1"
            "#
        ),
        RocResult::ok(1),
        RocResult<i8, ()>
    );
}

#[test]
#[cfg(feature = "gen-llvm")]
fn str_to_u8() {
    assert_evals_to!(
        indoc!(
            r#"
            Str.to_u8 "1"
            "#
        ),
        RocResult::ok(1),
        RocResult<u8, ()>
    );
}

#[test]
#[cfg(feature = "gen-llvm")]
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
#[cfg(feature = "gen-llvm")]
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
#[cfg(feature = "gen-llvm")]
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

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn issue_2811() {
    assert_evals_to!(
        indoc!(
            r#"
            x = Command { tool: "bash" }
            Command c = x
            c.tool
            "#
        ),
        RocStr::from("bash"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn str_split_on_first_one_char() {
    assert_evals_to!(
        indoc!(
            r#"
            Str.split_first "foo/bar/baz" "/"
            "#
        ),
        // the result is a { before, after } record, and because of
        // alphabetic ordering the fields here are flipped
        RocResult::ok((RocStr::from("bar/baz"), RocStr::from("foo"))),
        RocResult<(RocStr, RocStr), ()>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn str_split_on_first_multiple_chars() {
    assert_evals_to!(
        indoc!(
            r#"
            Str.split_first "foo//bar//baz" "//"
            "#
        ),
        RocResult::ok((RocStr::from("bar//baz"), RocStr::from("foo"))),
        RocResult<(RocStr, RocStr), ()>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn str_split_on_first_entire_input() {
    assert_evals_to!(
        indoc!(
            r#"
            Str.split_first "foo" "foo"
            "#
        ),
        RocResult::ok((RocStr::from(""), RocStr::from(""))),
        RocResult<(RocStr, RocStr), ()>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn str_split_on_first_not_found() {
    assert_evals_to!(
        indoc!(
            r#"
            Str.split_first "foo" "bar"
            "#
        ),
        RocResult::err(()),
        RocResult<(RocStr, RocStr), ()>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn str_split_on_last_one_char() {
    assert_evals_to!(
        indoc!(
            r#"
            Str.split_last "foo/bar/baz" "/"
            "#
        ),
        RocResult::ok((RocStr::from("baz"), RocStr::from("foo/bar"))),
        RocResult<(RocStr, RocStr), ()>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn str_split_on_last_multiple_chars() {
    assert_evals_to!(
        indoc!(
            r#"
            Str.split_last "foo//bar//baz" "//"
            "#
        ),
        RocResult::ok((RocStr::from("baz"), RocStr::from("foo//bar"))),
        RocResult<(RocStr, RocStr), ()>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn str_split_on_last_entire_input() {
    assert_evals_to!(
        indoc!(
            r#"
            Str.split_last "foo" "foo"
            "#
        ),
        RocResult::ok((RocStr::from(""), RocStr::from(""))),
        RocResult<(RocStr, RocStr), ()>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn str_split_on_last_not_found() {
    assert_evals_to!(
        indoc!(
            r#"
            Str.split_last "foo" "bar"
            "#
        ),
        RocResult::err(()),
        RocResult<(RocStr, RocStr), ()>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn str_split_on_overlapping_substring_1() {
    assert_evals_to!(
        r#"Str.split_on "aaa" "aa""#,
        RocList::from_slice(&[RocStr::from(""), RocStr::from("a")]),
        RocList<RocStr>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn str_split_on_overlapping_substring_2() {
    assert_evals_to!(
        r#"Str.split_on "aaaa" "aa""#,
        RocList::from_slice(&[RocStr::from(""), RocStr::from(""), RocStr::from("")]),
        RocList<RocStr>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn str_walk_utf8() {
    #[cfg(not(feature = "gen-llvm-wasm"))]
    assert_evals_to!(
        // Reverse the bytes
        indoc!(
            r#"
            Str.walk_utf8 "abcd" [] (\list, byte -> List.prepend list byte)
            "#
        ),
        RocList::from_slice(&[b'd', b'c', b'b', b'a']),
        RocList<u8>
    );

    #[cfg(feature = "gen-llvm-wasm")]
    assert_evals_to!(
        indoc!(
            r#"
            Str.walk_utf8 "abcd" [] (\list, byte -> List.prepend list byte)
            "#
        ),
        RocList::from_slice(&[b'd', b'c', b'b', b'a']),
        RocList<u8>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn str_walk_utf8_with_index() {
    assert_evals_to!(
        indoc!(
            r#"
            Str.walk_utf8_with_index "abcd" [] (\list, byte, index -> List.append list (Pair index byte))
            "#
        ),
        RocList::from_slice(&[(0, b'a'), (1, b'b'), (2, b'c'), (3, b'd')]),
        RocList<(u64, u8)>
    );
}

#[test]
#[cfg(feature = "gen-llvm-wasm")]
fn llvm_wasm_str_layout() {
    assert_evals_to!(
        indoc!(
            r#"
            "hello"
                |> Str.reserve 42
            "#
        ),
        [0, 5, 1],
        [u32; 3],
        |[_ptr, len, cap]: [u32; 3]| [0, len, if cap >= 42 { 1 } else { 0 }]
    )
}

#[test]
#[cfg(feature = "gen-llvm-wasm")]
fn llvm_wasm_str_layout_small() {
    // exposed an error in using bitcast instead of zextend
    assert_evals_to!(
        indoc!(
            r#"
            "𒀀𒀁"
                |> Str.trim
            "#
        ),
        [-2139057424, -2122280208, -2013265920],
        [i32; 3]
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn when_on_strings() {
    assert_evals_to!(
        indoc!(
            r#"
            when "Deyr fé, deyja frændr" is
                "Deyr fé, deyja frændr" -> 42
                "deyr sjalfr it sama" -> 1
                "en orðstírr deyr aldregi" -> 2
                "hveim er sér góðan getr" -> 3
                _ -> 4
            "#
        ),
        42,
        i64
    );

    assert_evals_to!(
        indoc!(
            r#"
            when "Deyr fé, deyja frændr" is
                "deyr sjalfr it sama" -> 1
                "en orðstírr deyr aldregi" -> 2
                "hveim er sér góðan getr" -> 3
                _ -> 4
            "#
        ),
        4,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn with_capacity() {
    assert_evals_to!(
        indoc!(
            r#"
            Str.with_capacity 10
            "#
        ),
        RocStr::from(""),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn with_capacity_concat() {
    assert_evals_to!(
        indoc!(
            r#"
            Str.with_capacity 10 |> Str.concat "Forty-two"
            "#
        ),
        RocStr::from("Forty-two"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn str_with_prefix() {
    assert_evals_to!(
        indoc!(
            r#"
            Str.with_prefix "world!" "Hello "
            "#
        ),
        RocStr::from("Hello world!"),
        RocStr
    );

    assert_evals_to!(
        indoc!(
            r#"
            "two" |> Str.with_prefix "Forty "
            "#
        ),
        RocStr::from("Forty two"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn destructure_pattern_assigned_from_thunk_opaque() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            MyCustomType := Str
            myMsg = @MyCustomType "Hello"

            main =
                @MyCustomType msg = myMsg

                msg
            "#
        ),
        RocStr::from("Hello"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn destructure_pattern_assigned_from_thunk_tag() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            myMsg = A "hello " "world"

            main =
                A m1 m2 = myMsg

                Str.concat m1 m2
            "#
        ),
        RocStr::from("hello world"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn release_excess_capacity() {
    assert_evals_to!(
        indoc!(
            r#"
            Str.reserve "" 50
            |> Str.release_excess_capacity
            "#
        ),
        (RocStr::empty().capacity(), RocStr::empty()),
        RocStr,
        |value: RocStr| (value.capacity(), value)
    );
}

// if this gets implemented, then add gen-dev to positive case above
#[test]
#[cfg(feature = "gen-dev")]
#[should_panic(expected = "not yet implemented: low level, StrReleaseExcessCapacity")]
fn release_excess_capacity() {
    assert_evals_to!(
        indoc!(
            r#"
            Str.reserve "" 50
            |> Str.release_excess_capacity
            "#
        ),
        (RocStr::empty().capacity(), RocStr::empty()),
        RocStr,
        |value: RocStr| (value.capacity(), value)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn release_excess_capacity_with_len() {
    assert_evals_to!(
        indoc!(
            r#"
            "123456789012345678901234567890"
            |> Str.reserve 50
            |> Str.release_excess_capacity
            "#
        ),
        (30, "123456789012345678901234567890".into()),
        RocStr,
        |value: RocStr| (value.capacity(), value)
    );
}

// if this gets implemented, then add gen-dev to positive case above
#[test]
#[cfg(feature = "gen-dev")]
#[should_panic(expected = "not yet implemented: low level, StrReleaseExcessCapacity")]
fn release_excess_capacity_with_len() {
    assert_evals_to!(
        indoc!(
            r#"
            "123456789012345678901234567890"
            |> Str.reserve 50
            |> Str.release_excess_capacity
            "#
        ),
        (30, "123456789012345678901234567890".into()),
        RocStr,
        |value: RocStr| (value.capacity(), value)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn release_excess_capacity_empty() {
    assert_evals_to!(
        indoc!(
            r#"
            Str.release_excess_capacity ""
            "#
        ),
        (RocStr::empty().capacity(), RocStr::empty()),
        RocStr,
        |value: RocStr| (value.capacity(), value)
    );
}

// if this gets implemented, then add gen-dev to positive case above
#[test]
#[cfg(feature = "gen-dev")]
#[should_panic(expected = "not yet implemented: low level, StrReleaseExcessCapacity")]
fn release_excess_capacity_empty() {
    assert_evals_to!(
        indoc!(
            r#"
            Str.release_excess_capacity ""
            "#
        ),
        (RocStr::empty().capacity(), RocStr::empty()),
        RocStr,
        |value: RocStr| (value.capacity(), value)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn str_contains_positive() {
    assert_evals_to!(
        r#"
        Str.contains "foobarbaz" "bar"
        "#,
        true,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn str_contains_negative() {
    assert_evals_to!(
        r#"
        Str.contains "apple" "orange"
        "#,
        false,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn str_contains_empty_positive() {
    assert_evals_to!(
        r#"
        Str.contains "anything" ""
        "#,
        true,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn str_contains_empty_negative() {
    assert_evals_to!(
        r#"
        Str.contains "" "anything"
        "#,
        false,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn str_contains_self() {
    assert_evals_to!(
        r#"
        Str.contains "self" "self"
        "#,
        true,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn str_drop_prefix() {
    assert_evals_to!(
        r#"
        Str.drop_prefix "" "foo"
        "#,
        RocStr::from(""),
        RocStr
    );

    assert_evals_to!(
        r#"
        Str.drop_prefix "bar" "foo"
        "#,
        RocStr::from("bar"),
        RocStr
    );

    assert_evals_to!(
        r#"
        Str.drop_prefix "foobar" "foo"
        "#,
        RocStr::from("bar"),
        RocStr
    );

    assert_evals_to!(
        r#"
        Str.drop_prefix "fooBarThisIsDefinitelyAReallyLongAndNotaShortString" "foo"
        "#,
        RocStr::from("BarThisIsDefinitelyAReallyLongAndNotaShortString"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn str_drop_suffix() {
    assert_evals_to!(
        r#"
        Str.drop_suffix "" "foo"
        "#,
        RocStr::from(""),
        RocStr
    );

    assert_evals_to!(
        r#"
        Str.drop_suffix "bar" "foo"
        "#,
        RocStr::from("bar"),
        RocStr
    );

    assert_evals_to!(
        r#"
        Str.drop_suffix "barfoo" "foo"
        "#,
        RocStr::from("bar"),
        RocStr
    );

    assert_evals_to!(
        r#"
        Str.drop_suffix "BarThisIsDefinitelyAReallyLongAndNotaShortStringfoo" "foo"
        "#,
        RocStr::from("BarThisIsDefinitelyAReallyLongAndNotaShortString"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn with_ascii_lowercased() {
    assert_evals_to!(
        r#"
        Str.with_ascii_lowercased("cOFFÉ")
        "#,
        RocStr::from("coffÉ"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn with_ascii_lowercased_non_zero_refcount() {
    assert_evals_to!(
        r#"
        original = "cOFFÉ cOFFÉ cOFFÉ cOFFÉ cOFFÉ cOFFÉ cOFFÉ cOFFÉ cOFFÉ cOFFÉ cOFFÉ cOFFÉ"
        res = Str.with_ascii_lowercased(original)
        Str.drop_prefix(res, original)
        "#,
        RocStr::from("coffÉ coffÉ coffÉ coffÉ coffÉ coffÉ coffÉ coffÉ coffÉ coffÉ coffÉ coffÉ"),
        RocStr
    );
}
