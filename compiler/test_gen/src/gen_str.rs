#![cfg(not(feature = "gen-wasm"))]

#[cfg(feature = "gen-llvm")]
use crate::helpers::llvm::assert_evals_to;
#[cfg(feature = "gen-llvm")]
use crate::helpers::llvm::assert_llvm_evals_to;

#[cfg(feature = "gen-dev")]
use crate::helpers::dev::assert_evals_to;
#[cfg(feature = "gen-dev")]
use crate::helpers::dev::assert_evals_to as assert_llvm_evals_to;

#[allow(unused_imports)]
use indoc::indoc;
#[allow(unused_imports)]
use roc_std::{RocList, RocStr};

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn str_split_bigger_delimiter_small_str() {
    assert_evals_to!(
        indoc!(
            r#"
                    List.len (Str.split "hello" "JJJJ there")
                "#
        ),
        1,
        i64
    );

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
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
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
        RocStr::from_slice(b"JJJJJJJJJJJJJJJJJJJJJJJJJ"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
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
        RocStr::from_slice(b"JJJ"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn str_split_big_str_small_delimiter() {
    assert_evals_to!(
        indoc!(
            r#"
                    Str.split "01234567789abcdefghi?01234567789abcdefghi" "?"
                "#
        ),
        RocList::from_slice(&[
            RocStr::from_slice(b"01234567789abcdefghi"),
            RocStr::from_slice(b"01234567789abcdefghi")
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
            RocStr::from_slice(b"01234567789abcdefghi "),
            RocStr::from_slice(b" 01234567789abcdefghi")
        ]),
        RocList<RocStr>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn str_split_small_str_small_delimiter() {
    assert_evals_to!(
        indoc!(
            r#"
                    Str.split "J!J!J" "!"
                "#
        ),
        RocList::from_slice(&[
            RocStr::from_slice(b"J"),
            RocStr::from_slice(b"J"),
            RocStr::from_slice(b"J")
        ]),
        RocList<RocStr>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn str_split_bigger_delimiter_big_strs() {
    assert_evals_to!(
        indoc!(
            r#"
                    Str.split
                        "string to split is shorter"
                        "than the delimiter which happens to be very very long"
                "#
        ),
        RocList::from_slice(&[RocStr::from_slice(b"string to split is shorter")]),
        RocList<RocStr>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn str_split_empty_strs() {
    assert_evals_to!(
        indoc!(
            r#"
                    Str.split "" ""
                "#
        ),
        RocList::from_slice(&[RocStr::from_slice(b"")]),
        RocList<RocStr>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn str_split_minimal_example() {
    assert_evals_to!(
        indoc!(
            r#"
                    Str.split "a," ","
                "#
        ),
        RocList::from_slice(&[RocStr::from_slice(b"a"), RocStr::from_slice(b"")]),
        RocList<RocStr>
    )
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
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
        i64
    );

    assert_evals_to!(
        indoc!(
            r#"
                    Str.split
                        "1---- ---- ---- ---- ----2---- ---- ---- ---- ----"
                        "---- ---- ---- ---- ----"
                "#
        ),
        RocList::from_slice(&[
            RocStr::from_slice(b"1"),
            RocStr::from_slice(b"2"),
            RocStr::from_slice(b"")
        ]),
        RocList<RocStr>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn str_split_small_str_20_char_delimiter() {
    assert_evals_to!(
        indoc!(
            r#"
                    Str.split
                        "3|-- -- -- -- -- -- |4|-- -- -- -- -- -- |"
                        "|-- -- -- -- -- -- |"
                "#
        ),
        RocList::from_slice(&[
            RocStr::from_slice(b"3"),
            RocStr::from_slice(b"4"),
            RocStr::from_slice(b"")
        ]),
        RocList<RocStr>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn str_concat_big_to_big() {
    assert_evals_to!(
            indoc!(
                r#"
                    Str.concat
                        "First string that is fairly long. Longer strings make for different errors. "
                        "Second string that is also fairly long. Two long strings test things that might not appear with short strings."
                "#
            ),
            RocStr::from_slice(b"First string that is fairly long. Longer strings make for different errors. Second string that is also fairly long. Two long strings test things that might not appear with short strings."),
            RocStr
        );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev"))]
fn small_str_literal() {
    assert_llvm_evals_to!(
        "\"JJJJJJJJJJJJJJJ\"",
        [
            0x4a,
            0x4a,
            0x4a,
            0x4a,
            0x4a,
            0x4a,
            0x4a,
            0x4a,
            0x4a,
            0x4a,
            0x4a,
            0x4a,
            0x4a,
            0x4a,
            0x4a,
            0b1000_1111
        ],
        [u8; 16]
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
            0b1000_0001
        ],
        [u8; 16]
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev"))]
fn small_str_concat_empty_first_arg() {
    assert_llvm_evals_to!(
        r#"Str.concat "" "JJJJJJJJJJJJJJJ""#,
        [
            0x4a,
            0x4a,
            0x4a,
            0x4a,
            0x4a,
            0x4a,
            0x4a,
            0x4a,
            0x4a,
            0x4a,
            0x4a,
            0x4a,
            0x4a,
            0x4a,
            0x4a,
            0b1000_1111
        ],
        [u8; 16]
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev"))]
fn small_str_concat_empty_second_arg() {
    assert_llvm_evals_to!(
        r#"Str.concat "JJJJJJJJJJJJJJJ" """#,
        [
            0x4a,
            0x4a,
            0x4a,
            0x4a,
            0x4a,
            0x4a,
            0x4a,
            0x4a,
            0x4a,
            0x4a,
            0x4a,
            0x4a,
            0x4a,
            0x4a,
            0x4a,
            0b1000_1111
        ],
        [u8; 16]
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn small_str_concat_small_to_big() {
    assert_evals_to!(
        r#"Str.concat "abc" " this is longer than 15 chars""#,
        RocStr::from_slice(b"abc this is longer than 15 chars"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev"))]
fn small_str_concat_small_to_small_staying_small() {
    assert_llvm_evals_to!(
        r#"Str.concat "J" "JJJJJJJJJJJJJJ""#,
        [
            0x4a,
            0x4a,
            0x4a,
            0x4a,
            0x4a,
            0x4a,
            0x4a,
            0x4a,
            0x4a,
            0x4a,
            0x4a,
            0x4a,
            0x4a,
            0x4a,
            0x4a,
            0b1000_1111
        ],
        [u8; 16]
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev"))]
fn small_str_concat_small_to_small_overflow_to_big() {
    assert_evals_to!(
        r#"Str.concat "abcdefghijklm" "nopqrstuvwxyz""#,
        RocStr::from_slice(b"abcdefghijklmnopqrstuvwxyz"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev"))]
fn str_concat_empty() {
    assert_evals_to!(r#"Str.concat "" """#, RocStr::default(), RocStr);
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn small_str_is_empty() {
    assert_evals_to!(r#"Str.isEmpty "abc""#, false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn big_str_is_empty() {
    assert_evals_to!(
        r#"Str.isEmpty "this is more than 15 chars long""#,
        false,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn empty_str_is_empty() {
    assert_evals_to!(r#"Str.isEmpty """#, true, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn str_starts_with() {
    assert_evals_to!(r#"Str.startsWith "hello world" "hell""#, true, bool);
    assert_evals_to!(r#"Str.startsWith "hello world" """#, true, bool);
    assert_evals_to!(r#"Str.startsWith "nope" "hello world""#, false, bool);
    assert_evals_to!(r#"Str.startsWith "hell" "hello world""#, false, bool);
    assert_evals_to!(r#"Str.startsWith "" "hello world""#, false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn str_starts_with_code_point() {
    assert_evals_to!(
        &format!(r#"Str.startsWithCodePt "foobar" {}"#, 'f' as u32),
        true,
        bool
    );
    assert_evals_to!(
        &format!(r#"Str.startsWithCodePt "zoobar" {}"#, 'f' as u32),
        false,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn str_ends_with() {
    assert_evals_to!(r#"Str.endsWith "hello world" "world""#, true, bool);
    assert_evals_to!(r#"Str.endsWith "nope" "hello world""#, false, bool);
    assert_evals_to!(r#"Str.endsWith "" "hello world""#, false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn str_count_graphemes_small_str() {
    assert_evals_to!(r#"Str.countGraphemes "å🤔""#, 2, usize);
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn str_count_graphemes_three_js() {
    assert_evals_to!(r#"Str.countGraphemes "JJJ""#, 3, usize);
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn str_count_graphemes_big_str() {
    assert_evals_to!(
        r#"Str.countGraphemes "6🤔å🤔e¥🤔çppkd🙃1jdal🦯asdfa∆ltråø˚waia8918.,🏅jjc""#,
        45,
        usize
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn str_starts_with_same_big_str() {
    assert_evals_to!(
        r#"Str.startsWith "123456789123456789" "123456789123456789""#,
        true,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn str_starts_with_different_big_str() {
    assert_evals_to!(
        r#"Str.startsWith "12345678912345678910" "123456789123456789""#,
        true,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn str_starts_with_same_small_str() {
    assert_evals_to!(r#"Str.startsWith "1234" "1234""#, true, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn str_starts_with_different_small_str() {
    assert_evals_to!(r#"Str.startsWith "1234" "12""#, true, bool);
}
#[test]
#[cfg(any(feature = "gen-llvm"))]
fn str_starts_with_false_small_str() {
    assert_evals_to!(r#"Str.startsWith "1234" "23""#, false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn str_from_int() {
    assert_evals_to!(
        r#"Str.fromInt 1234"#,
        roc_std::RocStr::from_slice("1234".as_bytes()),
        roc_std::RocStr
    );
    assert_evals_to!(
        r#"Str.fromInt 0"#,
        roc_std::RocStr::from_slice("0".as_bytes()),
        roc_std::RocStr
    );
    assert_evals_to!(
        r#"Str.fromInt -1"#,
        roc_std::RocStr::from_slice("-1".as_bytes()),
        roc_std::RocStr
    );

    let max = format!("{}", i64::MAX);
    assert_evals_to!(
        r#"Str.fromInt Num.maxInt"#,
        RocStr::from_slice(max.as_bytes()),
        RocStr
    );

    let min = format!("{}", i64::MIN);
    assert_evals_to!(
        r#"Str.fromInt Num.minInt"#,
        RocStr::from_slice(min.as_bytes()),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn str_from_utf8_pass_single_ascii() {
    assert_evals_to!(
        indoc!(
            r#"
                    when Str.fromUtf8 [ 97 ] is
                        Ok val -> val
                        Err _ -> ""
                "#
        ),
        roc_std::RocStr::from_slice("a".as_bytes()),
        roc_std::RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn str_from_utf8_pass_many_ascii() {
    assert_evals_to!(
        indoc!(
            r#"
                    when Str.fromUtf8 [ 97, 98, 99, 0x7E ] is
                        Ok val -> val
                        Err _ -> ""
                "#
        ),
        roc_std::RocStr::from_slice("abc~".as_bytes()),
        roc_std::RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn str_from_utf8_pass_single_unicode() {
    assert_evals_to!(
        indoc!(
            r#"
                    when Str.fromUtf8 [ 0xE2, 0x88, 0x86 ] is
                        Ok val -> val
                        Err _ -> ""
                "#
        ),
        roc_std::RocStr::from_slice("∆".as_bytes()),
        roc_std::RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn str_from_utf8_pass_many_unicode() {
    assert_evals_to!(
        indoc!(
            r#"
                    when Str.fromUtf8 [ 0xE2, 0x88, 0x86, 0xC5, 0x93, 0xC2, 0xAC ] is
                        Ok val -> val
                        Err _ -> ""
                "#
        ),
        roc_std::RocStr::from_slice("∆œ¬".as_bytes()),
        roc_std::RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn str_from_utf8_pass_single_grapheme() {
    assert_evals_to!(
        indoc!(
            r#"
                    when Str.fromUtf8 [ 0xF0, 0x9F, 0x92, 0x96 ] is
                        Ok val -> val
                        Err _ -> ""
                "#
        ),
        roc_std::RocStr::from_slice("💖".as_bytes()),
        roc_std::RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn str_from_utf8_pass_many_grapheme() {
    assert_evals_to!(
        indoc!(
            r#"
                    when Str.fromUtf8 [ 0xF0, 0x9F, 0x92, 0x96, 0xF0, 0x9F, 0xA4, 0xA0, 0xF0, 0x9F, 0x9A, 0x80 ] is
                        Ok val -> val
                        Err _ -> ""
                "#
        ),
        roc_std::RocStr::from_slice("💖🤠🚀".as_bytes()),
        roc_std::RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn str_from_utf8_pass_all() {
    assert_evals_to!(
        indoc!(
            r#"
                    when Str.fromUtf8 [ 0xF0, 0x9F, 0x92, 0x96, 98, 0xE2, 0x88, 0x86 ] is
                        Ok val -> val
                        Err _ -> ""
                "#
        ),
        roc_std::RocStr::from_slice("💖b∆".as_bytes()),
        roc_std::RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn str_from_utf8_fail_invalid_start_byte() {
    assert_evals_to!(
        indoc!(
            r#"
                    when Str.fromUtf8 [ 97, 98, 0x80, 99 ] is
                        Err (BadUtf8 InvalidStartByte byteIndex) ->
                            if byteIndex == 2 then
                                "a"
                            else
                                "b"
                        _ -> ""
                "#
        ),
        roc_std::RocStr::from_slice("a".as_bytes()),
        roc_std::RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn str_from_utf8_fail_unexpected_end_of_sequence() {
    assert_evals_to!(
        indoc!(
            r#"
                    when Str.fromUtf8 [ 97, 98, 99, 0xC2 ] is
                        Err (BadUtf8 UnexpectedEndOfSequence byteIndex) ->
                            if byteIndex == 3 then
                                "a"
                            else
                                "b"
                        _ -> ""
                "#
        ),
        roc_std::RocStr::from_slice("a".as_bytes()),
        roc_std::RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn str_from_utf8_fail_expected_continuation() {
    assert_evals_to!(
        indoc!(
            r#"
                    when Str.fromUtf8 [ 97, 98, 99, 0xC2, 0x00 ] is
                        Err (BadUtf8 ExpectedContinuation byteIndex) ->
                            if byteIndex == 3 then
                                "a"
                            else
                                "b"
                        _ -> ""
                "#
        ),
        roc_std::RocStr::from_slice("a".as_bytes()),
        roc_std::RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn str_from_utf8_fail_overlong_encoding() {
    assert_evals_to!(
        indoc!(
            r#"
                    when Str.fromUtf8 [ 97, 0xF0, 0x80, 0x80, 0x80 ] is
                        Err (BadUtf8 OverlongEncoding byteIndex) ->
                            if byteIndex == 1 then
                                "a"
                            else
                                "b"
                        _ -> ""
                "#
        ),
        roc_std::RocStr::from_slice("a".as_bytes()),
        roc_std::RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn str_from_utf8_fail_codepoint_too_large() {
    assert_evals_to!(
        indoc!(
            r#"
                    when Str.fromUtf8 [ 97, 0xF4, 0x90, 0x80, 0x80 ] is
                        Err (BadUtf8 CodepointTooLarge byteIndex) ->
                            if byteIndex == 1 then
                                "a"
                            else
                                "b"
                        _ -> ""
                "#
        ),
        roc_std::RocStr::from_slice("a".as_bytes()),
        roc_std::RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn str_from_utf8_fail_surrogate_half() {
    assert_evals_to!(
        indoc!(
            r#"
                    when Str.fromUtf8 [ 97, 98, 0xED, 0xA0, 0x80 ] is
                        Err (BadUtf8 EncodesSurrogateHalf byteIndex) ->
                            if byteIndex == 2 then
                                "a"
                            else
                                "b"
                        _ -> ""
                "#
        ),
        roc_std::RocStr::from_slice("a".as_bytes()),
        roc_std::RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
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
    let long = RocStr::from_slice("loremipsumdolarsitamet".as_bytes());
    let short = RocStr::from_slice("x".as_bytes());
    let empty = RocStr::from_slice("".as_bytes());

    debug_assert_eq!(long.clone(), long);
    debug_assert_eq!(short.clone(), short);
    debug_assert_eq!(empty.clone(), empty);
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn nested_recursive_literal() {
    assert_evals_to!(
        indoc!(
            r#"
                Expr : [ Add Expr Expr, Val I64, Var I64 ]

                expr : Expr
                expr = Add (Add (Val 3) (Val 1)) (Add (Val 1) (Var 1))

                printExpr : Expr -> Str
                printExpr = \e ->
                    when e is
                        Add a b ->
                            "Add ("
                                |> Str.concat (printExpr a)
                                |> Str.concat ") ("
                                |> Str.concat (printExpr b)
                                |> Str.concat ")"
                        Val v -> "Val " |> Str.concat (Str.fromInt v)
                        Var v -> "Var " |> Str.concat (Str.fromInt v)

                printExpr expr
                "#
        ),
        RocStr::from_slice(b"Add (Add (Val 3) (Val 1)) (Add (Val 1) (Var 1))"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn str_join_comma_small() {
    assert_evals_to!(
        r#"Str.joinWith ["1", "2"] ", " "#,
        RocStr::from("1, 2"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn str_join_comma_big() {
    assert_evals_to!(
        r#"Str.joinWith ["10000000", "2000000", "30000000"] ", " "#,
        RocStr::from("10000000, 2000000, 30000000"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn str_join_comma_single() {
    assert_evals_to!(r#"Str.joinWith ["1"] ", " "#, RocStr::from("1"), RocStr);
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn str_from_float() {
    assert_evals_to!(r#"Str.fromFloat 3.14"#, RocStr::from("3.14"), RocStr);
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
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
#[cfg(any(feature = "gen-llvm"))]
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
#[cfg(any(feature = "gen-llvm"))]
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
#[cfg(any(feature = "gen-llvm"))]
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
#[cfg(any(feature = "gen-llvm"))]
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
#[cfg(any(feature = "gen-llvm"))]
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
#[cfg(any(feature = "gen-llvm"))]
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
#[cfg(any(feature = "gen-llvm"))]
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
#[cfg(any(feature = "gen-llvm"))]
fn str_repeat_small() {
    assert_evals_to!(
        indoc!(r#"Str.repeat "Roc" 3"#),
        RocStr::from("RocRocRoc"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn str_repeat_big() {
    assert_evals_to!(
        indoc!(r#"Str.repeat "more than 16 characters" 2"#),
        RocStr::from("more than 16 charactersmore than 16 characters"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn str_repeat_empty_string() {
    assert_evals_to!(indoc!(r#"Str.repeat "" 3"#), RocStr::from(""), RocStr);
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn str_repeat_zero_times() {
    assert_evals_to!(indoc!(r#"Str.repeat "Roc" 0"#), RocStr::from(""), RocStr);
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn str_trim_empty_string() {
    assert_evals_to!(indoc!(r#"Str.trim """#), RocStr::from(""), RocStr);
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn str_trim_small_blank_string() {
    assert_evals_to!(indoc!(r#"Str.trim " ""#), RocStr::from(""), RocStr);
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn str_trim_small_to_small() {
    assert_evals_to!(
        indoc!(r#"Str.trim "  hello world  ""#),
        RocStr::from("hello world"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn str_trim_large_to_large_unique() {
    assert_evals_to!(
        indoc!(r#"Str.trim (Str.concat "  " "hello world from a large string ")"#),
        RocStr::from("hello world from a large string"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn str_trim_large_to_small_unique() {
    assert_evals_to!(
        indoc!(r#"Str.trim (Str.concat "  " "hello world        ")"#),
        RocStr::from("hello world"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
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
#[cfg(any(feature = "gen-llvm"))]
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
#[cfg(any(feature = "gen-llvm"))]
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
#[cfg(any(feature = "gen-llvm"))]
fn str_trim_left_small_blank_string() {
    assert_evals_to!(indoc!(r#"Str.trimLeft " ""#), RocStr::from(""), RocStr);
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn str_trim_left_small_to_small() {
    assert_evals_to!(
        indoc!(r#"Str.trimLeft "  hello world  ""#),
        RocStr::from("hello world  "),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn str_trim_left_large_to_large_unique() {
    assert_evals_to!(
        indoc!(r#"Str.trimLeft (Str.concat "    " "hello world from a large string ")"#),
        RocStr::from("hello world from a large string "),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn str_trim_left_large_to_small_unique() {
    assert_evals_to!(
        indoc!(r#"Str.trimLeft (Str.concat "  " "hello world        ")"#),
        RocStr::from("hello world        "),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn str_trim_left_large_to_large_shared() {
    assert_evals_to!(
        indoc!(
            r#"
               original : Str
               original = " hello world world "

               { trimmed: Str.trimLeft original, original: original }
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
#[cfg(any(feature = "gen-llvm"))]
fn str_trim_left_large_to_small_shared() {
    assert_evals_to!(
        indoc!(
            r#"
               original : Str
               original = " hello world             "

               { trimmed: Str.trimLeft original, original: original }
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
#[cfg(any(feature = "gen-llvm"))]
fn str_trim_left_small_to_small_shared() {
    assert_evals_to!(
        indoc!(
            r#"
               original : Str
               original = " hello world "

               { trimmed: Str.trimLeft original, original: original }
               "#
        ),
        (RocStr::from(" hello world "), RocStr::from("hello world "),),
        (RocStr, RocStr)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn str_trim_right_small_blank_string() {
    assert_evals_to!(indoc!(r#"Str.trimRight " ""#), RocStr::from(""), RocStr);
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn str_trim_right_small_to_small() {
    assert_evals_to!(
        indoc!(r#"Str.trimRight "  hello world  ""#),
        RocStr::from("  hello world"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn str_trim_right_large_to_large_unique() {
    assert_evals_to!(
        indoc!(r#"Str.trimRight (Str.concat " hello world from a large string" "    ")"#),
        RocStr::from(" hello world from a large string"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn str_trim_right_large_to_small_unique() {
    assert_evals_to!(
        indoc!(r#"Str.trimRight (Str.concat "        hello world" "  ")"#),
        RocStr::from("        hello world"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn str_trim_right_large_to_large_shared() {
    assert_evals_to!(
        indoc!(
            r#"
               original : Str
               original = " hello world world "

               { trimmed: Str.trimRight original, original: original }
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
#[cfg(any(feature = "gen-llvm"))]
fn str_trim_right_large_to_small_shared() {
    assert_evals_to!(
        indoc!(
            r#"
               original : Str
               original = "             hello world "

               { trimmed: Str.trimRight original, original: original }
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
#[cfg(any(feature = "gen-llvm"))]
fn str_trim_right_small_to_small_shared() {
    assert_evals_to!(
        indoc!(
            r#"
               original : Str
               original = " hello world "

               { trimmed: Str.trimRight original, original: original }
               "#
        ),
        (RocStr::from(" hello world "), RocStr::from(" hello world"),),
        (RocStr, RocStr)
    );
}
