#![cfg(feature = "gen-llvm")]

#[cfg(feature = "gen-llvm")]
use crate::helpers::llvm::assert_evals_to;

// #[cfg(feature = "gen-dev")]
// use crate::helpers::dev::assert_evals_to;

// #[cfg(feature = "gen-wasm")]
// use crate::helpers::wasm::assert_evals_to;

use crate::helpers::with_larger_debug_stack;
//use crate::assert_wasm_evals_to as assert_evals_to;
use indoc::indoc;
use roc_std::{RocList, RocStr};

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn roc_list_construction() {
    let list = RocList::from_slice(&[1i64; 23]);
    assert_eq!(&list, &list);
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn empty_list_literal() {
    assert_evals_to!("[]", RocList::from_slice(&[]), RocList<i64>);
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_literal_empty_record() {
    assert_evals_to!("[{}]", RocList::from_slice(&[()]), RocList<()>);
}

#[test]
fn int_singleton_list_literal() {
    assert_evals_to!("[1, 2]", RocList::from_slice(&[1, 2]), RocList<i64>);
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn int_list_literal() {
    assert_evals_to!("[ 12, 9 ]", RocList::from_slice(&[12, 9]), RocList<i64>);
    assert_evals_to!(
        "[ 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1 ]",
        RocList::from_slice(&[1i64; 23]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn bool_list_literal() {
    // NOTE: make sure to explicitly declare the elements to be of type bool, or
    // use both True and False; only using one of them causes the list to in practice be
    // of type `List [ True ]` or `List [ False ]`, those are tag unions with one constructor
    // and not fields, and don't have a runtime representation.
    assert_evals_to!(
        indoc!(
            r#"
               false : Bool
               false = False

               [ false ]
               "#
        ),
        RocList::from_slice(&[false; 1]),
        RocList<bool>
    );

    assert_evals_to!(
        "[ True, False, True ]",
        RocList::from_slice(&[true, false, true]),
        RocList<bool>
    );

    assert_evals_to!(
        indoc!(
            r#"
               false : Bool
               false = False

               [false ]
               "#
        ),
        RocList::from_slice(&[false; 1]),
        RocList<bool>
    );

    assert_evals_to!(
        indoc!(
            r#"
               true : Bool
               true = True

               List.repeat 23 true
               "#
        ),
        RocList::from_slice(&[true; 23]),
        RocList<bool>
    );

    assert_evals_to!(
        indoc!(
            r#"
               true : Bool
               true = True

               List.repeat 23 { x: true, y: true }
               "#
        ),
        RocList::from_slice(&[[true, true]; 23]),
        RocList<[bool; 2]>
    );

    assert_evals_to!(
        indoc!(
            r#"
               true : Bool
               true = True

               List.repeat 23 { x: true, y: true, a: true, b: true, c: true, d : true, e: true, f: true }
               "#
        ),
        RocList::from_slice(&[[true, true, true, true, true, true, true, true]; 23]),
        RocList<[bool; 8]>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn variously_sized_list_literals() {
    assert_evals_to!("[]", RocList::from_slice(&[]), RocList<i64>);
    assert_evals_to!("[1]", RocList::from_slice(&[1]), RocList<i64>);
    assert_evals_to!("[1, 2]", RocList::from_slice(&[1, 2]), RocList<i64>);
    assert_evals_to!("[1, 2, 3]", RocList::from_slice(&[1, 2, 3]), RocList<i64>);
    assert_evals_to!(
        "[1, 2, 3, 4]",
        RocList::from_slice(&[1, 2, 3, 4]),
        RocList<i64>
    );
    assert_evals_to!(
        "[1, 2, 3, 4, 5]",
        RocList::from_slice(&[1, 2, 3, 4, 5]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_append() {
    assert_evals_to!(
        "List.append [1] 2",
        RocList::from_slice(&[1, 2]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.append [1, 1] 2",
        RocList::from_slice(&[1, 1, 2]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_take_first() {
    assert_evals_to!(
        "List.takeFirst [1, 2, 3] 2",
        RocList::from_slice(&[1, 2]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.takeFirst [1, 2, 3] 0",
        RocList::from_slice(&[]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.takeFirst [] 1",
        RocList::from_slice(&[]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.takeFirst [1,2] 5",
        RocList::from_slice(&[1, 2]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_take_last() {
    assert_evals_to!(
        "List.takeLast [1, 2, 3] 2",
        RocList::from_slice(&[2, 3]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.takeLast [1, 2, 3] 0",
        RocList::from_slice(&[]),
        RocList<i64>
    );
    assert_evals_to!("List.takeLast [] 1", RocList::from_slice(&[]), RocList<i64>);
    assert_evals_to!(
        "List.takeLast [1,2] 5",
        RocList::from_slice(&[1, 2]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_sublist() {
    assert_evals_to!(
        "List.sublist [1, 2, 3] { start: 0 , len: 2 } ",
        RocList::from_slice(&[1, 2]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.sublist [1, 2, 3] { start: 1 , len: 2 } ",
        RocList::from_slice(&[2, 3]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.sublist [1, 2, 3] { start: 2 , len: 2 } ",
        RocList::from_slice(&[3]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.sublist [1, 2, 3] { start: 3 , len: 2 } ",
        RocList::from_slice(&[]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.sublist [] { start: 1 , len: 1 } ",
        RocList::from_slice(&[]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.sublist [1, 2, 3] { start: 1 , len: 0 } ",
        RocList::from_slice(&[]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.sublist [1, 2, 3] { start: 0 , len: 5 } ",
        RocList::from_slice(&[1, 2, 3]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_split() {
    assert_evals_to!(
        r#"
               list = List.split [1, 2, 3] 0
               list.before
            "#,
        RocList::from_slice(&[]),
        RocList<i64>
    );
    assert_evals_to!(
        r#"
               list = List.split [1, 2, 3] 0
               list.others
            "#,
        RocList::from_slice(&[1, 2, 3]),
        RocList<i64>
    );

    assert_evals_to!(
        "List.split [1, 2, 3] 1",
        (RocList::from_slice(&[1]), RocList::from_slice(&[2, 3]),),
        (RocList<i64>, RocList<i64>,)
    );
    assert_evals_to!(
        "List.split [1, 2, 3] 3",
        (RocList::from_slice(&[1, 2, 3]), RocList::from_slice(&[]),),
        (RocList<i64>, RocList<i64>,)
    );
    assert_evals_to!(
        "List.split [1, 2, 3] 4",
        (RocList::from_slice(&[1, 2, 3]), RocList::from_slice(&[]),),
        (RocList<i64>, RocList<i64>,)
    );
    assert_evals_to!(
        "List.split [] 1",
        (RocList::from_slice(&[]), RocList::from_slice(&[]),),
        (RocList<i64>, RocList<i64>,)
    );
}
#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_drop() {
    assert_evals_to!(
        "List.drop [1,2,3] 2",
        RocList::from_slice(&[3]),
        RocList<i64>
    );
    assert_evals_to!("List.drop [] 1", RocList::from_slice(&[]), RocList<i64>);
    assert_evals_to!("List.drop [1,2] 5", RocList::from_slice(&[]), RocList<i64>);
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_drop_at() {
    assert_evals_to!(
        "List.dropAt [1, 2, 3] 0",
        RocList::from_slice(&[2, 3]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.dropAt [0, 0, 0] 3",
        RocList::from_slice(&[0, 0, 0]),
        RocList<i64>
    );
    assert_evals_to!("List.dropAt [] 1", RocList::from_slice(&[]), RocList<i64>);
    assert_evals_to!("List.dropAt [0] 0", RocList::from_slice(&[]), RocList<i64>);
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_drop_at_shared() {
    assert_evals_to!(
        indoc!(
            r#"
               list : List I64
               list = [ if True then 4 else 4, 5, 6 ]

               { newList: List.dropAt list 0, original: list }
               "#
        ),
        (
            // new_list
            RocList::from_slice(&[5, 6]),
            // original
            RocList::from_slice(&[4, 5, 6]),
        ),
        (RocList<i64>, RocList<i64>,)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_drop_last() {
    assert_evals_to!(
        "List.dropLast [1, 2, 3]",
        RocList::from_slice(&[1, 2]),
        RocList<i64>
    );
    assert_evals_to!("List.dropLast []", RocList::from_slice(&[]), RocList<i64>);
    assert_evals_to!("List.dropLast [0]", RocList::from_slice(&[]), RocList<i64>);
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_drop_last_mutable() {
    assert_evals_to!(
        indoc!(
            r#"
               list : List I64
               list = [ if True then 4 else 4, 5, 6 ]

               { newList: List.dropLast list, original: list }
               "#
        ),
        (
            // new_list
            RocList::from_slice(&[4, 5]),
            // original
            RocList::from_slice(&[4, 5, 6]),
        ),
        (RocList<i64>, RocList<i64>,)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_drop_first() {
    assert_evals_to!(
        "List.dropFirst [1, 2, 3]",
        RocList::from_slice(&[2, 3]),
        RocList<i64>
    );
    assert_evals_to!("List.dropFirst []", RocList::from_slice(&[]), RocList<i64>);
    assert_evals_to!("List.dropFirst [0]", RocList::from_slice(&[]), RocList<i64>);
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_swap() {
    assert_evals_to!("List.swap [] 0 1", RocList::from_slice(&[]), RocList<i64>);
    assert_evals_to!(
        "List.swap [ 0 ] 1 2",
        RocList::from_slice(&[0]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.swap [ 1, 2 ] 0 1",
        RocList::from_slice(&[2, 1]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.swap [ 1, 2 ] 1 0",
        RocList::from_slice(&[2, 1]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.swap [ 0, 1, 2, 3, 4, 5 ] 2 4",
        RocList::from_slice(&[0, 1, 4, 3, 2, 5]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.swap [ 0, 1, 2 ] 1 3",
        RocList::from_slice(&[0, 1, 2]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.swap [ 1, 2, 3 ] 1 1",
        RocList::from_slice(&[1, 2, 3]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_append_to_empty_list() {
    assert_evals_to!("List.append [] 3", RocList::from_slice(&[3]), RocList<i64>);
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_append_to_empty_list_of_int() {
    assert_evals_to!(
        indoc!(
            r#"
                initThrees : List I64
                initThrees =
                    []

                List.append (List.append initThrees 3) 3
            "#
        ),
        RocList::from_slice(&[3, 3]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_append_bools() {
    assert_evals_to!(
        "List.append [ True, False ] True",
        RocList::from_slice(&[true, false, true]),
        RocList<bool>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_append_longer_list() {
    assert_evals_to!(
        "List.append [ 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22 ] 23",
        RocList::from_slice(&[11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_prepend() {
    assert_evals_to!("List.prepend [] 1", RocList::from_slice(&[1]), RocList<i64>);
    assert_evals_to!(
        "List.prepend [2] 1",
        RocList::from_slice(&[1, 2]),
        RocList<i64>
    );

    assert_evals_to!(
        indoc!(
            r#"
                init : List I64
                init =
                    []

                List.prepend (List.prepend init 4) 6
            "#
        ),
        RocList::from_slice(&[6, 4]),
        RocList<i64>
    );

    assert_evals_to!(
        indoc!(
            r#"
                init : List Str
                init =
                    ["foo"]

                List.prepend init "bar"
            "#
        ),
        RocList::from_slice(&[RocStr::from_slice(b"bar"), RocStr::from_slice(b"foo"),]),
        RocList<RocStr>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_prepend_bools() {
    assert_evals_to!(
        "List.prepend [ True, False ] True",
        RocList::from_slice(&[true, true, false]),
        RocList<bool>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_prepend_big_list() {
    assert_evals_to!(
        "List.prepend [ 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 100, 100, 100, 100 ] 9",
        RocList::from_slice(&[
            9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 100, 100, 100, 100
        ]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_walk_backwards_empty_all_inline() {
    assert_evals_to!(
        indoc!(
            r#"
            List.walkBackwards [0x1] 0 \state, elem -> state + elem
            "#
        ),
        1,
        i64
    );

    assert_evals_to!(
        indoc!(
            r#"
            empty : List I64
            empty =
                []

            List.walkBackwards empty 0 \state, elem -> state + elem
            "#
        ),
        0,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_walk_backwards_with_str() {
    assert_evals_to!(
        r#"List.walkBackwards [ "x", "y", "z" ] "<" Str.concat"#,
        RocStr::from("<zyx"),
        RocStr
    );

    assert_evals_to!(
        r#"List.walkBackwards [ "Second", "Third", "Fourth" ] "First" Str.concat"#,
        RocStr::from("FirstFourthThirdSecond"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_walk_backwards_with_record() {
    assert_evals_to!(
        indoc!(
            r#"
            Bit : [ Zero, One ]

            byte : List Bit
            byte = [ Zero, One, Zero, One, Zero, Zero, One, Zero ]

            initialCounts = { zeroes: 0, ones: 0 }

            acc = \r, b ->
                when b is
                    Zero -> { r & zeroes: r.zeroes + 1 }
                    One -> { r & ones: r.ones + 1 }

            finalCounts = List.walkBackwards byte initialCounts acc

            finalCounts.ones * 10 + finalCounts.zeroes
            "#
        ),
        35,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_walk_with_str() {
    assert_evals_to!(
        r#"List.walk [ "x", "y", "z" ] "<" Str.concat"#,
        RocStr::from("<xyz"),
        RocStr
    );

    assert_evals_to!(
        r#"List.walk [ "Second", "Third", "Fourth" ] "First" Str.concat"#,
        RocStr::from("FirstSecondThirdFourth"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_walk_subtraction() {
    assert_evals_to!(r#"List.walk [ 1, 2 ] 1 Num.sub"#, (1 - 1) - 2, i64);
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_walk_until_sum() {
    assert_evals_to!(
        r#"List.walkUntil [ 1, 2 ] 0 \a,b -> Continue (a + b)"#,
        3,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_walk_until_even_prefix_sum() {
    assert_evals_to!(
        r#"
        helper = \a, b ->
            if Num.isEven b then
                Continue (a + b)

            else
                Stop a

        List.walkUntil [ 2, 4, 8, 9 ] 0 helper"#,
        2 + 4 + 8,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_keep_if_empty_list_of_int() {
    assert_evals_to!(
        indoc!(
            r#"
            empty : List I64
            empty =
                []

            List.keepIf empty \_ -> True
            "#
        ),
        RocList::from_slice(&[]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_keep_if_empty_list() {
    assert_evals_to!(
        indoc!(
            r#"
            alwaysTrue : I64 -> Bool
            alwaysTrue = \_ ->
                True


            List.keepIf [] alwaysTrue
            "#
        ),
        RocList::from_slice(&[]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_keep_if_always_true_for_non_empty_list() {
    assert_evals_to!(
        indoc!(
            r#"
            alwaysTrue : I64 -> Bool
            alwaysTrue = \_ ->
                True

            oneThroughEight : List I64
            oneThroughEight =
                [1,2,3,4,5,6,7,8]

            List.keepIf oneThroughEight alwaysTrue
            "#
        ),
        RocList::from_slice(&[1, 2, 3, 4, 5, 6, 7, 8]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_keep_if_always_false_for_non_empty_list() {
    assert_evals_to!(
        indoc!(
            r#"
            alwaysFalse : I64 -> Bool
            alwaysFalse = \_ ->
                False

            List.keepIf [1,2,3,4,5,6,7,8] alwaysFalse
            "#
        ),
        RocList::from_slice(&[]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_keep_if_one() {
    assert_evals_to!(
        indoc!(
            r#"
            intIsLessThanThree : I64 -> Bool
            intIsLessThanThree = \i ->
                i < 3

            List.keepIf [1,2,3,4,5,6,7,8] intIsLessThanThree
            "#
        ),
        RocList::from_slice(&[1, 2]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_keep_if_str_is_hello() {
    assert_evals_to!(
        indoc!(
            r#"
             List.keepIf ["x", "y", "x"] (\x -> x == "x")
             "#
        ),
        RocList::from_slice(&[
            RocStr::from_slice("x".as_bytes()),
            RocStr::from_slice("x".as_bytes())
        ]),
        RocList<RocStr>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_map_on_empty_list_with_int_layout() {
    assert_evals_to!(
        indoc!(
            r#"
            empty : List I64
            empty =
                []

            List.map empty (\x -> x)
            "#
        ),
        RocList::from_slice(&[]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_map_on_non_empty_list() {
    assert_evals_to!(
        indoc!(
            r#"
            nonEmpty : List I64
            nonEmpty =
                [ 1 ]

            List.map nonEmpty (\x -> x)
            "#
        ),
        RocList::from_slice(&[1]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_map_changes_input() {
    assert_evals_to!(
        indoc!(
            r#"
            nonEmpty : List I64
            nonEmpty =
                [ 1 ]

            List.map nonEmpty (\x -> x + 1)
            "#
        ),
        RocList::from_slice(&[2]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_map_on_big_list() {
    assert_evals_to!(
        indoc!(
            r#"
            nonEmpty : List I64
            nonEmpty =
                [ 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5 ]

            List.map nonEmpty (\x -> x * 2)
            "#
        ),
        RocList::from_slice(&[
            2, 4, 6, 8, 10, 2, 4, 6, 8, 10, 2, 4, 6, 8, 10, 2, 4, 6, 8, 10, 2, 4, 6, 8, 10
        ]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_map_with_type_change() {
    assert_evals_to!(
        indoc!(
            r#"
            nonEmpty : List I64
            nonEmpty =
                [ 1, 1, -4, 1, 2 ]


            List.map nonEmpty (\x -> x > 0)
            "#
        ),
        RocList::from_slice(&[true, true, false, true, true]),
        RocList<bool>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_map_using_defined_function() {
    assert_evals_to!(
        indoc!(
            r#"
             nonEmpty : List I64
             nonEmpty =
                 [ 2, 2, -4, 2, 3 ]

             greaterThanOne : I64 -> Bool
             greaterThanOne = \i ->
                 i > 1

             List.map nonEmpty greaterThanOne
             "#
        ),
        RocList::from_slice(&[true, true, false, true, true]),
        RocList<bool>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_map_all_inline() {
    assert_evals_to!(
        indoc!(
            r#"
            List.map [] (\x -> x > 0)
            "#
        ),
        RocList::from_slice(&[]),
        RocList<bool>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_map_closure() {
    assert_evals_to!(
        indoc!(
            r#"
            pi : F64
            pi = 3.14

            single : List F64
            single =
                [ 0 ]

            List.map single (\x -> x + pi)
            "#
        ),
        RocList::from_slice(&[3.14]),
        RocList<f64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_map4_group() {
    assert_evals_to!(
        indoc!(
            r#"
            List.map4 [1,2,3] [3,2,1] [2,1,3] [3,1,2] (\a, b, c, d -> Group a b c d)
            "#
        ),
        RocList::from_slice(&[(1, 3, 2, 3), (2, 2, 1, 1), (3, 1, 3, 2)]),
        RocList<(i64, i64, i64, i64)>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_map4_different_length() {
    assert_evals_to!(
        indoc!(
            r#"
            List.map4
                ["h", "i", "j", "k"]
                ["o", "p", "q"]
                ["l", "m"]
                ["a"]
                (\a, b, c, d -> Str.concat a (Str.concat b (Str.concat c d)))
            "#
        ),
        RocList::from_slice(&[RocStr::from_slice("hola".as_bytes()),]),
        RocList<RocStr>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_map3_group() {
    assert_evals_to!(
        indoc!(
            r#"
            List.map3 [1,2,3] [3,2,1] [2,1,3] (\a, b, c -> Group a b c)
            "#
        ),
        RocList::from_slice(&[(1, 3, 2), (2, 2, 1), (3, 1, 3)]),
        RocList<(i64, i64, i64)>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_map3_different_length() {
    assert_evals_to!(
        indoc!(
            r#"
            List.map3
                ["a", "b", "d"]
                ["b", "x"]
                ["c"]
                (\a, b, c -> Str.concat a (Str.concat b c))
            "#
        ),
        RocList::from_slice(&[RocStr::from_slice("abc".as_bytes()),]),
        RocList<RocStr>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_map2_pair() {
    assert_evals_to!(
        indoc!(
            r#"
            f = (\a,b -> Pair a b)
            List.map2 [1,2,3] [3,2,1] f
            "#
        ),
        RocList::from_slice(&[(1, 3), (2, 2), (3, 1)]),
        RocList<(i64, i64)>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_map2_different_lengths() {
    assert_evals_to!(
        indoc!(
            r#"
            List.map2
                ["a", "b", "lllllllllllllongnggg" ]
                ["b"]
                (\a, b -> Str.concat a b)
            "#
        ),
        RocList::from_slice(&[RocStr::from_slice("ab".as_bytes()),]),
        RocList<RocStr>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_join_empty_list() {
    assert_evals_to!("List.join []", RocList::from_slice(&[]), RocList<i64>);
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_join_one_list() {
    assert_evals_to!(
        "List.join [ [1, 2, 3 ] ]",
        RocList::from_slice(&[1, 2, 3]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_join_two_non_empty_lists() {
    assert_evals_to!(
        "List.join [ [1, 2, 3 ] , [4 ,5, 6] ]",
        RocList::from_slice(&[1, 2, 3, 4, 5, 6]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_join_two_non_empty_lists_of_float() {
    assert_evals_to!(
        "List.join [ [ 1.2, 1.1 ], [ 2.1, 2.2 ] ]",
        RocList::from_slice(&[1.2, 1.1, 2.1, 2.2]),
        RocList<f64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_join_to_big_list() {
    assert_evals_to!(
        indoc!(
            r#"
                List.join
                    [
                        [ 1.2, 1.1 ],
                        [ 2.1, 2.2 ],
                        [ 3.0, 4.0, 5.0, 6.1, 9.0 ],
                        [ 3.0, 4.0, 5.0, 6.1, 9.0 ],
                        [ 3.0, 4.0, 5.0, 6.1, 9.0 ],
                        [ 3.0, 4.0, 5.0, 6.1, 9.0 ],
                        [ 3.0, 4.0, 5.0, 6.1, 9.0 ]
                    ]
            "#
        ),
        RocList::from_slice(&[
            1.2, 1.1, 2.1, 2.2, 3.0, 4.0, 5.0, 6.1, 9.0, 3.0, 4.0, 5.0, 6.1, 9.0, 3.0, 4.0, 5.0,
            6.1, 9.0, 3.0, 4.0, 5.0, 6.1, 9.0, 3.0, 4.0, 5.0, 6.1, 9.0
        ]),
        RocList<f64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_join_defined_empty_list() {
    assert_evals_to!(
        indoc!(
            r#"
                empty : List F64
                empty =
                    []

                List.join [ [ 0.2, 11.11 ], empty ]
            "#
        ),
        RocList::from_slice(&[0.2, 11.11]),
        RocList<f64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_join_all_empty_lists() {
    assert_evals_to!(
        "List.join [ [], [], [] ]",
        RocList::from_slice(&[]),
        RocList<f64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_join_one_empty_list() {
    assert_evals_to!(
        "List.join [ [ 1.2, 1.1 ], [] ]",
        RocList::from_slice(&[1.2, 1.1]),
        RocList<f64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_single() {
    assert_evals_to!("List.single 1", RocList::from_slice(&[1]), RocList<i64>);
    assert_evals_to!("List.single 5.6", RocList::from_slice(&[5.6]), RocList<f64>);
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_repeat() {
    assert_evals_to!(
        "List.repeat 5 1",
        RocList::from_slice(&[1, 1, 1, 1, 1]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.repeat 4 2",
        RocList::from_slice(&[2, 2, 2, 2]),
        RocList<i64>
    );

    assert_evals_to!(
        "List.repeat 2 []",
        RocList::from_slice(&[RocList::default(), RocList::default()]),
        RocList<RocList<i64>>
    );

    assert_evals_to!(
        indoc!(
            r#"
                noStrs : List Str
                noStrs =
                    []

                List.repeat 2 noStrs
            "#
        ),
        RocList::from_slice(&[RocList::default(), RocList::default()]),
        RocList<RocList<i64>>
    );

    assert_evals_to!(
        "List.repeat 15 4",
        RocList::from_slice(&[4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_reverse() {
    assert_evals_to!(
        "List.reverse [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ]",
        RocList::from_slice(&[12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.reverse [1, 2, 3]",
        RocList::from_slice(&[3, 2, 1]),
        RocList<i64>
    );
    assert_evals_to!("List.reverse [4]", RocList::from_slice(&[4]), RocList<i64>);
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_reverse_empty_list_of_int() {
    assert_evals_to!(
        indoc!(
            r#"
                emptyList : List I64
                emptyList =
                    []

                List.reverse emptyList
            "#
        ),
        RocList::from_slice(&[]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_reverse_empty_list() {
    assert_evals_to!("List.reverse []", RocList::from_slice(&[]), RocList<i64>);
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn foobarbaz() {
    assert_evals_to!(
        indoc!(
            r#"
                firstList : List I64
                firstList =
                    []

                secondList : List I64
                secondList =
                    []

                List.concat firstList secondList
            "#
        ),
        RocList::from_slice(&[]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_concat_two_empty_lists() {
    assert_evals_to!("List.concat [] []", RocList::from_slice(&[]), RocList<i64>);
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_concat_two_empty_lists_of_int() {
    assert_evals_to!(
        indoc!(
            r#"
                firstList : List I64
                firstList =
                    []

                secondList : List I64
                secondList =
                    []

                List.concat firstList secondList
            "#
        ),
        RocList::from_slice(&[]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_concat_second_list_is_empty() {
    assert_evals_to!(
        "List.concat [ 12, 13 ] []",
        RocList::from_slice(&[12, 13]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.concat [ 34, 43 ] [ 64, 55, 66 ]",
        RocList::from_slice(&[34, 43, 64, 55, 66]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_concat_first_list_is_empty() {
    assert_evals_to!(
        "List.concat [] [ 23, 24 ]",
        RocList::from_slice(&[23, 24]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_concat_two_non_empty_lists() {
    assert_evals_to!(
        "List.concat [1, 2 ] [ 3, 4 ]",
        RocList::from_slice(&[1, 2, 3, 4]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_concat_two_bigger_non_empty_lists() {
    assert_evals_to!(
        "List.concat [ 1.1, 2.2 ] [ 3.3, 4.4, 5.5 ]",
        RocList::from_slice(&[1.1, 2.2, 3.3, 4.4, 5.5]),
        RocList<f64>
    );
}

fn assert_concat_worked(num_elems1: i64, num_elems2: i64) {
    let vec1: Vec<i64> = (0..num_elems1)
        .map(|i| 12345 % (i + num_elems1 + num_elems2 + 1))
        .collect();
    let vec2: Vec<i64> = (0..num_elems2)
        .map(|i| 54321 % (i + num_elems1 + num_elems2 + 1))
        .collect();
    let slice_str1 = format!("{:?}", vec1);
    let slice_str2 = format!("{:?}", vec2);
    let mut expected = vec1;

    expected.extend(vec2);

    assert_evals_to!(
        &format!("List.concat {} {}", slice_str1, slice_str2),
        RocList::from_slice(&expected),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_concat_empty_list() {
    assert_concat_worked(0, 0);
    assert_concat_worked(1, 0);
    assert_concat_worked(2, 0);
    assert_concat_worked(3, 0);
    assert_concat_worked(4, 0);
    assert_concat_worked(7, 0);
    assert_concat_worked(8, 0);
    assert_concat_worked(9, 0);
    assert_concat_worked(25, 0);
    assert_concat_worked(150, 0);
    assert_concat_worked(0, 1);
    assert_concat_worked(0, 2);
    assert_concat_worked(0, 3);
    assert_concat_worked(0, 4);
    assert_concat_worked(0, 7);
    assert_concat_worked(0, 8);
    assert_concat_worked(0, 9);
    assert_concat_worked(0, 25);
    assert_concat_worked(0, 150);
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_concat_nonempty_lists() {
    assert_concat_worked(1, 1);
    assert_concat_worked(1, 2);
    assert_concat_worked(1, 3);
    assert_concat_worked(2, 3);
    assert_concat_worked(2, 1);
    assert_concat_worked(2, 2);
    assert_concat_worked(3, 1);
    assert_concat_worked(3, 2);
    assert_concat_worked(2, 3);
    assert_concat_worked(3, 3);
    assert_concat_worked(4, 4);
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_concat_large() {
    with_larger_debug_stack(|| {
        // these values produce mono ASTs so large that
        // it can cause a stack overflow. This has been solved
        // for current code, but may become a problem again in the future.
        assert_concat_worked(150, 150);
        assert_concat_worked(129, 350);
        assert_concat_worked(350, 129);
    })
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn empty_list_len() {
    assert_evals_to!("List.len []", 0, usize);
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn basic_int_list_len() {
    assert_evals_to!("List.len [ 12, 9, 6, 3 ]", 4, usize);
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn loaded_int_list_len() {
    assert_evals_to!(
        indoc!(
            r#"
                nums = [ 2, 4, 6 ]

                List.len nums
            "#
        ),
        3,
        usize
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn fn_int_list_len() {
    assert_evals_to!(
        indoc!(
            r#"
                getLen = \list -> List.len list

                nums = [ 2, 4, 6, 8 ]

                getLen nums
            "#
        ),
        4,
        usize
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn int_list_is_empty() {
    assert_evals_to!("List.isEmpty [ 12, 9, 6, 3 ]", false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn empty_list_is_empty() {
    assert_evals_to!("List.isEmpty []", true, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn first_int_list() {
    assert_evals_to!(
        indoc!(
            r#"
                when List.first [ 12, 9, 6, 3 ] is
                    Ok val -> val
                    Err _ -> -1
            "#
        ),
        12,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
#[ignore]
fn first_wildcard_empty_list() {
    assert_evals_to!(
        indoc!(
            r#"
                when List.first [] is
                    Ok _ -> 5
                    Err _ -> -1
            "#
        ),
        -1,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn first_empty_list() {
    assert_evals_to!(
        indoc!(
            r#"
                when List.first [] is
                    Ok val -> val
                    Err _ -> -1
            "#
        ),
        -1,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn last_int_list() {
    assert_evals_to!(
        indoc!(
            r#"
                when List.last [ 12, 9, 6, 3 ] is
                    Ok val -> val
                    Err _ -> -1
            "#
        ),
        3,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
#[ignore]
fn last_wildcard_empty_list() {
    assert_evals_to!(
        indoc!(
            r#"
                when List.last [] is
                    Ok _ -> 5
                    Err _ -> -1
            "#
        ),
        -1,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn last_empty_list() {
    assert_evals_to!(
        indoc!(
            r#"
                when List.last [] is
                    Ok val -> val
                    Err _ -> -1
            "#
        ),
        -1,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn get_empty_list() {
    assert_evals_to!(
        indoc!(
            r#"
               when List.get [] 0 is
                    Ok val -> val
                    Err _ -> -1
            "#
        ),
        -1,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
#[ignore]
fn get_wildcard_empty_list() {
    assert_evals_to!(
        indoc!(
            r#"
               when List.get [] 0 is
                    Ok _ -> 5
                    Err _ -> -1
            "#
        ),
        -1,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn get_int_list_ok() {
    assert_evals_to!(
        indoc!(
            r#"
                when List.get [ 12, 9, 6 ] 1 is
                    Ok val -> val
                    Err _ -> -1
            "#
        ),
        9,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn get_int_list_oob() {
    assert_evals_to!(
        indoc!(
            r#"
                when List.get [ 12, 9, 6 ] 1000 is
                    Ok val -> val
                    Err _ -> -1
            "#
        ),
        -1,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn get_set_unique_int_list() {
    assert_evals_to!(
        indoc!(
            r#"
                when List.get (List.set [ 12, 9, 7, 3 ] 1 42) 1 is
                    Ok val -> val
                    Err _ -> -1
            "#
        ),
        42,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn set_unique_int_list() {
    assert_evals_to!(
        "List.set [ 12, 9, 7, 1, 5 ] 2 33",
        RocList::from_slice(&[12, 9, 33, 1, 5]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn set_unique_list_oob() {
    assert_evals_to!(
        "List.set [ 3, 17, 4.1 ] 1337 9.25",
        RocList::from_slice(&[3.0, 17.0, 4.1]),
        RocList<f64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn set_shared_int_list() {
    assert_evals_to!(
        indoc!(
            r#"
            wrapper = \shared ->
                # This should not mutate the original
                x =
                    when List.get (List.set shared 1 7.7) 1 is
                        Ok num -> num
                        Err _ -> 0

                y =
                    when List.get shared 1 is
                        Ok num -> num
                        Err _ -> 0

                { x, y }

            wrapper [ 2.1, 4.3 ]
            "#
        ),
        (7.7, 4.3),
        (f64, f64)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn set_shared_list_oob() {
    assert_evals_to!(
        indoc!(
            r#"
            shared = [ 2, 4 ]

            # This List.set is out of bounds, and should have no effect
            x =
                when List.get (List.set shared 422 0) 1 is
                    Ok num -> num
                    Err _ -> 0

            y =
                when List.get shared 1 is
                    Ok num -> num
                    Err _ -> 0

            { x, y }
            "#
        ),
        (4, 4),
        (i64, i64)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn get_unique_int_list() {
    assert_evals_to!(
        indoc!(
            r#"
                unique = [ 2, 4 ]

                when List.get unique 1 is
                    Ok num -> num
                    Err _ -> -1
            "#
        ),
        4,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn gen_wrap_len() {
    assert_evals_to!(
        indoc!(
            r#"
                wrapLen = \list ->
                    [ List.len list ]

                wrapLen [ 1, 7, 9 ]
            "#
        ),
        RocList::from_slice(&[3]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn gen_wrap_first() {
    assert_evals_to!(
        indoc!(
            r#"
                wrapFirst = \list ->
                    [ List.first list ]

                wrapFirst [ 1, 2 ]
            "#
        ),
        RocList::from_slice(&[1]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn gen_duplicate() {
    assert_evals_to!(
        indoc!(
            r#"
                # Duplicate the first element into the second index
                dupe = \list ->
                    when List.first list is
                        Ok elem ->
                            List.set list 1 elem

                        _ ->
                            []

                dupe [ 1, 2 ]
            "#
        ),
        RocList::from_slice(&[1, 1]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn gen_swap() {
    assert_evals_to!(
        indoc!(
            r#"
            app "quicksort" provides [ main ] to "./platform"


            swap : Nat, Nat, List a -> List a
            swap = \i, j, list ->
                when Pair (List.get list i) (List.get list j) is
                    Pair (Ok atI) (Ok atJ) ->
                        list
                            |> List.set i atJ
                            |> List.set j atI

                    _ ->
                        []

            main =
                swap 0 1 [ 1, 2 ]
            "#
        ),
        RocList::from_slice(&[2, 1]),
        RocList<i64>
    );
}

//    #[test]
#[cfg(any(feature = "gen-llvm"))]
//    fn gen_partition() {
//        assert_evals_to!(
//            indoc!(
//                r#"
//                    swap : I64, I64, List a -> List a
//                    swap = \i, j, list ->
//                        when Pair (List.get list i) (List.get list j) is
//                            Pair (Ok atI) (Ok atJ) ->
//                                list
//                                    |> List.set i atJ
//                                    |> List.set j atI
//
//                            _ ->
//                                []
//                    partition : I64, I64, List (Num a) -> [ Pair I64 (List (Num a)) ]
//                    partition = \low, high, initialList ->
//                        when List.get initialList high is
//                            Ok pivot ->
//                                when partitionHelp (low - 1) low initialList high pivot is
//                                    Pair newI newList ->
//                                        Pair (newI + 1) (swap (newI + 1) high newList)
//
//                            Err _ ->
//                                Pair (low - 1) initialList
//
//
//                    partitionHelp : I64, I64, List (Num a), I64, I64 -> [ Pair I64 (List (Num a)) ]
//                    partitionHelp = \i, j, list, high, pivot ->
//                        if j < high then
//                            when List.get list j is
//                                Ok value ->
//                                    if value <= pivot then
//                                        partitionHelp (i + 1) (j + 1) (swap (i + 1) j list) high pivot
//                                    else
//                                        partitionHelp i (j + 1) list high pivot
//
//                                Err _ ->
//                                    Pair i list
//                        else
//                            Pair i list
//
//                    # when partition 0 0 [ 1,2,3,4,5 ] is
//                    # Pair list _ -> list
//                    [ 1,3 ]
//                "#
//            ),
//            RocList::from_slice(&[2, 1]),
//            RocList<i64>
//        );
//    }

//    #[test]
#[cfg(any(feature = "gen-llvm"))]
//    fn gen_partition() {
//        assert_evals_to!(
//            indoc!(
//                r#"
//                    swap : I64, I64, List a -> List a
//                    swap = \i, j, list ->
//                        when Pair (List.get list i) (List.get list j) is
//                            Pair (Ok atI) (Ok atJ) ->
//                                list
//                                    |> List.set i atJ
//                                    |> List.set j atI
//
//                            _ ->
//                                []
//                    partition : I64, I64, List (Num a) -> [ Pair I64 (List (Num a)) ]
//                    partition = \low, high, initialList ->
//                        when List.get initialList high is
//                            Ok pivot ->
//                                when partitionHelp (low - 1) low initialList high pivot is
//                                    Pair newI newList ->
//                                        Pair (newI + 1) (swap (newI + 1) high newList)
//
//                            Err _ ->
//                                Pair (low - 1) initialList
//
//
//                    partitionHelp : I64, I64, List (Num a), I64, I64 -> [ Pair I64 (List (Num a)) ]
//
//                    # when partition 0 0 [ 1,2,3,4,5 ] is
//                    # Pair list _ -> list
//                    [ 1,3 ]
//                "#
//            ),
//            RocList::from_slice(&[2, 1]),
//            RocList<i64>
//        );
//    }
#[test]
#[cfg(any(feature = "gen-llvm"))]
fn gen_quicksort() {
    with_larger_debug_stack(|| {
        assert_evals_to!(
            indoc!(
                r#"
                quicksort : List (Num a) -> List (Num a)
                quicksort = \list ->
                    n = List.len list
                    quicksortHelp list 0 (n - 1)


                quicksortHelp : List (Num a), Nat, Nat -> List (Num a)
                quicksortHelp = \list, low, high ->
                    if low < high then
                        when partition low high list is
                            Pair partitionIndex partitioned ->
                                partitioned
                                    |> quicksortHelp low (partitionIndex - 1)
                                    |> quicksortHelp (partitionIndex + 1) high
                    else
                        list


                swap : Nat, Nat, List a -> List a
                swap = \i, j, list ->
                    when Pair (List.get list i) (List.get list j) is
                        Pair (Ok atI) (Ok atJ) ->
                            list
                                |> List.set i atJ
                                |> List.set j atI

                        _ ->
                            []

                partition : Nat, Nat, List (Num a) -> [ Pair Nat (List (Num a)) ]
                partition = \low, high, initialList ->
                    when List.get initialList high is
                        Ok pivot ->
                            when partitionHelp (low - 1) low initialList high pivot is
                                Pair newI newList ->
                                    Pair (newI + 1) (swap (newI + 1) high newList)

                        Err _ ->
                            Pair (low - 1) initialList


                partitionHelp : Nat, Nat, List (Num a), Nat, (Num a) -> [ Pair Nat (List (Num a)) ]
                partitionHelp = \i, j, list, high, pivot ->
                    if j < high then
                        when List.get list j is
                            Ok value ->
                                if value <= pivot then
                                    partitionHelp (i + 1) (j + 1) (swap (i + 1) j list) high pivot
                                else
                                    partitionHelp i (j + 1) list high pivot

                            Err _ ->
                                Pair i list
                    else
                        Pair i list

                quicksort [ 7, 4, 21, 19 ]
            "#
            ),
            RocList::from_slice(&[4, 7, 19, 21]),
            RocList<i64>
        );
    })
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn foobar2() {
    with_larger_debug_stack(|| {
        assert_evals_to!(
            indoc!(
                r#"
                   quicksort : List (Num a) -> List (Num a)
                   quicksort = \list ->
                       quicksortHelp list 0 (List.len list - 1)


                   quicksortHelp : List (Num a), Nat, Nat -> List (Num a)
                   quicksortHelp = \list, low, high ->
                       if low < high then
                           when partition low high list is
                               Pair partitionIndex partitioned ->
                                   partitioned
                                       |> quicksortHelp low (partitionIndex - 1)
                                       |> quicksortHelp (partitionIndex + 1) high
                       else
                           list


                   swap : Nat, Nat, List a -> List a
                   swap = \i, j, list ->
                       when Pair (List.get list i) (List.get list j) is
                           Pair (Ok atI) (Ok atJ) ->
                               list
                                   |> List.set i atJ
                                   |> List.set j atI

                           _ ->
                               []

                   partition : Nat, Nat, List (Num a) -> [ Pair Nat (List (Num a)) ]
                   partition = \low, high, initialList ->
                       when List.get initialList high is
                           Ok pivot ->
                               when partitionHelp (low - 1) low initialList high pivot is
                                   Pair newI newList ->
                                       Pair (newI + 1) (swap (newI + 1) high newList)

                           Err _ ->
                               Pair (low - 1) initialList


                   partitionHelp : Nat, Nat, List (Num a), Nat, Num a -> [ Pair Nat (List (Num a)) ]
                   partitionHelp = \i, j, list, high, pivot ->
                       # if j < high then
                       if False then
                           when List.get list j is
                               Ok value ->
                                   if value <= pivot then
                                       partitionHelp (i + 1) (j + 1) (swap (i + 1) j list) high pivot
                                   else
                                       partitionHelp i (j + 1) list high pivot

                               Err _ ->
                                   Pair i list
                       else
                           Pair i list



                   quicksort [ 7, 4, 21, 19 ]
               "#
            ),
            RocList::from_slice(&[19, 7, 4, 21]),
            RocList<i64>
        );
    })
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn foobar() {
    with_larger_debug_stack(|| {
        assert_evals_to!(
            indoc!(
                r#"
                   quicksort : List (Num a) -> List (Num a)
                   quicksort = \list ->
                       quicksortHelp list 0 (List.len list - 1)


                   quicksortHelp : List (Num a), Nat, Nat -> List (Num a)
                   quicksortHelp = \list, low, high ->
                       if low < high then
                           when partition low high list is
                               Pair partitionIndex partitioned ->
                                   partitioned
                                       |> quicksortHelp low (partitionIndex - 1)
                                       |> quicksortHelp (partitionIndex + 1) high
                       else
                           list


                   swap : Nat, Nat, List a -> List a
                   swap = \i, j, list ->
                       when Pair (List.get list i) (List.get list j) is
                           Pair (Ok atI) (Ok atJ) ->
                               list
                                   |> List.set i atJ
                                   |> List.set j atI

                           _ ->
                               []

                   partition : Nat, Nat, List (Num a) -> [ Pair Nat (List (Num a)) ]
                   partition = \low, high, initialList ->
                       when List.get initialList high is
                           Ok pivot ->
                               when partitionHelp (low - 1) low initialList high pivot is
                                   Pair newI newList ->
                                       Pair (newI + 1) (swap (newI + 1) high newList)

                           Err _ ->
                               Pair (low - 1) initialList


                   partitionHelp : Nat, Nat, List (Num a), Nat, Num a -> [ Pair Nat (List (Num a)) ]
                   partitionHelp = \i, j, list, high, pivot ->
                       if j < high then
                           when List.get list j is
                               Ok value ->
                                   if value <= pivot then
                                       partitionHelp (i + 1) (j + 1) (swap (i + 1) j list) high pivot
                                   else
                                       partitionHelp i (j + 1) list high pivot

                               Err _ ->
                                   Pair i list
                       else
                           Pair i list



                   when List.first (quicksort [0x1]) is
                       _ -> 4
               "#
            ),
            4,
            i64
        );
    })
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn empty_list_increment_decrement() {
    assert_evals_to!(
        indoc!(
            r#"
            x : List I64
            x = []

            List.len x + List.len x
            "#
        ),
        0,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_literal_increment_decrement() {
    assert_evals_to!(
        indoc!(
            r#"
            x : List I64
            x = [1,2,3]

            List.len x + List.len x
            "#
        ),
        6,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_pass_to_function() {
    assert_evals_to!(
        indoc!(
            r#"
            x : List I64
            x = [1,2,3]

            id : List I64 -> List I64
            id = \y -> y

            id x
            "#
        ),
        RocList::from_slice(&[1, 2, 3]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_pass_to_set() {
    assert_evals_to!(
        indoc!(
            r#"
            x : List I64
            x = [1,2,3]

            id : List I64 -> List I64
            id = \y -> List.set y 0 0

            id x
            "#
        ),
        RocList::from_slice(&[0, 2, 3]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_wrap_in_tag() {
    assert_evals_to!(
        indoc!(
            r#"
            id : List I64 -> [ Pair (List I64) I64 ]
            id = \y -> Pair y 4

            when id [1,2,3] is
                Pair v _ -> v
            "#
        ),
        RocList::from_slice(&[1, 2, 3]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_contains() {
    assert_evals_to!(indoc!("List.contains [1,2,3] 1"), true, bool);

    assert_evals_to!(indoc!("List.contains [1,2,3] 4"), false, bool);

    assert_evals_to!(indoc!("List.contains [] 4"), false, bool);
}
#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_min() {
    assert_evals_to!(
        indoc!(
            r#"
                    when List.min [] is
                        Ok val -> val
                        Err _ -> -1
                "#
        ),
        -1,
        i64
    );
    assert_evals_to!(
        indoc!(
            r#"
                    when List.min [3, 1, 2] is
                        Ok val -> val
                        Err _ -> -1
                "#
        ),
        1,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_max() {
    assert_evals_to!(
        indoc!(
            r#"
                    when List.max [] is
                        Ok val -> val
                        Err _ -> -1
                "#
        ),
        -1,
        i64
    );
    assert_evals_to!(
        indoc!(
            r#"
                    when List.max [3, 1, 2] is
                        Ok val -> val
                        Err _ -> -1
                "#
        ),
        3,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_sum() {
    assert_evals_to!("List.sum []", 0, i64);
    assert_evals_to!("List.sum [ 1, 2, 3 ]", 6, i64);
    assert_evals_to!("List.sum [ 1.1, 2.2, 3.3 ]", 6.6, f64);
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_product() {
    assert_evals_to!("List.product []", 1, i64);
    assert_evals_to!("List.product [ 1, 2, 3 ]", 6, i64);
    assert_evals_to!("List.product [ 1.1, 2.2, 3.3 ]", 1.1 * 2.2 * 3.3, f64);
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_keep_oks() {
    assert_evals_to!("List.keepOks [] (\\x -> x)", 0, i64);
    assert_evals_to!(
        "List.keepOks [1,2] (\\x -> Ok x)",
        RocList::from_slice(&[1, 2]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.keepOks [1,2] (\\x -> x % 2)",
        RocList::from_slice(&[1, 0]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.keepOks [Ok 1, Err 2] (\\x -> x)",
        RocList::from_slice(&[1]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_keep_errs() {
    assert_evals_to!("List.keepErrs [] (\\x -> x)", 0, i64);
    assert_evals_to!(
        "List.keepErrs [1,2] (\\x -> Err x)",
        RocList::from_slice(&[1, 2]),
        RocList<i64>
    );
    assert_evals_to!(
        indoc!(
            r#"
            List.keepErrs [0,1,2] (\x -> x % 0 |> Result.mapErr (\_ -> 32))
            "#
        ),
        RocList::from_slice(&[32, 32, 32]),
        RocList<i64>
    );

    assert_evals_to!(
        "List.keepErrs [Ok 1, Err 2] (\\x -> x)",
        RocList::from_slice(&[2]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_map_with_index() {
    assert_evals_to!(
        "List.mapWithIndex [0,0,0] (\\index, x -> Num.intCast index + x)",
        RocList::from_slice(&[0, 1, 2]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
#[should_panic(expected = r#"Roc failed with message: "integer addition overflowed!"#)]
fn cleanup_because_exception() {
    assert_evals_to!(
        indoc!(
            r#"
            x = [ 1,2 ]

            five : I64
            five = 5

            five + Num.maxInt + 3 + (Num.intCast (List.len x))
               "#
        ),
        9,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_range() {
    assert_evals_to!("List.range 0 -1", RocList::from_slice(&[]), RocList<i64>);
    assert_evals_to!("List.range 0 0", RocList::from_slice(&[0]), RocList<i64>);
    assert_evals_to!(
        "List.range 0 5",
        RocList::from_slice(&[0, 1, 2, 3, 4]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_sort_with() {
    assert_evals_to!(
        "List.sortWith [] Num.compare",
        RocList::from_slice(&[]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.sortWith [ 4,3,2,1 ] Num.compare",
        RocList::from_slice(&[1, 2, 3, 4]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.sortWith [ 1,2,3,4] (\\a,b -> Num.compare b a)",
        RocList::from_slice(&[4, 3, 2, 1]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_any() {
    assert_evals_to!("List.any [] (\\e -> e > 3)", false, bool);
    assert_evals_to!("List.any [ 1, 2, 3 ] (\\e -> e > 3)", false, bool);
    assert_evals_to!("List.any [ 1, 2, 4 ] (\\e -> e > 3)", true, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
#[should_panic(expected = r#"Roc failed with message: "UnresolvedTypeVar"#)]
fn list_any_empty_with_unknown_element_type() {
    // Segfaults with invalid memory reference. Running this as a stand-alone
    // Roc program, generates the following error message:
    //
    //     Application crashed with message
    //     UnresolvedTypeVar compiler/mono/src/ir.rs line 3775
    //     Shutting down
    assert_evals_to!("List.any [] (\\_ -> True)", false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
#[should_panic(expected = r#"Roc failed with message: "invalid ret_layout""#)]
fn lists_with_incompatible_type_param_in_if() {
    assert_evals_to!(
        indoc!(
            r#"
            list1 = [ {} ]

            list2 = [ "" ]

            x = if True then list1 else list2

            ""
            "#
        ),
        RocStr::default(),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn map_with_index_multi_record() {
    // see https://github.com/rtfeldman/roc/issues/1700
    assert_evals_to!(
        indoc!(
            r#"
            List.mapWithIndex [ { x: {}, y: {} } ] \_, _ -> {}
            "#
        ),
        RocList::from_slice(&[((), ())]),
        RocList<((), ())>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn empty_list_of_function_type() {
    // see https://github.com/rtfeldman/roc/issues/1732
    assert_evals_to!(
        indoc!(
            r#"
            myList : List (Str -> Str)
            myList = []

            myClosure : Str -> Str
            myClosure = \_ -> "bar"

            choose =
                if False then
                    myList
                else
                    [ myClosure ]

            when List.get choose 0 is
                Ok f -> f "foo"
                Err _ -> "bad!"
            "#
        ),
        RocStr::from_slice(b"bar"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_join_map() {
    assert_evals_to!(
        indoc!(
            r#"
            List.joinMap ["guava,apple,pear", "bailey,cyrus"] (\s -> Str.split s ",")
            "#
        ),
        RocList::from_slice(&[
            RocStr::from_slice("guava".as_bytes()),
            RocStr::from_slice("apple".as_bytes()),
            RocStr::from_slice("pear".as_bytes()),
            RocStr::from_slice("bailey".as_bytes()),
            RocStr::from_slice("cyrus".as_bytes()),
        ]),
        RocList<RocStr>
    )
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_join_map_empty() {
    assert_evals_to!(
        indoc!(
            r#"
            List.joinMap [] (\s -> Str.split s ",")
            "#
        ),
        RocList::from_slice(&[]),
        RocList<RocStr>
    )
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_find() {
    assert_evals_to!(
        indoc!(
            r#"
            when List.find ["a", "bc", "def"] (\s -> Str.countGraphemes s > 1) is
                Ok v -> v
                Err _ -> "not found"
            "#
        ),
        RocStr::from_slice(b"bc"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_find_not_found() {
    assert_evals_to!(
        indoc!(
            r#"
            when List.find ["a", "bc", "def"] (\s -> Str.countGraphemes s > 5) is
                Ok v -> v
                Err _ -> "not found"
            "#
        ),
        RocStr::from_slice(b"not found"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_find_empty_typed_list() {
    assert_evals_to!(
        indoc!(
            r#"
            when List.find [] (\s -> Str.countGraphemes s > 5) is
                Ok v -> v
                Err _ -> "not found"
            "#
        ),
        RocStr::from_slice(b"not found"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
#[ignore = "Fails because monomorphization can't be done if we don't have a concrete element type!"]
fn list_find_empty_layout() {
    assert_evals_to!(
        indoc!(
            r#"
            List.find [] (\_ -> True)
            "#
        ),
        0,
        i64
    );
}
