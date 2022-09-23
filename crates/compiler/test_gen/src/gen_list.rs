#[cfg(feature = "gen-llvm")]
use crate::helpers::llvm::assert_evals_to;

#[cfg(feature = "gen-dev")]
use crate::helpers::dev::assert_evals_to;

#[cfg(feature = "gen-wasm")]
use crate::helpers::wasm::assert_evals_to;

#[allow(unused_imports)]
use crate::helpers::with_larger_debug_stack;
//use crate::assert_wasm_evals_to as assert_evals_to;
#[allow(unused_imports)]
use indoc::indoc;
#[allow(unused_imports)]
use roc_std::{RocList, RocResult, RocStr};

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn roc_list_construction() {
    let list = RocList::from_slice(&[1i64; 23]);
    assert_eq!(&list, &list);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn empty_list_literal() {
    assert_evals_to!("[]", RocList::<i64>::from_slice(&[]), RocList<i64>);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_literal_empty_record() {
    assert_evals_to!("[{}]", RocList::from_slice(&[()]), RocList<()>);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn int_singleton_list_literal() {
    assert_evals_to!("[1, 2]", RocList::from_slice(&[1, 2]), RocList<i64>);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn int_list_literal() {
    assert_evals_to!("[12, 9]", RocList::from_slice(&[12, 9]), RocList<i64>);
    assert_evals_to!(
        "[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]",
        RocList::from_slice(&[1i64; 23]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn bool_list_literal() {
    assert_evals_to!(
        indoc!(
            r#"
               false = Bool.false

               [false]
               "#
        ),
        RocList::from_slice(&[false; 1]),
        RocList<bool>
    );

    assert_evals_to!(
        "[Bool.true, Bool.false, Bool.true]",
        RocList::from_slice(&[true, false, true]),
        RocList<bool>
    );

    assert_evals_to!(
        indoc!(
            r#"
               false : Bool
               false = Bool.false

               [false]
               "#
        ),
        RocList::from_slice(&[false; 1]),
        RocList<bool>
    );

    assert_evals_to!(
        indoc!(
            r#"
               true : Bool
               true = Bool.true

               List.repeat true 23
               "#
        ),
        RocList::from_slice(&[true; 23]),
        RocList<bool>
    );

    assert_evals_to!(
        indoc!(
            r#"
               true : Bool
               true = Bool.true

               List.repeat { x: true, y: true } 23
               "#
        ),
        RocList::from_slice(&[[true, true]; 23]),
        RocList<[bool; 2]>
    );

    assert_evals_to!(
        indoc!(
            r#"
               true : Bool
               true = Bool.true

               List.repeat { x: true, y: true, a: true, b: true, c: true, d : true, e: true, f: true } 23
               "#
        ),
        RocList::from_slice(&[[true, true, true, true, true, true, true, true]; 23]),
        RocList<[bool; 8]>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn variously_sized_list_literals() {
    assert_evals_to!("[]", RocList::<i64>::from_slice(&[]), RocList<i64>);
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_append_basic() {
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_take_first() {
    assert_evals_to!(
        "List.takeFirst [1, 2, 3] 2",
        RocList::from_slice(&[1, 2]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.takeFirst [1, 2, 3] 0",
        RocList::<i64>::from_slice(&[]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.takeFirst [] 1",
        RocList::<i64>::from_slice(&[]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.takeFirst [1,2] 5",
        RocList::from_slice(&[1, 2]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_take_last() {
    assert_evals_to!(
        "List.takeLast [1, 2, 3] 2",
        RocList::from_slice(&[2, 3]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.takeLast [1, 2, 3] 0",
        RocList::<i64>::from_slice(&[]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.takeLast [] 1",
        RocList::<i64>::from_slice(&[]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.takeLast [1,2] 5",
        RocList::from_slice(&[1, 2]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
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
        RocList::<i64>::from_slice(&[]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.sublist [] { start: 1 , len: 1 } ",
        RocList::<i64>::from_slice(&[]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.sublist [1, 2, 3] { start: 1 , len: 0 } ",
        RocList::<i64>::from_slice(&[]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.sublist [1, 2, 3] { start: 0 , len: 5 } ",
        RocList::from_slice(&[1, 2, 3]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_map_try_ok() {
    assert_evals_to!(
        // No transformation
        r#"
            List.mapTry [1, 2, 3] \elem -> Ok elem
        "#,
        // Result I64 [] is unwrapped to just I64
        RocList::<i64>::from_slice(&[1, 2, 3]),
        RocList<i64>
    );
    assert_evals_to!(
        // Transformation
        r#"
            List.mapTry [1, 2, 3] \num ->
                str = Num.toStr (num * 2)

                Ok "\(str)!"
        "#,
        // Result Str [] is unwrapped to just Str
        RocList::<RocStr>::from_slice(&[
            RocStr::from("2!"),
            RocStr::from("4!"),
            RocStr::from("6!"),
        ]),
        RocList<RocStr>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_map_try_err() {
    use core::convert::Infallible;

    assert_evals_to!(
        r#"
            List.mapTry [1, 2, 3] \_ -> Err -1
        "#,
        RocResult::err(-1),
        RocResult<RocList<Infallible>, i64>
    );

    assert_evals_to!(
        // If any element returns Err, the whole thing returns Err
        r#"
            List.mapTry [1, 2, 3] \num ->
                if num > 2 then
                    Err -1
                else
                    Ok num
        "#,
        RocResult::err(-1),
        RocResult<RocList<i64>, i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_split() {
    assert_evals_to!(
        r#"
               list = List.split [1, 2, 3] 0
               list.before
            "#,
        RocList::<i64>::from_slice(&[]),
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
        (
            RocList::from_slice(&[1, 2, 3]),
            RocList::<i64>::from_slice(&[]),
        ),
        (RocList<i64>, RocList<i64>,)
    );
    assert_evals_to!(
        "List.split [1, 2, 3] 4",
        (
            RocList::from_slice(&[1, 2, 3]),
            RocList::<i64>::from_slice(&[]),
        ),
        (RocList<i64>, RocList<i64>,)
    );
    assert_evals_to!(
        "List.split [] 1",
        (
            RocList::<i64>::from_slice(&[]),
            RocList::<i64>::from_slice(&[]),
        ),
        (RocList<i64>, RocList<i64>,)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_split_first() {
    assert_evals_to!(
        r#"
               List.splitFirst [2, 3, 0, 4, 0, 6, 0, 8, 9] 0
               |> Result.map .before
        "#,
        RocResult::ok(RocList::<i64>::from_slice(&[2, 3])),
        RocResult<RocList<i64>, ()>
    );
    assert_evals_to!(
        r#"
               List.splitFirst [2, 3, 0, 4, 0, 6, 0, 8, 9] 0
               |> Result.map .after
        "#,
        RocResult::ok(RocList::<i64>::from_slice(&[4, 0, 6, 0, 8, 9])),
        RocResult<RocList<i64>, ()>
    );

    assert_evals_to!(
        "List.splitFirst [1, 2, 3] 0",
        RocResult::err(()),
        RocResult<(RocList<i64>, RocList<i64>), ()>
    );

    assert_evals_to!(
        "List.splitFirst [] 1",
        RocResult::err(()),
        RocResult<(RocList<i64>, RocList<i64>), ()>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_split_last() {
    assert_evals_to!(
        r#"
               List.splitLast [2, 3, 0, 4, 0, 6, 0, 8, 9] 0
               |> Result.map .before
        "#,
        RocResult::ok(RocList::<i64>::from_slice(&[2, 3, 0, 4, 0, 6])),
        RocResult<RocList<i64>, ()>
    );
    assert_evals_to!(
        r#"
               List.splitLast [2, 3, 0, 4, 0, 6, 0, 8, 9] 0
               |> Result.map .after
        "#,
        RocResult::ok(RocList::<i64>::from_slice(&[8, 9])),
        RocResult<RocList<i64>, ()>
    );

    assert_evals_to!(
        "List.splitLast [1, 2, 3] 0",
        RocResult::err(()),
        RocResult<(RocList<i64>, RocList<i64>), ()>
    );

    assert_evals_to!(
        "List.splitLast [] 1",
        RocResult::err(()),
        RocResult<(RocList<i64>, RocList<i64>), ()>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_drop() {
    assert_evals_to!(
        "List.drop [1,2,3] 2",
        RocList::from_slice(&[3]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.drop [] 1",
        RocList::<i64>::from_slice(&[]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.drop [1,2] 5",
        RocList::<i64>::from_slice(&[]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
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
    assert_evals_to!(
        "List.dropAt [] 1",
        RocList::<i64>::from_slice(&[]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.dropAt [0] 0",
        RocList::<i64>::from_slice(&[]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_intersperse() {
    assert_evals_to!(
        indoc!(
            r#"
                    List.intersperse [0, 0, 0] 1
                "#
        ),
        RocList::from_slice(&[0, 1, 0, 1, 0]),
        RocList<i64>
    );
    assert_evals_to!(
        indoc!(
            r#"
                    List.intersperse [] 1
                "#
        ),
        RocList::<i64>::from_slice(&[]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_drop_at_shared() {
    assert_evals_to!(
        indoc!(
            r#"
               list : List I64
               list = [if Bool.true then 4 else 4, 5, 6]

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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_drop_if_empty_list_of_int() {
    assert_evals_to!(
        indoc!(
            r#"
            empty : List I64
            empty = []

            List.dropIf empty \_ -> Bool.true
            "#
        ),
        RocList::<i64>::from_slice(&[]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_drop_if_empty_list() {
    assert_evals_to!(
        indoc!(
            r#"
            alwaysTrue : I64 -> Bool
            alwaysTrue = \_ -> Bool.true

            List.dropIf [] alwaysTrue
            "#
        ),
        RocList::<i64>::from_slice(&[]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_drop_if_always_false_for_non_empty_list() {
    assert_evals_to!(
        indoc!(
            r#"
            List.dropIf [1,2,3,4,5,6,7,8] (\_ -> Bool.false)
            "#
        ),
        RocList::from_slice(&[1, 2, 3, 4, 5, 6, 7, 8]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_drop_if_always_true_for_non_empty_list() {
    assert_evals_to!(
        indoc!(
            r#"
            List.dropIf [1,2,3,4,5,6,7,8] (\_ -> Bool.true)
            "#
        ),
        RocList::<i64>::from_slice(&[]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_drop_if_geq3() {
    assert_evals_to!(
        indoc!(
            r#"
            List.dropIf [1,2,3,4,5,6,7,8] (\n -> n >= 3)
            "#
        ),
        RocList::from_slice(&[1, 2]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_drop_if_string_eq() {
    assert_evals_to!(
        indoc!(
            r#"
             List.dropIf ["x", "y", "x"] (\s -> s == "y")
             "#
        ),
        RocList::from_slice(&[RocStr::from("x"), RocStr::from("x")]),
        RocList<RocStr>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_drop_last() {
    assert_evals_to!(
        "List.dropLast [1, 2, 3]",
        RocList::from_slice(&[1, 2]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.dropLast []",
        RocList::<i64>::from_slice(&[]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.dropLast [0]",
        RocList::<i64>::from_slice(&[]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_drop_last_mutable() {
    assert_evals_to!(
        indoc!(
            r#"
               list : List I64
               list = [if Bool.true then 4 else 4, 5, 6]

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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_drop_first() {
    assert_evals_to!(
        "List.dropFirst [1, 2, 3]",
        RocList::from_slice(&[2, 3]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.dropFirst []",
        RocList::<i64>::from_slice(&[]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.dropFirst [0]",
        RocList::<i64>::from_slice(&[]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_swap() {
    assert_evals_to!(
        "List.swap [] 0 1",
        RocList::<i64>::from_slice(&[]),
        RocList<i64>
    );
    assert_evals_to!("List.swap [0] 1 2", RocList::from_slice(&[0]), RocList<i64>);
    assert_evals_to!(
        "List.swap [1, 2] 0 1",
        RocList::from_slice(&[2, 1]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.swap [1, 2] 1 0",
        RocList::from_slice(&[2, 1]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.swap [0, 1, 2, 3, 4, 5] 2 4",
        RocList::from_slice(&[0, 1, 4, 3, 2, 5]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.swap [0, 1, 2] 1 3",
        RocList::from_slice(&[0, 1, 2]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.swap [1, 2, 3] 1 1",
        RocList::from_slice(&[1, 2, 3]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_append_to_empty_list() {
    assert_evals_to!("List.append [] 3", RocList::from_slice(&[3]), RocList<i64>);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_append_bools() {
    assert_evals_to!(
        "List.append [Bool.true, Bool.false] Bool.true",
        RocList::from_slice(&[true, false, true]),
        RocList<bool>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_append_longer_list() {
    assert_evals_to!(
        "List.append [11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22] 23",
        RocList::from_slice(&[11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
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
        RocList::from_slice(&[RocStr::from("bar"), RocStr::from("foo"),]),
        RocList<RocStr>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_prepend_bools() {
    assert_evals_to!(
        "List.prepend [Bool.true, Bool.false] Bool.true",
        RocList::from_slice(&[true, true, false]),
        RocList<bool>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_prepend_big_list() {
    assert_evals_to!(
        "List.prepend [10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 100, 100, 100, 100] 9",
        RocList::from_slice(&[
            9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 100, 100, 100, 100
        ]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_walk_backwards_with_str() {
    assert_evals_to!(
        r#"List.walkBackwards ["x", "y", "z"] "<" Str.concat"#,
        RocStr::from("<zyx"),
        RocStr
    );

    assert_evals_to!(
        r#"List.walkBackwards ["Second", "Third", "Fourth"] "First" Str.concat"#,
        RocStr::from("FirstFourthThirdSecond"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_walk_backwards_with_record() {
    assert_evals_to!(
        indoc!(
            r#"
            Bit : [Zero, One]

            byte : List Bit
            byte = [Zero, One, Zero, One, Zero, Zero, One, Zero]

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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_walk_with_str() {
    assert_evals_to!(
        r#"List.walk ["x", "y", "z"] "<" Str.concat"#,
        RocStr::from("<xyz"),
        RocStr
    );

    assert_evals_to!(
        r#"List.walk ["Second", "Third", "Fourth"] "First" Str.concat"#,
        RocStr::from("FirstSecondThirdFourth"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_walk_subtraction() {
    assert_evals_to!(r#"List.walk [1, 2] 1 Num.sub"#, (1 - 1) - 2, i64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_walk_until_sum() {
    assert_evals_to!(
        r#"List.walkUntil [1, 2] 0 \a,b -> Continue (a + b)"#,
        3,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_walk_implements_position() {
    assert_evals_to!(
        r#"
        Option a : [Some a, None]

        find : List a, a -> Option Nat
        find = \list, needle ->
            findHelp list needle
                |> .v

        findHelp = \list, needle ->
            List.walkUntil list { n: 0, v: None } \{ n, v }, element ->
                if element == needle then
                    Break { n, v: Some n }
                else
                    Continue { n: n + 1, v }

        when find [1, 2, 3] 3 is
            None -> 0
            Some v -> v
        "#,
        2,
        usize
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_walk_until_even_prefix_sum() {
    assert_evals_to!(
        r#"
        helper = \a, b ->
            if Num.isEven b then
                Continue (a + b)

            else
                Break a

        List.walkUntil [2, 4, 8, 9] 0 helper"#,
        2 + 4 + 8,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_keep_if_empty_list_of_int() {
    assert_evals_to!(
        indoc!(
            r#"
            empty : List I64
            empty =
                []

            List.keepIf empty \_ -> Bool.true
            "#
        ),
        RocList::<i64>::from_slice(&[]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_keep_if_empty_list() {
    assert_evals_to!(
        indoc!(
            r#"
            alwaysTrue : I64 -> Bool
            alwaysTrue = \_ ->
                Bool.true


            List.keepIf [] alwaysTrue
            "#
        ),
        RocList::<i64>::from_slice(&[]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_keep_if_always_true_for_non_empty_list() {
    assert_evals_to!(
        indoc!(
            r#"
            alwaysTrue : I64 -> Bool
            alwaysTrue = \_ ->
                Bool.true

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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_keep_if_always_false_for_non_empty_list() {
    assert_evals_to!(
        indoc!(
            r#"
            alwaysFalse : I64 -> Bool
            alwaysFalse = \_ ->
                Bool.false

            List.keepIf [1,2,3,4,5,6,7,8] alwaysFalse
            "#
        ),
        RocList::<i64>::from_slice(&[]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_keep_if_str_is_hello() {
    assert_evals_to!(
        indoc!(
            r#"
             List.keepIf ["x", "y", "x"] (\x -> x == "x")
             "#
        ),
        RocList::from_slice(&[RocStr::from("x"), RocStr::from("x")]),
        RocList<RocStr>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
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
        RocList::<i64>::from_slice(&[]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_map_on_non_empty_list() {
    assert_evals_to!(
        indoc!(
            r#"
            nonEmpty : List I64
            nonEmpty =
                [1]

            List.map nonEmpty (\x -> x)
            "#
        ),
        RocList::from_slice(&[1]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_map_changes_input() {
    assert_evals_to!(
        indoc!(
            r#"
            nonEmpty : List I64
            nonEmpty =
                [1]

            List.map nonEmpty (\x -> x + 1)
            "#
        ),
        RocList::from_slice(&[2]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_map_on_big_list() {
    assert_evals_to!(
        indoc!(
            r#"
            nonEmpty : List I64
            nonEmpty =
                [1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5]

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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_map_with_type_change() {
    assert_evals_to!(
        indoc!(
            r#"
            nonEmpty : List I64
            nonEmpty =
                [1, 1, -4, 1, 2]


            List.map nonEmpty (\x -> x > 0)
            "#
        ),
        RocList::from_slice(&[true, true, false, true, true]),
        RocList<bool>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_map_using_defined_function() {
    assert_evals_to!(
        indoc!(
            r#"
             nonEmpty : List I64
             nonEmpty =
                 [2, 2, -4, 2, 3]

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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_map_all_inline() {
    assert_evals_to!(
        indoc!(
            r#"
            List.map [] (\x -> x > 0)
            "#
        ),
        RocList::<bool>::from_slice(&[]),
        RocList<bool>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_map_closure() {
    assert_evals_to!(
        indoc!(
            r#"
            float : F64
            float = 1.23

            single : List F64
            single =
                [0]

            List.map single (\x -> x + float)
            "#
        ),
        RocList::from_slice(&[1.23]),
        RocList<f64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_map4_group() {
    assert_evals_to!(
        indoc!(
            r#"
            List.map4 [1,2,3] [3,2,1] [2,1,3] [3,1,2] (\a, b, c, d -> Group a b c d)
            "#
        ),
        RocList::from_slice(&[[1, 3, 2, 3], [2, 2, 1, 1], [3, 1, 3, 2]]),
        RocList<[i64; 4]>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
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
        RocList::from_slice(&[RocStr::from("hola"),]),
        RocList<RocStr>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
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
        RocList::from_slice(&[RocStr::from("abc"),]),
        RocList<RocStr>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_map2_different_lengths() {
    assert_evals_to!(
        indoc!(
            r#"
            List.map2
                ["a", "b", "lllllllllllllooooooooongnggg"]
                ["b"]
                (\a, b -> Str.concat a b)
            "#
        ),
        RocList::from_slice(&[RocStr::from("ab"),]),
        RocList<RocStr>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_join_empty_list() {
    assert_evals_to!(
        "List.join []",
        RocList::<i64>::from_slice(&[]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_join_one_list() {
    assert_evals_to!(
        "List.join [[1, 2, 3]]",
        RocList::from_slice(&[1, 2, 3]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_join_two_non_empty_lists() {
    assert_evals_to!(
        "List.join [[1, 2, 3] , [4 ,5, 6]]",
        RocList::from_slice(&[1, 2, 3, 4, 5, 6]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_join_two_non_empty_lists_of_float() {
    assert_evals_to!(
        "List.join [[1.2, 1.1], [2.1, 2.2]]",
        RocList::from_slice(&[1.2, 1.1, 2.1, 2.2]),
        RocList<f64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_join_to_big_list() {
    assert_evals_to!(
        indoc!(
            r#"
                List.join
                    [
                        [1.2, 1.1],
                        [2.1, 2.2],
                        [3.0, 4.0, 5.0, 6.1, 9.0],
                        [3.0, 4.0, 5.0, 6.1, 9.0],
                        [3.0, 4.0, 5.0, 6.1, 9.0],
                        [3.0, 4.0, 5.0, 6.1, 9.0],
                        [3.0, 4.0, 5.0, 6.1, 9.0]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_join_defined_empty_list() {
    assert_evals_to!(
        indoc!(
            r#"
                empty : List F64
                empty =
                    []

                List.join [[0.2, 11.11], empty]
            "#
        ),
        RocList::from_slice(&[0.2, 11.11]),
        RocList<f64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_join_all_empty_lists() {
    assert_evals_to!(
        "List.join [[], [], []]",
        RocList::<f64>::from_slice(&[]),
        RocList<f64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_join_one_empty_list() {
    assert_evals_to!(
        "List.join [[1.2, 1.1], []]",
        RocList::from_slice(&[1.2, 1.1]),
        RocList<f64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_single() {
    assert_evals_to!("List.single 1", RocList::from_slice(&[1]), RocList<i64>);
    assert_evals_to!("List.single 5.6", RocList::from_slice(&[5.6]), RocList<f64>);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_repeat() {
    assert_evals_to!(
        "List.repeat 1 5",
        RocList::from_slice(&[1, 1, 1, 1, 1]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.repeat 2 4",
        RocList::from_slice(&[2, 2, 2, 2]),
        RocList<i64>
    );

    assert_evals_to!(
        "List.repeat [] 2",
        RocList::from_slice(&[RocList::<i64>::default(), RocList::default()]),
        RocList<RocList<i64>>
    );

    assert_evals_to!(
        indoc!(
            r#"
                noStrs : List Str
                noStrs =
                    []

                List.repeat noStrs 2
            "#
        ),
        RocList::from_slice(&[RocList::<i64>::default(), RocList::default()]),
        RocList<RocList<i64>>
    );

    assert_evals_to!(
        "List.repeat 4 15",
        RocList::from_slice(&[4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_reverse() {
    assert_evals_to!(
        "List.reverse [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]",
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
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
        RocList::<i64>::from_slice(&[]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_reverse_empty_list() {
    assert_evals_to!(
        "List.reverse []",
        RocList::<i64>::from_slice(&[]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_concat_two_empty_lists() {
    assert_evals_to!(
        "List.concat [] []",
        RocList::<i64>::from_slice(&[]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
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
        RocList::<i64>::from_slice(&[]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_concat_second_list_is_empty() {
    assert_evals_to!(
        "List.concat [12, 13] []",
        RocList::from_slice(&[12, 13]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_concat_first_list_is_empty() {
    assert_evals_to!(
        "List.concat [] [23, 24]",
        RocList::from_slice(&[23, 24]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_concat_two_non_empty_lists() {
    assert_evals_to!(
        "List.concat [1, 2] [3, 4]",
        RocList::from_slice(&[1, 2, 3, 4]),
        RocList<i64>
    );

    assert_evals_to!(
        "List.concat [34, 43] [64, 55, 66]",
        RocList::from_slice(&[34, 43, 64, 55, 66]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_concat_two_bigger_non_empty_lists() {
    assert_evals_to!(
        "List.concat [1.1, 2.2] [3.3, 4.4, 5.5]",
        RocList::from_slice(&[1.1, 2.2, 3.3, 4.4, 5.5]),
        RocList<f64>
    );
}

#[allow(dead_code)]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn empty_list_len() {
    assert_evals_to!("List.len []", 0, usize);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn basic_int_list_len() {
    assert_evals_to!("List.len [12, 9, 6, 3]", 4, usize);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn loaded_int_list_len() {
    assert_evals_to!(
        indoc!(
            r#"
                nums = [2, 4, 6]

                List.len nums
            "#
        ),
        3,
        usize
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn fn_int_list_len() {
    assert_evals_to!(
        indoc!(
            r#"
                getLen = \list -> List.len list

                nums = [2, 4, 6, 8]

                getLen nums
            "#
        ),
        4,
        usize
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn int_list_is_empty() {
    assert_evals_to!("List.isEmpty [12, 9, 6, 3]", false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn empty_list_is_empty() {
    assert_evals_to!("List.isEmpty []", true, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn first_int_list() {
    assert_evals_to!(
        indoc!(
            r#"
                when List.first [12, 9, 6, 3] is
                    Ok val -> val
                    Err _ -> -1
            "#
        ),
        12,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn last_int_list() {
    assert_evals_to!(
        indoc!(
            r#"
                when List.last [12, 9, 6, 3] is
                    Ok val -> val
                    Err _ -> -1
            "#
        ),
        3,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn get_int_list_ok() {
    assert_evals_to!(
        indoc!(
            r#"
                when List.get [12, 9, 6] 1 is
                    Ok val -> val
                    Err _ -> -1
            "#
        ),
        9,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn get_int_list_oob() {
    assert_evals_to!(
        indoc!(
            r#"
                when List.get [12, 9, 6] 1000 is
                    Ok val -> val
                    Err _ -> -1
            "#
        ),
        -1,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn replace_unique_int_list() {
    assert_evals_to!(
        indoc!(
            r#"
                record = List.replace [12, 9, 7, 1, 5] 2 33
                record.list
            "#
        ),
        RocList::from_slice(&[12, 9, 33, 1, 5]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn replace_unique_int_list_out_of_bounds() {
    assert_evals_to!(
        indoc!(
            r#"
                record = List.replace [12, 9, 7, 1, 5] 5 33
                record.value
            "#
        ),
        33,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn replace_unique_int_list_get_old_value() {
    assert_evals_to!(
        indoc!(
            r#"
                record = List.replace [12, 9, 7, 1, 5] 2 33
                record.value
            "#
        ),
        7,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn replace_unique_get_large_value() {
    assert_evals_to!(
        indoc!(
            r#"
                list : List { a : U64, b: U64, c: U64, d: U64 }
                list = [{ a: 1, b: 2, c: 3, d: 4 }, { a: 5, b: 6, c: 7, d: 8 }, { a: 9, b: 10, c: 11, d: 12 }]
                record = List.replace list 1 { a: 13, b: 14, c: 15, d: 16 }
                record.value
            "#
        ),
        [5, 6, 7, 8],
        [u64; 4]
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn replace_shared_int_list() {
    assert_evals_to!(
        indoc!(
            r#"
            wrapper = \shared ->
                # This should not mutate the original
                replaced = (List.replace shared 1 7.7).list
                x =
                    when List.get replaced 1 is
                        Ok num -> num
                        Err _ -> 0

                y =
                    when List.get shared 1 is
                        Ok num -> num
                        Err _ -> 0

                { x, y }

            wrapper [2.1, 4.3]
            "#
        ),
        (7.7, 4.3),
        (f64, f64)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn get_set_unique_int_list_i64() {
    assert_evals_to!(
        indoc!(
            r#"
                when List.get (List.set [12, 9, 7, 3] 1 42) 1 is
                    Ok val -> val
                    Err _ -> -1
            "#
        ),
        42,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn get_set_unique_int_list_i8() {
    assert_evals_to!(
        indoc!(
            r#"
                when List.get (List.set [12, 9, 7, 3] 1 42i8) 1 is
                    Ok val -> val
                    Err _ -> -1i8
            "#
        ),
        42,
        i8
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn set_unique_int_list() {
    assert_evals_to!(
        "List.set [12, 9, 7, 1, 5] 2 33",
        RocList::from_slice(&[12, 9, 33, 1, 5]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn set_unique_list_oob() {
    assert_evals_to!(
        "List.set [3, 17, 4.1] 1337 9.25",
        RocList::from_slice(&[3.0, 17.0, 4.1]),
        RocList<f64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
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

            wrapper [2.1, 4.3]
            "#
        ),
        (7.7, 4.3),
        (f64, f64)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn set_shared_list_oob() {
    assert_evals_to!(
        indoc!(
            r#"
            shared = [2, 4]

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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn get_unique_int_list() {
    assert_evals_to!(
        indoc!(
            r#"
                unique = [2, 4]

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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn gen_wrap_len() {
    assert_evals_to!(
        indoc!(
            r#"
                wrapLen = \list ->
                    [List.len list]

                wrapLen [1, 7, 9]
            "#
        ),
        RocList::from_slice(&[3]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn gen_wrap_first() {
    assert_evals_to!(
        indoc!(
            r#"
                wrapFirst = \list ->
                    [List.first list]

                wrapFirst [1, 2]
            "#
        ),
        RocList::from_slice(&[1]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
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

                dupe [1, 2]
            "#
        ),
        RocList::from_slice(&[1, 1]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn gen_swap() {
    assert_evals_to!(
        indoc!(
            r#"
            app "quicksort" provides [main] to "./platform"


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
                swap 0 1 [1, 2]
            "#
        ),
        RocList::from_slice(&[2, 1]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
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
                                    |> quicksortHelp low (Num.subSaturated partitionIndex 1)
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

                partition : Nat, Nat, List (Num a) -> [Pair Nat (List (Num a))]
                partition = \low, high, initialList ->
                    when List.get initialList high is
                        Ok pivot ->
                            when partitionHelp low low initialList high pivot is
                                Pair newI newList ->
                                    Pair newI (swap newI high newList)

                        Err _ ->
                            Pair low initialList


                partitionHelp : Nat, Nat, List (Num a), Nat, (Num a) -> [Pair Nat (List (Num a))]
                partitionHelp = \i, j, list, high, pivot ->
                    if j < high then
                        when List.get list j is
                            Ok value ->
                                if value <= pivot then
                                    partitionHelp (i + 1) (j + 1) (swap i j list) high pivot
                                else
                                    partitionHelp i (j + 1) list high pivot

                            Err _ ->
                                Pair i list
                    else
                        Pair i list

                quicksort [7, 4, 21, 19]
            "#
            ),
            RocList::from_slice(&[4, 7, 19, 21]),
            RocList<i64>
        );
    })
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn quicksort() {
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
                                       |> quicksortHelp low (Num.subSaturated partitionIndex 1)
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

                   partition : Nat, Nat, List (Num a) -> [Pair Nat (List (Num a))]
                   partition = \low, high, initialList ->
                       when List.get initialList high is
                           Ok pivot ->
                               when partitionHelp low low initialList high pivot is
                                   Pair newI newList ->
                                       Pair newI (swap newI high newList)

                           Err _ ->
                               Pair low initialList


                   partitionHelp : Nat, Nat, List (Num a), Nat, Num a -> [Pair Nat (List (Num a))]
                   partitionHelp = \i, j, list, high, pivot ->
                       # if j < high then
                       if Bool.false then
                           when List.get list j is
                               Ok value ->
                                   if value <= pivot then
                                       partitionHelp (i + 1) (j + 1) (swap i j list) high pivot
                                   else
                                       partitionHelp i (j + 1) list high pivot

                               Err _ ->
                                   Pair i list
                       else
                           Pair i list



                   quicksort [7, 4, 21, 19]
               "#
            ),
            RocList::from_slice(&[19, 7, 4, 21]),
            RocList<i64>
        );
    })
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn quicksort_singleton() {
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
                                       |> quicksortHelp low (Num.subSaturated partitionIndex 1)
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

                   partition : Nat, Nat, List (Num a) -> [Pair Nat (List (Num a))]
                   partition = \low, high, initialList ->
                       when List.get initialList high is
                           Ok pivot ->
                               when partitionHelp low low initialList high pivot is
                                   Pair newI newList ->
                                       Pair newI (swap newI high newList)

                           Err _ ->
                               Pair low initialList


                   partitionHelp : Nat, Nat, List (Num a), Nat, Num a -> [Pair Nat (List (Num a))]
                   partitionHelp = \i, j, list, high, pivot ->
                       if j < high then
                           when List.get list j is
                               Ok value ->
                                   if value <= pivot then
                                       partitionHelp (i + 1) (j + 1) (swap i j list) high pivot
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
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
        usize
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
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
        usize
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_wrap_in_tag() {
    assert_evals_to!(
        indoc!(
            r#"
            id : List I64 -> [Pair (List I64) I64]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_contains_int() {
    assert_evals_to!(indoc!("List.contains [1,2,3] 1"), true, bool);

    assert_evals_to!(indoc!("List.contains [1,2,3] 4"), false, bool);

    assert_evals_to!(indoc!("List.contains [] 4"), false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_contains_str() {
    assert_evals_to!(indoc!(r#"List.contains ["foo", "bar"] "bar""#), true, bool);

    assert_evals_to!(
        indoc!(r#"List.contains ["foo", "bar"] "spam""#),
        false,
        bool
    );

    assert_evals_to!(indoc!(r#"List.contains [] "spam""#), false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_manual_range() {
    assert_evals_to!(
        indoc!(
            r#"
            range : I64, I64, List I64-> List I64
            range = \low, high, accum ->
                if low < high then
                    range (low + 1) high (List.append accum low)
                else
                    accum

            range 0 5 [42]
            "#
        ),
        RocList::from_slice(&[42, 0, 1, 2, 3, 4]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_sum() {
    assert_evals_to!("List.sum []", 0, i64);
    assert_evals_to!("List.sum [1, 2, 3]", 6, i64);
    assert_evals_to!("List.sum [1.1, 2.2, 3.3]", 6.6, f64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_product() {
    assert_evals_to!("List.product []", 1, i64);
    assert_evals_to!("List.product [1, 2, 3]", 6, i64);
    assert_evals_to!("List.product [1.1, 2.2, 3.3]", 1.1 * 2.2 * 3.3, f64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_keep_void() {
    assert_evals_to!(
        "List.keepOks [] (\\x -> x)",
        RocList::from_slice(&[]),
        RocList<()>
    );

    assert_evals_to!(
        "List.keepErrs [] (\\x -> x)",
        RocList::from_slice(&[]),
        RocList<()>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_keep_oks() {
    assert_evals_to!(
        "List.keepOks [Ok {}, Ok {}] (\\x -> x)",
        RocList::from_slice(&[(), ()]),
        RocList<()>
    );
    assert_evals_to!(
        "List.keepOks [1,2] (\\x -> Ok x)",
        RocList::from_slice(&[1, 2]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.keepOks [1,2] (\\x -> Num.remChecked x 2)",
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_keep_errs() {
    assert_evals_to!(
        "List.keepErrs [Err {}, Err {}] (\\x -> x)",
        RocList::from_slice(&[(), ()]),
        RocList<()>
    );
    assert_evals_to!(
        "List.keepErrs [1,2] (\\x -> Err x)",
        RocList::from_slice(&[1, 2]),
        RocList<i64>
    );
    assert_evals_to!(
        indoc!(
            r#"
            List.keepErrs [0,1,2] (\x -> Num.remChecked x 0 |> Result.mapErr (\_ -> 32))
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_map_with_index() {
    assert_evals_to!(
        "List.mapWithIndex [0,0,0] (\\x, index -> Num.intCast index + x)",
        RocList::from_slice(&[0, 1, 2]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
#[should_panic(expected = r#"Roc failed with message: "integer addition overflowed!"#)]
fn cleanup_because_exception() {
    assert_evals_to!(
        indoc!(
            r#"
            x = [1,2]

            five : I64
            five = 5

            five + Num.maxI64 + 3 + (Num.intCast (List.len x))
               "#
        ),
        9,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_range() {
    assert_evals_to!(
        "List.range 0 -1",
        RocList::<i64>::from_slice(&[]),
        RocList<i64>
    );
    assert_evals_to!("List.range 0 0", RocList::from_slice(&[0]), RocList<i64>);
    assert_evals_to!(
        "List.range 0 5",
        RocList::from_slice(&[0, 1, 2, 3, 4]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_sort_with() {
    assert_evals_to!(
        "List.sortWith [] Num.compare",
        RocList::<i64>::from_slice(&[]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.sortWith [4,3,2,1] Num.compare",
        RocList::from_slice(&[1, 2, 3, 4]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.sortWith [1,2,3,4] (\\a,b -> Num.compare b a)",
        RocList::from_slice(&[4, 3, 2, 1]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_sort_asc() {
    assert_evals_to!(
        "List.sortAsc []",
        RocList::<i64>::from_slice(&[]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.sortAsc [4,3,2,1]",
        RocList::from_slice(&[1, 2, 3, 4]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_sort_desc() {
    assert_evals_to!(
        "List.sortDesc []",
        RocList::<i64>::from_slice(&[]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.sortDesc [1,2,3,4]",
        RocList::from_slice(&[4, 3, 2, 1]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_any() {
    assert_evals_to!("List.any [] (\\e -> e > 3)", false, bool);
    assert_evals_to!("List.any [1, 2, 3] (\\e -> e > 3)", false, bool);
    assert_evals_to!("List.any [1, 2, 4] (\\e -> e > 3)", true, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_any_empty_with_unknown_element_type() {
    assert_evals_to!("List.any [] (\\_ -> Bool.true)", false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_all() {
    assert_evals_to!("List.all [] (\\e -> e > 3)", true, bool);
    assert_evals_to!("List.all [1, 2, 3] (\\e -> e > 3)", false, bool);
    assert_evals_to!("List.all [1, 2, 4] (\\e -> e > 3)", false, bool);
    assert_evals_to!("List.all [1, 2, 3] (\\e -> e >= 1)", true, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_all_empty_with_unknown_element_type() {
    assert_evals_to!("List.all [] (\\_ -> Bool.true)", true, bool);
}

#[test]
// This doesn't work on Windows. If you make it return a `bool`, e.g. with `|> Str.isEmpty` at the end,
// then it works. We don't know what the problem is here!
#[cfg(all(
    not(target_family = "windows"),
    any(feature = "gen-llvm", feature = "gen-wasm")
))]
#[should_panic(expected = r#"Roc failed with message: "invalid ret_layout""#)]
fn lists_with_incompatible_type_param_in_if() {
    assert_evals_to!(
        indoc!(
            r#"
            list1 = [{}]

            list2 = [""]

            x = if Bool.true then list1 else list2

            ""
            "#
        ),
        RocStr::default(),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn map_with_index_multi_record() {
    // see https://github.com/roc-lang/roc/issues/1700
    assert_evals_to!(
        indoc!(
            r#"
            List.mapWithIndex [{ x: {}, y: {} }] \_, _ -> {}
            "#
        ),
        RocList::from_slice(&[((), ())]),
        RocList<((), ())>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn empty_list_of_function_type() {
    // see https://github.com/roc-lang/roc/issues/1732
    assert_evals_to!(
        indoc!(
            r#"
            myList : List (Str -> Str)
            myList = []

            myClosure : Str -> Str
            myClosure = \_ -> "bar"

            choose =
                if Bool.false then
                    myList
                else
                    [myClosure]

            when List.get choose 0 is
                Ok f -> f "foo"
                Err _ -> "bad!"
            "#
        ),
        RocStr::from("bar"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_join_map() {
    assert_evals_to!(
        indoc!(
            r#"
            List.joinMap ["guava,apple,pear", "bailey,cyrus"] (\s -> Str.split s ",")
            "#
        ),
        RocList::from_slice(&[
            RocStr::from("guava"),
            RocStr::from("apple"),
            RocStr::from("pear"),
            RocStr::from("bailey"),
            RocStr::from("cyrus"),
        ]),
        RocList<RocStr>
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_find() {
    assert_evals_to!(
        indoc!(
            r#"
            when List.findFirst ["a", "bc", "def", "g"] (\s -> Str.countGraphemes s > 1) is
                Ok v -> v
                Err _ -> "not found"
            "#
        ),
        RocStr::from("bc"),
        RocStr
    );

    assert_evals_to!(
        indoc!(
            r#"
            when List.findLast ["a", "bc", "def", "g"] (\s -> Str.countGraphemes s > 1) is
                Ok v -> v
                Err _ -> "not found"
            "#
        ),
        RocStr::from("def"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_find_not_found() {
    assert_evals_to!(
        indoc!(
            r#"
            when List.findFirst ["a", "bc", "def", "g"] (\s -> Str.countGraphemes s > 5) is
                Ok v -> v
                Err _ -> "not found"
            "#
        ),
        RocStr::from("not found"),
        RocStr
    );

    assert_evals_to!(
        indoc!(
            r#"
            when List.findLast ["a", "bc", "def", "g"] (\s -> Str.countGraphemes s > 5) is
                Ok v -> v
                Err _ -> "not found"
            "#
        ),
        RocStr::from("not found"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_find_empty_typed_list() {
    assert_evals_to!(
        indoc!(
            r#"
            when List.findFirst [] (\s -> Str.countGraphemes s > 5) is
                Ok v -> v
                Err _ -> "not found"
            "#
        ),
        RocStr::from("not found"),
        RocStr
    );

    assert_evals_to!(
        indoc!(
            r#"
            when List.findLast [] (\s -> Str.countGraphemes s > 5) is
                Ok v -> v
                Err _ -> "not found"
            "#
        ),
        RocStr::from("not found"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_find_empty_layout() {
    assert_evals_to!(
        indoc!(
            r#"
            List.findFirst [] \_ -> Bool.true
            "#
        ),
        // [Ok [], Err [NotFound]] gets unwrapped all the way to just [NotFound],
        // which is the unit!
        (),
        ()
    );

    assert_evals_to!(
        indoc!(
            r#"
            List.findLast [] \_ -> Bool.true
            "#
        ),
        // [Ok [], Err [NotFound]] gets unwrapped all the way to just [NotFound],
        // which is the unit!
        (),
        ()
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_find_index() {
    assert_evals_to!(
        indoc!(
            r#"
            when List.findFirstIndex ["a", "bc", "def", "g"] (\s -> Str.countGraphemes s > 1) is
                Ok v -> v
                Err _ -> 999
            "#
        ),
        1,
        usize
    );

    assert_evals_to!(
        indoc!(
            r#"
            when List.findLastIndex ["a", "bc", "def", "g"] (\s -> Str.countGraphemes s > 1) is
                Ok v -> v
                Err _ -> 999
            "#
        ),
        2,
        usize
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_find_index_not_found() {
    assert_evals_to!(
        indoc!(
            r#"
            when List.findFirstIndex ["a", "bc", "def", "g"] (\s -> Str.countGraphemes s > 5) is
                Ok v -> v
                Err _ -> 999
            "#
        ),
        999,
        usize
    );

    assert_evals_to!(
        indoc!(
            r#"
            when List.findLastIndex ["a", "bc", "def"] (\s -> Str.countGraphemes s > 5) is
                Ok v -> v
                Err _ -> 999
            "#
        ),
        999,
        usize
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_find_index_empty_typed_list() {
    assert_evals_to!(
        indoc!(
            r#"
            when List.findFirstIndex [] (\s -> Str.countGraphemes s > 5) is
                Ok v -> v
                Err _ -> 999
            "#
        ),
        999,
        usize
    );

    assert_evals_to!(
        indoc!(
            r#"
            when List.findLastIndex [] (\s -> Str.countGraphemes s > 5) is
                Ok v -> v
                Err _ -> 999
            "#
        ),
        999,
        usize
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_ends_with_empty() {
    assert_evals_to!(
        indoc!(
            r#"
            List.endsWith [] []
            "#
        ),
        true,
        bool
    );

    assert_evals_to!(
        indoc!(
            r#"
            List.endsWith ["a"] []
            "#
        ),
        true,
        bool
    );

    assert_evals_to!(
        indoc!(
            r#"
            List.endsWith [] ["a"]
            "#
        ),
        false,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_ends_with_nonempty() {
    assert_evals_to!(
        indoc!(
            r#"
            List.endsWith ["a", "bc", "def"] ["def"]
            "#
        ),
        true,
        bool
    );

    assert_evals_to!(
        indoc!(
            r#"
            List.endsWith ["a", "bc", "def"] ["bc", "def"]
            "#
        ),
        true,
        bool
    );

    assert_evals_to!(
        indoc!(
            r#"
            List.endsWith ["a", "bc", "def"] ["a"]
            "#
        ),
        false,
        bool
    );

    assert_evals_to!(
        indoc!(
            r#"
            List.endsWith ["a", "bc", "def"] [""]
            "#
        ),
        false,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_starts_with_empty() {
    assert_evals_to!(
        indoc!(
            r#"
            List.startsWith [] []
            "#
        ),
        true,
        bool
    );

    assert_evals_to!(
        indoc!(
            r#"
            List.startsWith ["a"] []
            "#
        ),
        true,
        bool
    );

    assert_evals_to!(
        indoc!(
            r#"
            List.startsWith [] ["a"]
            "#
        ),
        false,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_starts_with_nonempty() {
    assert_evals_to!(
        indoc!(
            r#"
            List.startsWith ["a", "bc", "def"] ["a"]
            "#
        ),
        true,
        bool
    );

    assert_evals_to!(
        indoc!(
            r#"
            List.startsWith ["a", "bc", "def"] ["a", "bc"]
            "#
        ),
        true,
        bool
    );

    assert_evals_to!(
        indoc!(
            r#"
            List.startsWith ["a", "bc", "def"] ["def"]
            "#
        ),
        false,
        bool
    );

    assert_evals_to!(
        indoc!(
            r#"
            List.startsWith ["a", "bc", "def"] [""]
            "#
        ),
        false,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn monomorphized_lists() {
    assert_evals_to!(
        indoc!(
            r#"
            l = [1, 2, 3]

            f : List U8, List U16 -> Nat
            f = \_, _ -> 18

            f l l
            "#
        ),
        18,
        usize
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn with_capacity() {
    // see https://github.com/roc-lang/roc/issues/1732
    assert_evals_to!(
        indoc!(
            r#"
            List.withCapacity 10
                |> List.append 0u64
                |> List.append 1u64
                |> List.append 2u64
                |> List.append 3u64
            "#
        ),
        RocList::from_slice(&[0, 1, 2, 3]),
        RocList<u64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn call_function_in_empty_list() {
    assert_evals_to!(
        indoc!(
            r#"
            lst : List ({} -> {})
            lst = []
            List.map lst \f -> f {}
            "#
        ),
        RocList::from_slice(&[]),
        RocList<()>
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn call_function_in_empty_list_unbound() {
    assert_evals_to!(
        indoc!(
            r#"
            lst = []
            List.map lst \f -> f {}
            "#
        ),
        RocList::from_slice(&[]),
        RocList<()>
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn issue_3571_lowlevel_call_function_with_bool_lambda_set() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            apply : List (a -> b), List a -> List b
            apply = \funs, vals ->
              initial = List.withCapacity ((List.len funs) * (List.len vals))
              List.walk funs initial \state, fun ->
                mappedVals = List.map vals fun
                List.concat state mappedVals

            add2 : Str -> Str
            add2 = \x -> "added \(x)"

            mul2 : Str -> Str
            mul2 = \x -> "multiplied \(x)"

            foo = [add2, mul2]
            bar = ["1", "2", "3", "4"]

            main = foo |> apply bar |> Str.joinWith ", "
            "#
        ),
        RocStr::from("added 1, added 2, added 3, added 4, multiplied 1, multiplied 2, multiplied 3, multiplied 4"),
        RocStr
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn issue_3530_uninitialized_capacity_in_list_literal() {
    assert_evals_to!(
        indoc!(
            r#"
            [11,22,33]
            "#
        ),
        3,
        (usize, usize, usize),
        |(_, _, cap)| cap
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_let_generalization() {
    assert_evals_to!(
        indoc!(
            r#"
            empty : List a
            empty = []

            xs : List Str
            xs = List.append empty "foo"

            List.len xs
            "#
        ),
        1,
        usize
    );
}
