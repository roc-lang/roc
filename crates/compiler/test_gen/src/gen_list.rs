#[cfg(feature = "gen-llvm")]
use crate::helpers::llvm::assert_evals_to;

#[cfg(feature = "gen-dev")]
use crate::helpers::dev::assert_evals_to;

#[cfg(feature = "gen-wasm")]
use crate::helpers::wasm::assert_evals_to;

use crate::helpers::with_larger_debug_stack;
//use crate::assert_wasm_evals_to as assert_evals_to;
#[allow(unused_imports)]
use indoc::indoc;
#[allow(unused_imports)]
use roc_std::{RocDec, RocList, RocResult, RocStr};

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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn bool_list_literal() {
    assert_evals_to!(
        indoc!(
            r"
               false = Bool.false

               [false]
               "
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
            r"
               false : Bool
               false = Bool.false

               [false]
               "
        ),
        RocList::from_slice(&[false; 1]),
        RocList<bool>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn dec_list_literal() {
    assert_evals_to!(
        "[1.0dec, 2.0dec]",
        RocList::from_slice(&[RocDec::from(1), RocDec::from(2)]),
        RocList<RocDec>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn dec_list_join() {
    assert_evals_to!(
        "List.concat [1.0dec, 2.0] [3.0, 4.0, 5.0]",
        RocList::from_slice(&[
            RocDec::from(1),
            RocDec::from(2),
            RocDec::from(3),
            RocDec::from(4),
            RocDec::from(5),
        ]),
        RocList<RocDec>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn bool_list_concat() {
    assert_evals_to!(
        indoc!(
            r"
               List.concat [Bool.true, Bool.false] [Bool.false, Bool.true]
               "
        ),
        RocList::from_slice(&[true, false, false, true]),
        RocList<bool>
    );

    assert_evals_to!(
        indoc!(
            r"
               List.concat [] [Bool.false, Bool.true]
               "
        ),
        RocList::from_slice(&[false, true]),
        RocList<bool>
    );

    assert_evals_to!(
        indoc!(
            r"
               List.concat [Bool.true, Bool.false] []
               "
        ),
        RocList::from_slice(&[true, false]),
        RocList<bool>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn bool_list_literal_repeat() {
    assert_evals_to!(
        indoc!(
            r"
               true : Bool
               true = Bool.true

               List.repeat true 23
               "
        ),
        RocList::from_slice(&[true; 23]),
        RocList<bool>
    );

    assert_evals_to!(
        indoc!(
            r"
               true : Bool
               true = Bool.true

               List.repeat { x: true, y: true } 23
               "
        ),
        RocList::from_slice(&[[true, true]; 23]),
        RocList<[bool; 2]>
    );

    assert_evals_to!(
        indoc!(
            r"
               true : Bool
               true = Bool.true

               List.repeat { x: true, y: true, a: true, b: true, c: true, d : true, e: true, f: true } 23
               "
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_take_first() {
    assert_evals_to!(
        "List.take_first [1, 2, 3] 2",
        RocList::from_slice(&[1, 2]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.take_first [1, 2, 3] 0",
        RocList::<i64>::from_slice(&[]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.take_first [] 1",
        RocList::<i64>::from_slice(&[]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.take_first [1,2] 5",
        RocList::from_slice(&[1, 2]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_take_last() {
    assert_evals_to!(
        "List.take_last [1, 2, 3] 2",
        RocList::from_slice(&[2, 3]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.take_last [1, 2, 3] 0",
        RocList::<i64>::from_slice(&[]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.take_last [] 1",
        RocList::<i64>::from_slice(&[]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.take_last [1,2] 5",
        RocList::from_slice(&[1, 2]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_map_try_ok() {
    assert_evals_to!(
        // No transformation
        r"
            List.map_try [1, 2, 3] \elem -> Ok elem
        ",
        // Result I64 [] is unwrapped to just I64
        RocList::<i64>::from_slice(&[1, 2, 3]),
        RocList<i64>
    );
    assert_evals_to!(
        // Transformation
        r#"
            List.map_try [1, 2, 3] \num ->
                str = Num.to_str (num * 2)

                Ok "${str}!"
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_map_try_err() {
    use core::convert::Infallible;

    assert_evals_to!(
        r"
            List.map_try [1, 2, 3] \_ -> Err -1
        ",
        RocResult::err(-1),
        RocResult<RocList<Infallible>, i64>
    );

    assert_evals_to!(
        // If any element returns Err, the whole thing returns Err
        r"
            List.map_try [1, 2, 3] \num ->
                if num > 2 then
                    Err -1
                else
                    Ok num
        ",
        RocResult::err(-1),
        RocResult<RocList<i64>, i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_split_at() {
    assert_evals_to!(
        r"
        list = List.split_at [1, 2, 3] 0
        list.before
        ",
        RocList::<i64>::from_slice(&[]),
        RocList<i64>
    );
    assert_evals_to!(
        r"
        list = List.split_at [1, 2, 3] 0
        list.others
        ",
        RocList::from_slice(&[1, 2, 3]),
        RocList<i64>
    );
    assert_evals_to!(
        r"
        List.split_at [1, 2, 3] 1
        ",
        (RocList::from_slice(&[1]), RocList::from_slice(&[2, 3])),
        (RocList<i64>, RocList<i64>,)
    );
    assert_evals_to!(
        "List.split_at [1, 2, 3] 3",
        (
            RocList::from_slice(&[1, 2, 3]),
            RocList::<i64>::from_slice(&[]),
        ),
        (RocList<i64>, RocList<i64>,)
    );
    assert_evals_to!(
        "List.split_at [1, 2, 3] 4",
        (
            RocList::from_slice(&[1, 2, 3]),
            RocList::<i64>::from_slice(&[]),
        ),
        (RocList<i64>, RocList<i64>,)
    );
    assert_evals_to!(
        "List.split_at [] 1",
        (
            RocList::<i64>::from_slice(&[]),
            RocList::<i64>::from_slice(&[]),
        ),
        (RocList<i64>, RocList<i64>,)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_split_on() {
    assert_evals_to!(
        r"
        List.split_on [] 1
        ",
        RocList::<RocList<i64>>::from_slice(&[RocList::<i64>::from_slice(&[])]),
        RocList<RocList<i64>>
    );
    assert_evals_to!(
        r"
        List.split_on [1] 1
        ",
        RocList::<RocList<i64>>::from_slice(&[
            RocList::<i64>::from_slice(&[]),
            RocList::<i64>::from_slice(&[]),
        ]),
        RocList<RocList<i64>>
    );
    assert_evals_to!(
        r"
        List.split_on [1, 2, 3] 47
        ",
        RocList::<RocList<i64>>::from_slice(&[RocList::<i64>::from_slice(&[1, 2, 3])]),
        RocList<RocList<i64>>
    );
    assert_evals_to!(
        r"
        List.split_on [1, 2, 3, 4, 5] 3
        ",
        RocList::<RocList<i64>>::from_slice(&[
            RocList::<i64>::from_slice(&[1, 2]),
            RocList::<i64>::from_slice(&[4, 5]),
        ]),
        RocList<RocList<i64>>
    );
    assert_evals_to!(
        r"
        List.split_on [1, 0, 1, 0, 1] 1
        ",
        RocList::<RocList<i64>>::from_slice(&[
            RocList::<i64>::from_slice(&[]),
            RocList::<i64>::from_slice(&[0]),
            RocList::<i64>::from_slice(&[0]),
            RocList::<i64>::from_slice(&[]),
        ]),
        RocList<RocList<i64>>
    );
    assert_evals_to!(
        r"
        List.split_on [1, 0, 1, 0, 1] 0
        ",
        RocList::<RocList<i64>>::from_slice(&[
            RocList::<i64>::from_slice(&[1]),
            RocList::<i64>::from_slice(&[1]),
            RocList::<i64>::from_slice(&[1]),
        ]),
        RocList<RocList<i64>>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_split_on_list() {
    assert_evals_to!(
        r"
        List.split_on_list [] []
        ",
        RocList::<RocList<i64>>::from_slice(&[RocList::<i64>::from_slice(&[])]),
        RocList<RocList<i64>>
    );
    assert_evals_to!(
        r"
        List.split_on_list [] [1, 2, 3]
        ",
        RocList::<RocList<i64>>::from_slice(&[RocList::<i64>::from_slice(&[]),]),
        RocList<RocList<i64>>
    );
    assert_evals_to!(
        r"
        List.split_on_list [1, 2, 3] []
        ",
        RocList::<RocList<i64>>::from_slice(&[RocList::<i64>::from_slice(&[1, 2, 3]),]),
        RocList<RocList<i64>>
    );
    assert_evals_to!(
        r"
        List.split_on_list [1] [1]
        ",
        RocList::<RocList<i64>>::from_slice(&[
            RocList::<i64>::from_slice(&[]),
            RocList::<i64>::from_slice(&[]),
        ]),
        RocList<RocList<i64>>
    );
    assert_evals_to!(
        r"
        List.split_on_list [1, 2, 3] [47]
        ",
        RocList::<RocList<i64>>::from_slice(&[RocList::<i64>::from_slice(&[1, 2, 3])]),
        RocList<RocList<i64>>
    );
    assert_evals_to!(
        r"
        List.split_on_list [1, 2, 3, 4, 5] [2, 3]
        ",
        RocList::<RocList<i64>>::from_slice(&[
            RocList::<i64>::from_slice(&[1]),
            RocList::<i64>::from_slice(&[4, 5]),
        ]),
        RocList<RocList<i64>>
    );
    assert_evals_to!(
        r"
        List.split_on_list [1, 0, 1, 0, 1] [1]
        ",
        RocList::<RocList<i64>>::from_slice(&[
            RocList::<i64>::from_slice(&[]),
            RocList::<i64>::from_slice(&[0]),
            RocList::<i64>::from_slice(&[0]),
            RocList::<i64>::from_slice(&[]),
        ]),
        RocList<RocList<i64>>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_split_first() {
    assert_evals_to!(
        r"
               List.split_first [2, 3, 0, 4, 0, 6, 0, 8, 9] 0
               |> Result.map_ok .before
        ",
        RocResult::ok(RocList::<i64>::from_slice(&[2, 3])),
        RocResult<RocList<i64>, ()>
    );
    assert_evals_to!(
        r"
               List.split_first [2, 3, 0, 4, 0, 6, 0, 8, 9] 0
               |> Result.map_ok .after
        ",
        RocResult::ok(RocList::<i64>::from_slice(&[4, 0, 6, 0, 8, 9])),
        RocResult<RocList<i64>, ()>
    );

    assert_evals_to!(
        "List.split_first [1, 2, 3] 0",
        RocResult::err(()),
        RocResult<(RocList<i64>, RocList<i64>), ()>
    );

    assert_evals_to!(
        "List.split_first [] 1",
        RocResult::err(()),
        RocResult<(RocList<i64>, RocList<i64>), ()>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_split_last() {
    assert_evals_to!(
        r"
               List.split_last [2, 3, 0, 4, 0, 6, 0, 8, 9] 0
               |> Result.map_ok .before
        ",
        RocResult::ok(RocList::<i64>::from_slice(&[2, 3, 0, 4, 0, 6])),
        RocResult<RocList<i64>, ()>
    );
    assert_evals_to!(
        r"
               List.split_last [2, 3, 0, 4, 0, 6, 0, 8, 9] 0
               |> Result.map_ok .after
        ",
        RocResult::ok(RocList::<i64>::from_slice(&[8, 9])),
        RocResult<RocList<i64>, ()>
    );

    assert_evals_to!(
        "List.split_last [1, 2, 3] 0",
        RocResult::err(()),
        RocResult<(RocList<i64>, RocList<i64>), ()>
    );

    assert_evals_to!(
        "List.split_last [] 1",
        RocResult::err(()),
        RocResult<(RocList<i64>, RocList<i64>), ()>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_chunks_of() {
    assert_evals_to!(
        "List.chunks_of [1, 2, 3, 4, 5, 6, 7, 8] 3",
        RocList::<RocList<i64>>::from_slice(&[
            RocList::from_slice(&[1, 2, 3]),
            RocList::from_slice(&[4, 5, 6]),
            RocList::from_slice(&[7, 8]),
        ]),
        RocList<RocList<i64>>
    );

    assert_evals_to!(
        "List.chunks_of [1, 2, 3, 4] 5",
        RocList::<RocList<i64>>::from_slice(&[RocList::from_slice(&[1, 2, 3, 4]),]),
        RocList<RocList<i64>>
    );

    assert_evals_to!(
        "List.chunks_of [1, 2, 3] 0",
        RocList::<RocList<i64>>::from_slice(&[]),
        RocList<RocList<i64>>
    );

    assert_evals_to!(
        "List.chunks_of [] 5",
        RocList::<RocList<i64>>::from_slice(&[]),
        RocList<RocList<i64>>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_drop_first() {
    assert_evals_to!(
        "List.drop_first [1,2,3] 2",
        RocList::from_slice(&[3]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.drop_first [] 1",
        RocList::<i64>::from_slice(&[]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.drop_first [1,2] 5",
        RocList::<i64>::from_slice(&[]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_drop_at() {
    assert_evals_to!(
        "List.drop_at [1, 2, 3] 0",
        RocList::from_slice(&[2, 3]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.drop_at [1, 2, 3] 1",
        RocList::from_slice(&[1, 3]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.drop_at [0, 0, 0] 3",
        RocList::from_slice(&[0, 0, 0]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.drop_at [] 1",
        RocList::<i64>::from_slice(&[]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.drop_at [0] 0",
        RocList::<i64>::from_slice(&[]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_intersperse() {
    assert_evals_to!(
        indoc!(
            r"
                    List.intersperse [0, 0, 0] 1
                "
        ),
        RocList::from_slice(&[0, 1, 0, 1, 0]),
        RocList<i64>
    );
    assert_evals_to!(
        indoc!(
            r"
                    List.intersperse [] 1
                "
        ),
        RocList::<i64>::from_slice(&[]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_drop_at_shared() {
    assert_evals_to!(
        indoc!(
            r"
               list : List I64
               list = [if Bool.true then 4 else 4, 5, 6]

               { new_list: List.drop_at list 0, original: list }
               "
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_drop_if_empty_list_of_int() {
    assert_evals_to!(
        indoc!(
            r"
            empty : List I64
            empty = []

            List.drop_if empty \_ -> Bool.true
            "
        ),
        RocList::<i64>::from_slice(&[]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_drop_if_empty_list() {
    assert_evals_to!(
        indoc!(
            r"
            always_true : I64 -> Bool
            always_true = \_ -> Bool.true

            List.drop_if [] always_true
            "
        ),
        RocList::<i64>::from_slice(&[]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_drop_if_always_false_for_non_empty_list() {
    assert_evals_to!(
        indoc!(
            r"
            List.drop_if [1,2,3,4,5,6,7,8] (\_ -> Bool.false)
            "
        ),
        RocList::from_slice(&[1, 2, 3, 4, 5, 6, 7, 8]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_drop_if_always_true_for_non_empty_list() {
    assert_evals_to!(
        indoc!(
            r"
            List.drop_if [1,2,3,4,5,6,7,8] (\_ -> Bool.true)
            "
        ),
        RocList::<i64>::from_slice(&[]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_drop_if_geq3() {
    assert_evals_to!(
        indoc!(
            r"
            List.drop_if [1,2,3,4,5,6,7,8] (\n -> n >= 3)
            "
        ),
        RocList::from_slice(&[1, 2]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_drop_if_string_eq() {
    assert_evals_to!(
        indoc!(
            r#"
             List.drop_if ["x", "y", "x"] (\s -> s == "y")
             "#
        ),
        RocList::from_slice(&[RocStr::from("x"), RocStr::from("x")]),
        RocList<RocStr>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_drop_last() {
    assert_evals_to!(
        "List.drop_last [1, 2, 3] 1",
        RocList::from_slice(&[1, 2]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.drop_last [] 5",
        RocList::<i64>::from_slice(&[]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.drop_last [0] 0",
        RocList::<i64>::from_slice(&[0]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_drop_last_mutable() {
    assert_evals_to!(
        indoc!(
            r"
               list : List I64
               list = [if Bool.true then 4 else 4, 5, 6]

               { new_list: List.drop_last list 1, original: list }
               "
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_append_to_empty_list() {
    assert_evals_to!("List.append [] 3", RocList::from_slice(&[3]), RocList<i64>);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_append_to_empty_list_of_int() {
    assert_evals_to!(
        indoc!(
            r"
                init_threes : List I64
                init_threes =
                    []

                List.append (List.append init_threes 3) 3
            "
        ),
        RocList::from_slice(&[3, 3]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_append_bools() {
    assert_evals_to!(
        "List.append [Bool.true, Bool.false] Bool.true",
        RocList::from_slice(&[true, false, true]),
        RocList<bool>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_append_longer_list() {
    assert_evals_to!(
        "List.append [11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22] 23",
        RocList::from_slice(&[11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_append_record() {
    assert_evals_to!(
        indoc!(
            r#"
            [
                { name: "foo", content: "cfoo" },
                { name: "bar", content: "cbar" },
                { name: "baz", content: "cbaz" },
            ]
            |> List.append { name: "spam", content: "cspam" }
            "#
        ),
        RocList::from_slice(&[
            (RocStr::from("cfoo"), RocStr::from("foo"),),
            (RocStr::from("cbar"), RocStr::from("bar"),),
            (RocStr::from("cbaz"), RocStr::from("baz"),),
            (RocStr::from("cspam"), RocStr::from("spam"),),
        ]),
        RocList<(RocStr, RocStr)>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_prepend() {
    assert_evals_to!("List.prepend [] 1", RocList::from_slice(&[1]), RocList<i64>);
    assert_evals_to!(
        "List.prepend [2] 1",
        RocList::from_slice(&[1, 2]),
        RocList<i64>
    );

    assert_evals_to!(
        indoc!(
            r"
                init : List I64
                init =
                    []

                List.prepend (List.prepend init 4) 6
            "
        ),
        RocList::from_slice(&[6, 4]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_prepend_str() {
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_prepend_bools() {
    assert_evals_to!(
        "List.prepend [Bool.true, Bool.false] Bool.true",
        RocList::from_slice(&[true, true, false]),
        RocList<bool>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_prepend_record() {
    assert_evals_to!(
        indoc!(
            r"
            payment1 : { amount: Dec, date: [RD I32] }
            payment1 = { amount: 1dec, date: (RD 1000) }
            payment2 : { amount: Dec, date: [RD I32] }
            payment2 = { amount: 2dec, date: (RD 1001) }

            List.prepend [payment2] payment1
            "
        ),
        RocList::from_slice(&[(RocDec::from(1), 1000i32), (RocDec::from(2), 1001i32),]),
        RocList<(RocDec, i32)>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_walk_backwards_empty_all_inline() {
    assert_evals_to!(
        indoc!(
            r"
            List.walk_backwards [0x1] 0 \state, elem -> state + elem
            "
        ),
        1,
        i64
    );

    assert_evals_to!(
        indoc!(
            r"
            empty : List I64
            empty =
                []

            List.walk_backwards empty 0 \state, elem -> state + elem
            "
        ),
        0,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_walk_backwards_with_str() {
    assert_evals_to!(
        r#"List.walk_backwards ["x", "y", "z"] "<" Str.concat"#,
        RocStr::from("<zyx"),
        RocStr
    );

    assert_evals_to!(
        r#"List.walk_backwards ["Second", "Third", "Fourth"] "First" Str.concat"#,
        RocStr::from("FirstFourthThirdSecond"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_walk_backwards_with_record() {
    assert_evals_to!(
        indoc!(
            r"
            Bit : [Zero, One]

            byte : List Bit
            byte = [Zero, One, Zero, One, Zero, Zero, One, Zero]

            initial_counts = { zeroes: 0, ones: 0 }

            acc = \r, b ->
                when b is
                    Zero -> { r & zeroes: r.zeroes + 1 }
                    One -> { r & ones: r.ones + 1 }

            final_counts = List.walk_backwards byte initial_counts acc

            final_counts.ones * 10 + final_counts.zeroes
            "
        ),
        35,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_walk_subtraction() {
    assert_evals_to!(r"List.walk [1, 2] 1 Num.sub", (1 - 1) - 2, i64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_walk_until_sum() {
    assert_evals_to!(r"List.walk_until [1, 2] 0 \a,b -> Continue (a + b)", 3, i64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_walk_with_index_until_sum() {
    assert_evals_to!(
        r"
        List.walk_with_index_until [5, 7, 2, 3] 0 (\state, elem, index ->
            if elem % 2 == 0 then
                Break state
            else
                # Convert to I64 to sidestep weird bug with WASM codegen
                a = Num.to_i64 elem
                b = Num.to_i64 index
                c = Num.to_i64 state
                Continue (a + b + c)
        )
        ",
        13,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_walk_implements_position() {
    assert_evals_to!(
        r"
        Option a : [Some a, None]

        find : List a, a -> Option U64 where a implements Eq
        find = \list, needle ->
            find_help list needle
                |> .v

        find_help = \list, needle ->
            List.walk_until list { n: 0, v: None } \{ n, v }, element ->
                if element == needle then
                    Break { n, v: Some n }
                else
                    Continue { n: n + 1, v }

        when find [1, 2, 3] 3 is
            None -> 0
            Some v -> v
        ",
        2,
        u64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_walk_until_even_prefix_sum() {
    assert_evals_to!(
        r"
        helper = \a, b ->
            if Num.is_even b then
                Continue (a + b)

            else
                Break a

        List.walk_until [2, 4, 8, 9] 0 helper",
        2 + 4 + 8,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_walk_from_sum() {
    assert_evals_to!(r"List.walk_from [1, 2, 3] 1 0 Num.add", 5, i64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_keep_if_empty_list_of_int() {
    assert_evals_to!(
        indoc!(
            r"
            empty : List I64
            empty =
                []

            List.keep_if empty \_ -> Bool.true
            "
        ),
        RocList::<i64>::from_slice(&[]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_keep_if_empty_list() {
    assert_evals_to!(
        indoc!(
            r"
            always_true : I64 -> Bool
            always_true = \_ ->
                Bool.true


            List.keep_if [] always_true
            "
        ),
        RocList::<i64>::from_slice(&[]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_keep_if_always_true_for_non_empty_list() {
    assert_evals_to!(
        indoc!(
            r"
            always_true : I64 -> Bool
            always_true = \_ ->
                Bool.true

            one_through_eight : List I64
            one_through_eight =
                [1,2,3,4,5,6,7,8]

            List.keep_if one_through_eight always_true
            "
        ),
        RocList::from_slice(&[1, 2, 3, 4, 5, 6, 7, 8]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_keep_if_always_false_for_non_empty_list() {
    assert_evals_to!(
        indoc!(
            r"
            always_false : I64 -> Bool
            always_false = \_ ->
                Bool.false

            List.keep_if [1,2,3,4,5,6,7,8] always_false
            "
        ),
        RocList::<i64>::from_slice(&[]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_keep_if_one() {
    assert_evals_to!(
        indoc!(
            r"
            int_is_less_than_three : I64 -> Bool
            int_is_less_than_three = \i ->
                i < 3

            List.keep_if [1,2,3,4,5,6,7,8] int_is_less_than_three
            "
        ),
        RocList::from_slice(&[1, 2]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_keep_if_str_is_hello() {
    assert_evals_to!(
        indoc!(
            r#"
             List.keep_if ["x", "y", "x"] (\x -> x == "x")
             "#
        ),
        RocList::from_slice(&[RocStr::from("x"), RocStr::from("x")]),
        RocList<RocStr>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_count_if_empty_list() {
    assert_evals_to!(
        indoc!(
            r"
            List.count_if [] \_ -> Bool.true
            "
        ),
        0,
        u64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_count_if_always_true_for_non_empty_list() {
    assert_evals_to!(
        indoc!(
            r"
            always_true : I64 -> Bool
            always_true = \_ ->
                Bool.true

            one_through_eight : List I64
            one_through_eight =
                [1,2,3,4,5,6,7,8]

            List.count_if one_through_eight always_true
            "
        ),
        8,
        u64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_count_if_always_false_for_non_empty_list() {
    assert_evals_to!(
        indoc!(
            r"
            always_false : I64 -> Bool
            always_false = \_ ->
                Bool.false

            List.count_if [1,2,3,4,5,6,7,8] always_false
            "
        ),
        0,
        u64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_count_if_condition() {
    assert_evals_to!(
        indoc!(
            r"
            int_is_less_than_three : I64 -> Bool
            int_is_less_than_three = \i ->
                i < 3

            List.count_if [1,2,3,4,5,6,7,8] int_is_less_than_three
            "
        ),
        2,
        u64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_count_if_str() {
    assert_evals_to!(
        indoc!(
            r#"
             List.count_if ["x", "y", "x"] (\x -> x == "x")
             "#
        ),
        2,
        u64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_map_on_empty_list_with_int_layout() {
    assert_evals_to!(
        indoc!(
            r"
            empty : List I64
            empty =
                []

            List.map empty (\x -> x)
            "
        ),
        RocList::<i64>::from_slice(&[]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_map_on_non_empty_list() {
    assert_evals_to!(
        indoc!(
            r"
            non_empty : List I64
            non_empty =
                [1]

            List.map non_empty (\x -> x)
            "
        ),
        RocList::from_slice(&[1]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_map_changes_input() {
    assert_evals_to!(
        indoc!(
            r"
            non_empty : List I64
            non_empty =
                [1]

            List.map non_empty (\x -> x + 1)
            "
        ),
        RocList::from_slice(&[2]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_map_on_big_list() {
    assert_evals_to!(
        indoc!(
            r"
            non_empty : List I64
            non_empty =
                [1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5]

            List.map non_empty (\x -> x * 2)
            "
        ),
        RocList::from_slice(&[
            2, 4, 6, 8, 10, 2, 4, 6, 8, 10, 2, 4, 6, 8, 10, 2, 4, 6, 8, 10, 2, 4, 6, 8, 10
        ]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_map_with_type_change() {
    assert_evals_to!(
        indoc!(
            r"
            non_empty : List I64
            non_empty =
                [1, 1, -4, 1, 2]


            List.map non_empty (\x -> x > 0)
            "
        ),
        RocList::from_slice(&[true, true, false, true, true]),
        RocList<bool>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_map_using_defined_function() {
    assert_evals_to!(
        indoc!(
            r"
             non_empty : List I64
             non_empty =
                 [2, 2, -4, 2, 3]

             greater_than_one : I64 -> Bool
             greater_than_one = \i ->
                 i > 1

             List.map non_empty greater_than_one
             "
        ),
        RocList::from_slice(&[true, true, false, true, true]),
        RocList<bool>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_map_all_inline() {
    assert_evals_to!(
        indoc!(
            r"
            List.map [] (\x -> x > 0)
            "
        ),
        RocList::<bool>::from_slice(&[]),
        RocList<bool>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_map_closure_int() {
    assert_evals_to!(
        indoc!(
            r"
            int : I64
            int = 123

            single : List I64
            single =
                [0]

            List.map single (\x -> x + int)
            "
        ),
        RocList::from_slice(&[123]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_map_closure_float() {
    assert_evals_to!(
        indoc!(
            r"
            float : F64
            float = 1.23

            single : List F64
            single =
                [0]

            List.map single (\x -> x + float)
            "
        ),
        RocList::from_slice(&[1.23]),
        RocList<f64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_map_closure_string() {
    assert_evals_to!(
        indoc!(
            r#"
            one : Str
            one = "one "

            List.map ["pear", "apple"] (\x -> Str.concat one x)
            "#
        ),
        RocList::from_slice(&[RocStr::from("one pear"), RocStr::from("one apple")]),
        RocList<RocStr>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_map4_group() {
    assert_evals_to!(
        indoc!(
            r"
            List.map4 [1,2,3] [3,2,1] [2,1,3] [3,1,2] (\a, b, c, d -> Group a b c d)
            "
        ),
        RocList::from_slice(&[[1, 3, 2, 3], [2, 2, 1, 1], [3, 1, 3, 2]]),
        RocList<[i64; 4]>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_map3_group() {
    assert_evals_to!(
        indoc!(
            r"
            List.map3 [1,2,3] [3,2,1] [2,1,3] (\a, b, c -> Group a b c)
            "
        ),
        RocList::from_slice(&[(1, 3, 2), (2, 2, 1), (3, 1, 3)]),
        RocList<(i64, i64, i64)>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_map2_pair() {
    assert_evals_to!(
        indoc!(
            r"
            f = (\a,b -> Pair a b)
            List.map2 [1,2,3] [3,2,1] f
            "
        ),
        RocList::from_slice(&[(1, 3), (2, 2), (3, 1)]),
        RocList<(i64, i64)>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_join_empty_list() {
    assert_evals_to!(
        "List.join []",
        RocList::<i64>::from_slice(&[]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_join_one_list() {
    assert_evals_to!(
        indoc!("List.walk [[1, 2, 3]] [] List.concat",),
        RocList::from_slice(&[1, 2, 3]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_join_two_non_empty_lists() {
    assert_evals_to!(
        "List.join [[1, 2, 3] , [4 ,5, 6]]",
        RocList::from_slice(&[1, 2, 3, 4, 5, 6]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_join_two_non_empty_lists_of_float() {
    assert_evals_to!(
        "List.join [[1.2f64, 1.1], [2.1, 2.2]]",
        RocList::from_slice(&[1.2, 1.1, 2.1, 2.2]),
        RocList<f64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_join_to_big_list() {
    assert_evals_to!(
        indoc!(
            r"
                List.join
                    [
                        [1.2f64, 1.1],
                        [2.1, 2.2],
                        [3.0, 4.0, 5.0, 6.1, 9.0],
                        [3.0, 4.0, 5.0, 6.1, 9.0],
                        [3.0, 4.0, 5.0, 6.1, 9.0],
                        [3.0, 4.0, 5.0, 6.1, 9.0],
                        [3.0, 4.0, 5.0, 6.1, 9.0]
                   ]
            "
        ),
        RocList::from_slice(&[
            1.2, 1.1, 2.1, 2.2, 3.0, 4.0, 5.0, 6.1, 9.0, 3.0, 4.0, 5.0, 6.1, 9.0, 3.0, 4.0, 5.0,
            6.1, 9.0, 3.0, 4.0, 5.0, 6.1, 9.0, 3.0, 4.0, 5.0, 6.1, 9.0
        ]),
        RocList<f64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_join_defined_empty_list() {
    assert_evals_to!(
        indoc!(
            r"
                empty : List F64
                empty =
                    []

                List.join [[0.2, 11.11], empty]
            "
        ),
        RocList::from_slice(&[0.2, 11.11]),
        RocList<f64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_join_all_empty_lists() {
    assert_evals_to!(
        "List.join [[], [], []]",
        RocList::<f64>::from_slice(&[]),
        RocList<f64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_join_one_empty_list() {
    assert_evals_to!(
        "List.join [[1.2f64, 1.1], []]",
        RocList::from_slice(&[1.2, 1.1]),
        RocList<f64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_single() {
    assert_evals_to!("List.single 1", RocList::from_slice(&[1]), RocList<i64>);
    assert_evals_to!(
        "List.single 5.6f32",
        RocList::from_slice(&[5.6]),
        RocList<f32>
    );
    assert_evals_to!(
        "List.single 5.6f64",
        RocList::from_slice(&[5.6]),
        RocList<f64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
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
            r"
                no_strs : List Str
                no_strs =
                    []

                List.repeat no_strs 2
            "
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_reverse_empty_list_of_int() {
    assert_evals_to!(
        indoc!(
            r"
                empty_list : List I64
                empty_list =
                    []

                List.reverse empty_list
            "
        ),
        RocList::<i64>::from_slice(&[]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_reverse_empty_list() {
    assert_evals_to!(
        "List.reverse []",
        RocList::<i64>::from_slice(&[]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_concat_two_empty_lists() {
    assert_evals_to!(
        "List.concat [] []",
        RocList::<i64>::from_slice(&[]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_concat_two_empty_lists_of_int() {
    assert_evals_to!(
        indoc!(
            r"
                first_list : List I64
                first_list =
                    []

                second_list : List I64
                second_list =
                    []

                List.concat first_list second_list
            "
        ),
        RocList::<i64>::from_slice(&[]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_concat_second_list_is_empty() {
    assert_evals_to!(
        "List.concat [12, 13] []",
        RocList::from_slice(&[12, 13]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_concat_first_list_is_empty() {
    assert_evals_to!(
        "List.concat [] [23, 24]",
        RocList::from_slice(&[23, 24]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_concat_two_bigger_non_empty_lists() {
    assert_evals_to!(
        "List.concat [1.1f64, 2.2] [3.3, 4.4, 5.5]",
        RocList::from_slice(&[1.1f64, 2.2, 3.3, 4.4, 5.5]),
        RocList<f64>
    );
}

#[allow(dead_code)]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn assert_concat_worked(num_elems1: i64, num_elems2: i64) {
    let vec1: Vec<i64> = (0..num_elems1)
        .map(|i| 12345 % (i + num_elems1 + num_elems2 + 1))
        .collect();
    let vec2: Vec<i64> = (0..num_elems2)
        .map(|i| 54321 % (i + num_elems1 + num_elems2 + 1))
        .collect();
    let slice_str1 = format!("{vec1:?}");
    let slice_str2 = format!("{vec2:?}");
    let mut expected = vec1;

    expected.extend(vec2);

    assert_evals_to!(
        &format!("List.concat {slice_str1} {slice_str2}"),
        RocList::from_slice(&expected),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn empty_list_len() {
    assert_evals_to!("List.len []", 0, u64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn basic_int_list_len() {
    assert_evals_to!("List.len [12, 9, 6, 3]", 4, u64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn loaded_int_list_len() {
    assert_evals_to!(
        indoc!(
            r"
                nums = [2, 4, 6]

                List.len nums
            "
        ),
        3,
        u64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn fn_int_list_len() {
    assert_evals_to!(
        indoc!(
            r"
                get_len = \list -> List.len list

                nums = [2, 4, 6, 8]

                get_len nums
            "
        ),
        4,
        u64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn int_list_is_empty() {
    assert_evals_to!("List.is_empty [12, 9, 6, 3]", false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn empty_list_is_empty() {
    assert_evals_to!("List.is_empty []", true, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn first_int_list() {
    assert_evals_to!(
        indoc!(
            r"
            List.first [12, 9, 6, 3]
            "
        ),
        RocResult::ok(12),
        RocResult<i64, ()>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn first_str_list() {
    assert_evals_to!(
        indoc!(
            r#"
            List.first ["short", "bar"]
            "#
        ),
        RocResult::ok(RocStr::from("short")),
        RocResult<RocStr, ()>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn first_wildcard_empty_list() {
    assert_evals_to!(
        indoc!(
            r"
            List.last [] |> Result.map_ok (\_ -> 0i64)
            "
        ),
        RocResult::err(()),
        RocResult<i64, ()>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn first_empty_list() {
    assert_evals_to!(
        indoc!(
            r"
            list : List I64
            list = []

            List.first list
            "
        ),
        RocResult::err(()),
        RocResult<i64, ()>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn last_int_list() {
    assert_evals_to!(
        indoc!(
            r"
            List.last [12, 9, 6, 3]
            "
        ),
        RocResult::ok(3),
        RocResult<i64, ()>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn last_wildcard_empty_list() {
    assert_evals_to!(
        indoc!(
            r"
            List.last [] |> Result.map_ok (\_ -> 0i64)
            "
        ),
        RocResult::err(()),
        RocResult<i64, ()>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn last_empty_list() {
    assert_evals_to!(
        indoc!(
            r"
            list : List I64
            list = []

            List.last list
            "
        ),
        RocResult::err(()),
        RocResult<i64, ()>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn get_empty_list() {
    assert_evals_to!(
        indoc!(
            r"
            list : List I64
            list = []

            List.get list 0
            "
        ),
        RocResult::err(()),
        RocResult<i64, ()>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn get_wildcard_empty_list() {
    // NOTE: by default, the return type is `Result [] [NotFound]`, which is actually represented
    // as just `[NotFound]`. Casting that to `RocResult<(), ()>` is invalid! But accepting any `()`
    // would make the test pointless. Therefore, we must explicitly change the type on the roc side
    assert_evals_to!(
        indoc!(
            r"
            List.get [] 0
            |> Result.map_ok (\_ -> {})
            "
        ),
        RocResult::err(()),
        RocResult<(), ()>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn get_str_list_ok() {
    assert_evals_to!(
        indoc!(
            r#"
            List.get ["foo", "bar"] 1
            "#
        ),
        RocResult::ok(RocStr::from("bar")),
        RocResult<RocStr, ()>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn get_int_list_ok() {
    assert_evals_to!(
        indoc!(
            r"
            List.get [12, 9, 6] 1
            "
        ),
        RocResult::ok(9),
        RocResult<i64, ()>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn get_int_list_oob() {
    assert_evals_to!(
        indoc!(
            r"
            List.get [12, 9, 6] 1000
            "
        ),
        RocResult::err(()),
        RocResult<i64, ()>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn replace_unique_int_list() {
    assert_evals_to!(
        indoc!(
            r"
                record = List.replace [12, 9, 7, 1, 5] 2 33
                record.list
            "
        ),
        RocList::from_slice(&[12, 9, 33, 1, 5]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn replace_unique_int_list_out_of_bounds() {
    assert_evals_to!(
        indoc!(
            r"
                record = List.replace [12, 9, 7, 1, 5] 5 33
                record.value
            "
        ),
        33,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn replace_unique_int_list_get_old_value() {
    assert_evals_to!(
        indoc!(
            r"
                record = List.replace [12, 9, 7, 1, 5] 2 33
                record.value
            "
        ),
        7,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn replace_unique_get_large_value() {
    assert_evals_to!(
        indoc!(
            r"
                list : List { a : U64, b: U64, c: U64, d: U64 }
                list = [{ a: 1, b: 2, c: 3, d: 4 }, { a: 5, b: 6, c: 7, d: 8 }, { a: 9, b: 10, c: 11, d: 12 }]
                record = List.replace list 1 { a: 13, b: 14, c: 15, d: 16 }
                record.value
            "
        ),
        [5, 6, 7, 8],
        [u64; 4]
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn replace_shared_int_list() {
    assert_evals_to!(
        indoc!(
            r"
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

            wrapper [2.1f64, 4.3]
            "
        ),
        (7.7, 4.3),
        (f64, f64)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn get_set_unique_int_list_i64() {
    assert_evals_to!(
        indoc!(
            r"
            List.get (List.set [12, 9, 7, 3] 1 42) 1
            "
        ),
        RocResult::ok(42),
        RocResult<i64, ()>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn get_set_unique_int_list_i8() {
    assert_evals_to!(
        indoc!(
            r"
            List.get (List.set [12, 9, 7, 3] 1 42i8) 1
            "
        ),
        RocResult::ok(42),
        RocResult<i8, ()>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn set_unique_int_list() {
    assert_evals_to!(
        "List.set [12, 9, 7, 1, 5] 2 33",
        RocList::from_slice(&[12, 9, 33, 1, 5]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn set_unique_list_oob() {
    assert_evals_to!(
        "List.set [3f64, 17, 4.1] 1337 9.25",
        RocList::from_slice(&[3.0, 17.0, 4.1]),
        RocList<f64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn set_shared_int_list() {
    assert_evals_to!(
        indoc!(
            r"
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

            wrapper [2.1f64, 4.3]
            "
        ),
        (7.7, 4.3),
        (f64, f64)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn set_shared_list_oob() {
    assert_evals_to!(
        indoc!(
            r"
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
            "
        ),
        (4, 4),
        (i64, i64)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn get_unique_int_list() {
    assert_evals_to!(
        indoc!(
            r"
                unique = [2, 4]

                List.get unique 1
            "
        ),
        RocResult::ok(4),
        RocResult<i64, ()>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_wrap_len() {
    assert_evals_to!(
        indoc!(
            r"
                wrap_len = \list ->
                    [List.len list]

                wrap_len [1, 7, 9]
            "
        ),
        RocList::from_slice(&[3]),
        RocList<u64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_wrap_first() {
    assert_evals_to!(
        indoc!(
            r"
                wrap_first = \list ->
                    [List.first list]

                wrap_first [1, 2]
            "
        ),
        RocList::from_slice(&[1]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_duplicate() {
    assert_evals_to!(
        indoc!(
            r"
                # Duplicate the first element into the second index
                dupe = \list ->
                    when List.first list is
                        Ok elem ->
                            List.set list 1 elem

                        _ ->
                            []

                dupe [1, 2]
            "
        ),
        RocList::from_slice(&[1, 1]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_swap() {
    assert_evals_to!(
        indoc!(
            r#"
            app "quicksort" provides [main] to "./platform"


            swap : U64, U64, List a -> List a
            swap = \i, j, list ->
                when Pair (List.get list i) (List.get list j) is
                    Pair (Ok at_i) (Ok at_j) ->
                        list
                            |> List.set i at_j
                            |> List.set j at_i

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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_quicksort() {
    with_larger_debug_stack(|| {
        assert_evals_to!(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                quicksort : List (Num a) -> List (Num a)
                quicksort = \list ->
                    n = List.len list
                    quicksort_help list 0 (n - 1)


                quicksort_help : List (Num a), U64, U64 -> List (Num a)
                quicksort_help = \list, low, high ->
                    if low < high then
                        when partition low high list is
                            Pair partition_index partitioned ->
                                partitioned
                                    |> quicksort_help low (Num.sub_saturated partition_index 1)
                                    |> quicksort_help (partition_index + 1) high
                    else
                        list


                swap : U64, U64, List a -> List a
                swap = \i, j, list ->
                    when Pair (List.get list i) (List.get list j) is
                        Pair (Ok at_i) (Ok at_j) ->
                            list
                                |> List.set i at_j
                                |> List.set j at_i

                        _ ->
                            []

                partition : U64, U64, List (Num a) -> [Pair U64 (List (Num a))]
                partition = \low, high, initial_list ->
                    when List.get initial_list high is
                        Ok pivot ->
                            when partition_help low low initial_list high pivot is
                                Pair new_i new_list ->
                                    Pair new_i (swap new_i high new_list)

                        Err _ ->
                            Pair low initial_list


                partition_help : U64, U64, List (Num a), U64, (Num a) -> [Pair U64 (List (Num a))]
                partition_help = \i, j, list, high, pivot ->
                    if j < high then
                        when List.get list j is
                            Ok value ->
                                if value <= pivot then
                                    partition_help (i + 1) (j + 1) (swap i j list) high pivot
                                else
                                    partition_help i (j + 1) list high pivot

                            Err _ ->
                                Pair i list
                    else
                        Pair i list

                main = quicksort [7, 4, 21, 19]
            "#
            ),
            RocList::from_slice(&[4, 7, 19, 21]),
            RocList<i64>
        );
    })
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn quicksort() {
    with_larger_debug_stack(|| {
        assert_evals_to!(
            indoc!(
                r#"
                   app "test" provides [main] to "./platform"

                   quicksort : List (Num a) -> List (Num a)
                   quicksort = \list ->
                       quicksort_help list 0 (List.len list - 1)


                   quicksort_help : List (Num a), U64, U64 -> List (Num a)
                   quicksort_help = \list, low, high ->
                       if low < high then
                           when partition low high list is
                               Pair partition_index partitioned ->
                                   partitioned
                                       |> quicksort_help low (Num.sub_saturated partition_index 1)
                                       |> quicksort_help (partition_index + 1) high
                       else
                           list


                   swap : U64, U64, List a -> List a
                   swap = \i, j, list ->
                       when Pair (List.get list i) (List.get list j) is
                           Pair (Ok at_i) (Ok at_j) ->
                               list
                                   |> List.set i at_j
                                   |> List.set j at_i

                           _ ->
                               []

                   partition : U64, U64, List (Num a) -> [Pair U64 (List (Num a))]
                   partition = \low, high, initial_list ->
                       when List.get initial_list high is
                           Ok pivot ->
                               when partition_help low low initial_list high pivot is
                                   Pair new_i new_list ->
                                       Pair new_i (swap new_i high new_list)

                           Err _ ->
                               Pair low initial_list


                   partition_help : U64, U64, List (Num a), U64, Num a -> [Pair U64 (List (Num a))]
                   partition_help = \i, j, list, high, pivot ->
                       # if j < high then
                       if Bool.false then
                           when List.get list j is
                               Ok value ->
                                   if value <= pivot then
                                       partition_help (i + 1) (j + 1) (swap i j list) high pivot
                                   else
                                       partition_help i (j + 1) list high pivot

                               Err _ ->
                                   Pair i list
                       else
                           Pair i list



                   main = quicksort [7, 4, 21, 19]
               "#
            ),
            RocList::from_slice(&[19, 7, 4, 21]),
            RocList<i64>
        );
    })
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn quicksort_singleton() {
    with_larger_debug_stack(|| {
        assert_evals_to!(
            indoc!(
                r#"
                   app "test" provides [main] to "./platform"

                   quicksort : List (Num a) -> List (Num a)
                   quicksort = \list ->
                       quicksort_help list 0 (List.len list - 1)


                   quicksort_help : List (Num a), U64, U64 -> List (Num a)
                   quicksort_help = \list, low, high ->
                       if low < high then
                           when partition low high list is
                               Pair partition_index partitioned ->
                                   partitioned
                                       |> quicksort_help low (Num.sub_saturated partition_index 1)
                                       |> quicksort_help (partition_index + 1) high
                       else
                           list


                   swap : U64, U64, List a -> List a
                   swap = \i, j, list ->
                       when Pair (List.get list i) (List.get list j) is
                           Pair (Ok at_i) (Ok at_j) ->
                               list
                                   |> List.set i at_j
                                   |> List.set j at_i

                           _ ->
                               []

                   partition : U64, U64, List (Num a) -> [Pair U64 (List (Num a))]
                   partition = \low, high, initial_list ->
                       when List.get initial_list high is
                           Ok pivot ->
                               when partition_help low low initial_list high pivot is
                                   Pair new_i new_list ->
                                       Pair new_i (swap new_i high new_list)

                           Err _ ->
                               Pair low initial_list


                   partition_help : U64, U64, List (Num a), U64, Num a -> [Pair U64 (List (Num a))]
                   partition_help = \i, j, list, high, pivot ->
                       if j < high then
                           when List.get list j is
                               Ok value ->
                                   if value <= pivot then
                                       partition_help (i + 1) (j + 1) (swap i j list) high pivot
                                   else
                                       partition_help i (j + 1) list high pivot

                               Err _ ->
                                   Pair i list
                       else
                           Pair i list



                   main =
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn empty_list_increment_decrement() {
    assert_evals_to!(
        indoc!(
            r"
            x : List I64
            x = []

            List.len x + List.len x
            "
        ),
        0,
        u64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_literal_increment_decrement() {
    assert_evals_to!(
        indoc!(
            r"
            x : List I64
            x = [1,2,3]

            List.len x + List.len x
            "
        ),
        6,
        u64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_pass_to_function() {
    assert_evals_to!(
        indoc!(
            r"
            x : List I64
            x = [1,2,3]

            id : List I64 -> List I64
            id = \y -> y

            id x
            "
        ),
        RocList::from_slice(&[1, 2, 3]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_pass_to_set() {
    assert_evals_to!(
        indoc!(
            r"
            x : List I64
            x = [1,2,3]

            id : List I64 -> List I64
            id = \y -> List.set y 0 0

            id x
            "
        ),
        RocList::from_slice(&[0, 2, 3]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_wrap_in_tag() {
    assert_evals_to!(
        indoc!(
            r"
            id : List I64 -> [Pair (List I64) I64]
            id = \y -> Pair y 4

            when id [1,2,3] is
                Pair v _ -> v
            "
        ),
        RocList::from_slice(&[1, 2, 3]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_contains_int() {
    assert_evals_to!(indoc!("List.contains [1,2,3] 1"), true, bool);

    assert_evals_to!(indoc!("List.contains [1,2,3] 4"), false, bool);

    assert_evals_to!(indoc!("List.contains [] 4"), false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_manual_range() {
    assert_evals_to!(
        indoc!(
            r"
            range : I64, I64, List I64-> List I64
            range = \low, high, accum ->
                if low < high then
                    range (low + 1) high (List.append accum low)
                else
                    accum

            range 0 5 [42]
            "
        ),
        RocList::from_slice(&[42, 0, 1, 2, 3, 4]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_min() {
    assert_evals_to!(
        indoc!(
            r"
            List.min []
            |> Result.map_ok (\_ -> {})
            "
        ),
        RocResult::err(()),
        RocResult<(), ()>
    );

    assert_evals_to!(
        indoc!(
            r"
            List.min [3, 1, 2]
            "
        ),
        RocResult::ok(1),
        RocResult<i64, ()>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_max() {
    assert_evals_to!(
        indoc!(
            r"
            List.max []
            |> Result.map_ok (\_ -> {})
            "
        ),
        RocResult::err(()),
        RocResult<(), ()>
    );

    assert_evals_to!(
        indoc!(
            r"
            List.max [3, 1, 2]
            "
        ),
        RocResult::ok(3),
        RocResult<i64, ()>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_sum() {
    assert_evals_to!("List.sum []", 0, i64);
    assert_evals_to!("List.sum [1, 2, 3]", 6, i64);
    assert_evals_to!("List.sum [1.1f64, 2.2, 3.3]", 6.6, f64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_sum_dec() {
    assert_evals_to!("List.sum [1.0dec, 2.0]", RocDec::from(3), RocDec);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_product() {
    assert_evals_to!("List.product []", 1, i64);
    assert_evals_to!("List.product [1, 2, 3]", 6, i64);
    assert_evals_to!("List.product [1.1f64, 2.2, 3.3]", 1.1 * 2.2 * 3.3, f64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_keep_void() {
    assert_evals_to!(
        "List.keep_oks [] (\\x -> x)",
        RocList::from_slice(&[]),
        RocList<()>
    );

    assert_evals_to!(
        "List.keep_errs [] (\\x -> x)",
        RocList::from_slice(&[]),
        RocList<()>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_keep_oks() {
    assert_evals_to!(
        "List.keep_oks [Ok {}, Ok {}] (\\x -> x)",
        RocList::from_slice(&[(), ()]),
        RocList<()>
    );
    assert_evals_to!(
        "List.keep_oks [1,2] (\\x -> Ok x)",
        RocList::from_slice(&[1, 2]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.keep_oks [1,2] (\\x -> Num.rem_checked x 2)",
        RocList::from_slice(&[1, 0]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.keep_oks [Ok 1, Err 2] (\\x -> x)",
        RocList::from_slice(&[1]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_keep_errs() {
    assert_evals_to!(
        "List.keep_errs [Err {}, Err {}] (\\x -> x)",
        RocList::from_slice(&[(), ()]),
        RocList<()>
    );
    assert_evals_to!(
        "List.keep_errs [1,2] (\\x -> Err x)",
        RocList::from_slice(&[1, 2]),
        RocList<i64>
    );
    assert_evals_to!(
        indoc!(
            r"
            List.keep_errs [0,1,2] (\x -> Num.rem_checked x 0 |> Result.map_err (\_ -> 32))
            "
        ),
        RocList::from_slice(&[32, 32, 32]),
        RocList<i64>
    );

    assert_evals_to!(
        "List.keep_errs [Ok 1, Err 2] (\\x -> x)",
        RocList::from_slice(&[2]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_map_with_index() {
    assert_evals_to!(
        "List.map_with_index [0,0,0] (\\x, index -> Num.int_cast index + x)",
        RocList::from_slice(&[0, 1, 2]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
#[should_panic(expected = r#"Roc failed with message: "Integer addition overflowed!"#)]
fn cleanup_because_exception() {
    assert_evals_to!(
        indoc!(
            r"
            x = [1,2]

            five : I64
            five = 5

            five + Num.max_i64 + 3 + (Num.int_cast (List.len x))
               "
        ),
        9,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_sort_with() {
    assert_evals_to!(
        "List.sort_with [] Num.compare",
        RocList::<i64>::from_slice(&[]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.sort_with [4,3,2,1] Num.compare",
        RocList::from_slice(&[1, 2, 3, 4]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.sort_with [1,2,3,4] (\\a,b -> Num.compare b a)",
        RocList::from_slice(&[4, 3, 2, 1]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_sort_asc() {
    assert_evals_to!(
        "List.sort_asc []",
        RocList::<i64>::from_slice(&[]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.sort_asc [4,3,2,1]",
        RocList::from_slice(&[1, 2, 3, 4]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_sort_desc() {
    assert_evals_to!(
        "List.sort_desc []",
        RocList::<i64>::from_slice(&[]),
        RocList<i64>
    );
    assert_evals_to!(
        "List.sort_desc [1,2,3,4]",
        RocList::from_slice(&[4, 3, 2, 1]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_any() {
    assert_evals_to!("List.any [] (\\e -> e > 3)", false, bool);
    assert_evals_to!("List.any [1, 2, 3] (\\e -> e > 3)", false, bool);
    assert_evals_to!("List.any [1, 2, 4] (\\e -> e > 3)", true, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_any_empty_with_unknown_element_type() {
    assert_evals_to!("List.any [] (\\_ -> Bool.true)", false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_all() {
    assert_evals_to!("List.all [] (\\e -> e > 3)", true, bool);
    assert_evals_to!("List.all [1, 2, 3] (\\e -> e > 3)", false, bool);
    assert_evals_to!("List.all [1, 2, 4] (\\e -> e > 3)", false, bool);
    assert_evals_to!("List.all [1, 2, 3] (\\e -> e >= 1)", true, bool);
}

#[test]
#[cfg(feature = "gen-llvm")]
fn list_all_empty_with_unknown_element_type() {
    assert_evals_to!("List.all [] (\\_ -> Bool.true)", true, bool);
}

#[test]
// This doesn't work on Windows. If you make it return a `bool`, e.g. with `|> Str.is_empty` at the end,
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn map_with_index_multi_record() {
    // see https://github.com/roc-lang/roc/issues/1700
    assert_evals_to!(
        indoc!(
            r"
            List.map_with_index [{ x: {}, y: {} }] \_, _ -> {}
            "
        ),
        RocList::from_slice(&[((), ())]),
        RocList<((), ())>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn empty_list_of_function_type() {
    // see https://github.com/roc-lang/roc/issues/1732
    assert_evals_to!(
        indoc!(
            r#"
            my_list : List (Str -> Str)
            my_list = []

            my_closure : Str -> Str
            my_closure = \_ -> "bar"

            choose =
                if Bool.false then
                    my_list
                else
                    [my_closure]

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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_join_map() {
    assert_evals_to!(
        indoc!(
            r#"
            List.join_map ["guava,apple,pear", "bailey,cyrus"] (\s -> Str.split_on s ",")
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_join_map_empty() {
    assert_evals_to!(
        indoc!(
            r#"
            List.join_map [] (\s -> Str.split_on s ",")
            "#
        ),
        RocList::from_slice(&[]),
        RocList<RocStr>
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_find() {
    assert_evals_to!(
        indoc!(
            r#"
            when List.find_first ["a", "bc", "def", "g"] (\s -> Str.count_utf8_bytes s > 1) is
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
            when List.find_last ["a", "bc", "def", "g"] (\s -> Str.count_utf8_bytes s > 1) is
                Ok v -> v
                Err _ -> "not found"
            "#
        ),
        RocStr::from("def"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_find_not_found() {
    assert_evals_to!(
        indoc!(
            r#"
            when List.find_first ["a", "bc", "def", "g"] (\s -> Str.count_utf8_bytes s > 5) is
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
            when List.find_last ["a", "bc", "def", "g"] (\s -> Str.count_utf8_bytes s > 5) is
                Ok v -> v
                Err _ -> "not found"
            "#
        ),
        RocStr::from("not found"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_find_empty_typed_list() {
    assert_evals_to!(
        indoc!(
            r#"
            when List.find_first [] (\s -> Str.count_utf8_bytes s > 5) is
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
            when List.find_last [] (\s -> Str.count_utf8_bytes s > 5) is
                Ok v -> v
                Err _ -> "not found"
            "#
        ),
        RocStr::from("not found"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_find_empty_layout() {
    assert_evals_to!(
        indoc!(
            r"
            List.find_first [] \_ -> Bool.true
            "
        ),
        // [Ok [], Err [NotFound]] gets unwrapped all the way to just [NotFound],
        // which is the unit!
        (),
        ()
    );

    assert_evals_to!(
        indoc!(
            r"
            List.find_last [] \_ -> Bool.true
            "
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
            when List.find_first_index ["a", "bc", "def", "g"] (\s -> Str.count_utf8_bytes s > 1) is
                Ok v -> v
                Err _ -> 999
            "#
        ),
        1,
        u64
    );

    assert_evals_to!(
        indoc!(
            r#"
            when List.find_last_index ["a", "bc", "def", "g"] (\s -> Str.count_utf8_bytes s > 1) is
                Ok v -> v
                Err _ -> 999
            "#
        ),
        2,
        u64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_find_index_not_found() {
    assert_evals_to!(
        indoc!(
            r#"
            when List.find_first_index ["a", "bc", "def", "g"] (\s -> Str.count_utf8_bytes s > 5) is
                Ok v -> v
                Err _ -> 999
            "#
        ),
        999,
        u64
    );

    assert_evals_to!(
        indoc!(
            r#"
            when List.find_last_index ["a", "bc", "def"] (\s -> Str.count_utf8_bytes s > 5) is
                Ok v -> v
                Err _ -> 999
            "#
        ),
        999,
        u64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_find_index_empty_typed_list() {
    assert_evals_to!(
        indoc!(
            r"
            when List.find_first_index [] (\s -> Str.count_utf8_bytes s > 5) is
                Ok v -> v
                Err _ -> 999
            "
        ),
        999,
        u64
    );

    assert_evals_to!(
        indoc!(
            r"
            when List.find_last_index [] (\s -> Str.count_utf8_bytes s > 5) is
                Ok v -> v
                Err _ -> 999
            "
        ),
        999,
        u64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_ends_with_empty() {
    assert_evals_to!(
        indoc!(
            r"
            List.ends_with [] []
            "
        ),
        true,
        bool
    );

    assert_evals_to!(
        indoc!(
            r#"
            List.ends_with ["a"] []
            "#
        ),
        true,
        bool
    );

    assert_evals_to!(
        indoc!(
            r#"
            List.ends_with [] ["a"]
            "#
        ),
        false,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_ends_with_nonempty() {
    assert_evals_to!(
        indoc!(
            r#"
            List.ends_with ["a", "bc", "def"] ["def"]
            "#
        ),
        true,
        bool
    );

    assert_evals_to!(
        indoc!(
            r#"
            List.ends_with ["a", "bc", "def"] ["bc", "def"]
            "#
        ),
        true,
        bool
    );

    assert_evals_to!(
        indoc!(
            r#"
            List.ends_with ["a", "bc", "def"] ["a"]
            "#
        ),
        false,
        bool
    );

    assert_evals_to!(
        indoc!(
            r#"
            List.ends_with ["a", "bc", "def"] [""]
            "#
        ),
        false,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_starts_with_empty() {
    assert_evals_to!(
        indoc!(
            r"
            List.starts_with [] []
            "
        ),
        true,
        bool
    );

    assert_evals_to!(
        indoc!(
            r#"
            List.starts_with ["a"] []
            "#
        ),
        true,
        bool
    );

    assert_evals_to!(
        indoc!(
            r#"
            List.starts_with [] ["a"]
            "#
        ),
        false,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_starts_with_nonempty() {
    assert_evals_to!(
        indoc!(
            r#"
            List.starts_with ["a", "bc", "def"] ["a"]
            "#
        ),
        true,
        bool
    );

    assert_evals_to!(
        indoc!(
            r#"
            List.starts_with ["a", "bc", "def"] ["a", "bc"]
            "#
        ),
        true,
        bool
    );

    assert_evals_to!(
        indoc!(
            r#"
            List.starts_with ["a", "bc", "def"] ["def"]
            "#
        ),
        false,
        bool
    );

    assert_evals_to!(
        indoc!(
            r#"
            List.starts_with ["a", "bc", "def"] [""]
            "#
        ),
        false,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn monomorphized_lists() {
    assert_evals_to!(
        indoc!(
            r"
            l = \{} -> [1, 2, 3]

            f : List U8, List U16 -> U64
            f = \_, _ -> 18

            f (l {}) (l {})
            "
        ),
        18,
        u64
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn with_capacity() {
    assert_evals_to!(
        indoc!(
            r"
            l : List U64
            l = List.with_capacity 10

            l
            "
        ),
        // Equality check for RocList does not account for capacity
        (10, RocList::with_capacity(10)),
        RocList<u64>,
        |value: RocList<u64>| (value.capacity(), value)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn with_capacity_append() {
    // see https://github.com/roc-lang/roc/issues/1732
    assert_evals_to!(
        indoc!(
            r"
            List.with_capacity 10
                |> List.append 0u64
                |> List.append 1u64
                |> List.append 2u64
                |> List.append 3u64
            "
        ),
        (10, RocList::from_slice(&[0, 1, 2, 3])),
        RocList<u64>,
        |value: RocList<u64>| (value.capacity(), value)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn reserve() {
    assert_evals_to!(
        indoc!(
            r"
            List.reserve [] 15
            "
        ),
        (15, RocList::empty()),
        RocList<u64>,
        |value: RocList<u64>| (value.capacity(), value)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn reserve_unchanged() {
    assert_evals_to!(
        indoc!(
            r"
            a = []
            b = List.reserve a 15
            {a, b}
            "
        ),
        // a's capacity is unchanged when we reserve 15 more capcity
        // both lists are empty.
        (0, RocList::empty(), 15, RocList::empty()),
        (RocList<u64>, RocList<u64>),
        |(value_a, value_b): (RocList<u64>, RocList<u64>)| (
            value_a.capacity(),
            value_a,
            value_b.capacity(),
            value_b
        )
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn release_excess_capacity() {
    assert_evals_to!(
        indoc!(
            r"
            List.reserve [] 15
            |> List.release_excess_capacity
            "
        ),
        (0, RocList::empty()),
        RocList<u64>,
        |value: RocList<u64>| (value.capacity(), value)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn release_excess_capacity_with_len() {
    assert_evals_to!(
        indoc!(
            r"
            List.reserve [1] 50
            |> List.release_excess_capacity
            "
        ),
        (1, RocList::from_slice(&[1])),
        RocList<u64>,
        |value: RocList<u64>| (value.capacity(), value)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn release_excess_capacity_empty() {
    assert_evals_to!(
        indoc!(
            r"
            List.release_excess_capacity []
            "
        ),
        (0, RocList::empty()),
        RocList<u64>,
        |value: RocList<u64>| (value.capacity(), value)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn call_function_in_empty_list() {
    assert_evals_to!(
        indoc!(
            r"
            lst : List ({} -> {})
            lst = []
            List.map lst \f -> f {}
            "
        ),
        RocList::from_slice(&[]),
        RocList<()>
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn call_function_in_empty_list_unbound() {
    assert_evals_to!(
        indoc!(
            r"
            lst = []
            List.map lst \f -> f {}
            "
        ),
        RocList::from_slice(&[]),
        RocList<()>
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn issue_3571_lowlevel_call_function_with_bool_lambda_set() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            apply : List (a -> b), List a -> List b
            apply = \funs, vals ->
                initial = List.with_capacity ((List.len funs) * (List.len vals))
                List.walk funs initial \state, fun ->
                    mapped_vals = List.map vals fun
                    List.concat state mapped_vals

            add2 : Str -> Str
            add2 = \x -> "added ${x}"

            mul2 : Str -> Str
            mul2 = \x -> "multiplied ${x}"

            foo = [add2, mul2]
            bar = ["1", "2", "3", "4"]

            main = foo |> apply bar |> Str.join_with ", "
            "#
        ),
        RocStr::from("added 1, added 2, added 3, added 4, multiplied 1, multiplied 2, multiplied 3, multiplied 4"),
        RocStr
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn issue_3530_uninitialized_capacity_in_list_literal() {
    assert_evals_to!(
        indoc!(
            r"
            [11,22,33]
            "
        ),
        3,
        (usize, usize, usize),
        |(_, _, cap)| cap
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_infer_usage() {
    assert_evals_to!(
        indoc!(
            r#"
            empty : List _
            empty = []

            xs : List Str
            xs = List.append empty "foo"

            List.len xs
            "#
        ),
        1,
        u64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_walk_backwards_implements_position() {
    assert_evals_to!(
        r"
        Option a : [Some a, None]

        find : List a, a -> Option U64 where a implements Eq
        find = \list, needle ->
            find_help list needle
                |> .v

        find_help = \list, needle ->
            List.walk_backwards_until list { n: 0, v: None } \{ n, v }, element ->
                if element == needle then
                    Break { n, v: Some n }
                else
                    Continue { n: n + 1, v }

        when find [1, 2, 3] 3 is
            None -> 0
            Some v -> v
        ",
        0,
        u64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_walk_backwards_until_sum() {
    assert_evals_to!(
        r"List.walk_backwards_until [1, 2] 0 \a,b -> Continue (a + b)",
        3,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_walk_backwards_until_even_prefix_sum() {
    assert_evals_to!(
        r"
        helper = \a, b ->
            if Num.is_even b then
                Continue (a + b)

            else
                Break a

        List.walk_backwards_until [9, 8, 4, 2] 0 helper",
        2 + 4 + 8,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_walk_from_until_sum() {
    assert_evals_to!(
        r"List.walk_from_until [1, 2, 3, 4] 2 0 \a,b -> Continue (a + b)",
        7,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn concat_unique_to_nonunique_overlapping_issue_4697() {
    assert_evals_to!(
        r"
        # original_list is shared, but others is unique.
        # When we concat original_list with others, others should be re-used.

        original_list = [1u8]
        others = [2u8, 3u8, 4u8]
        new = List.concat original_list others
        {a: original_list, b: new}
        ",
        (
            RocList::from_slice(&[1u8]),
            RocList::from_slice(&[1u8, 2, 3, 4]),
        ),
        (RocList<u8>, RocList<u8>)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_walk_from_even_prefix_sum() {
    assert_evals_to!(
        r"
        helper = \a, b ->
            if Num.is_even b then
                Continue (a + b)

            else
                Break a

        List.walk_from_until [2, 4, 8, 9] 1 0 helper",
        4 + 8,
        i64
    );
}

#[test]
#[cfg(feature = "gen-llvm")]
// TODO: update how roc decides whether or not to print `User crashed` or `Roc failed` such that this prints `Roc failed ...``
#[should_panic(
    expected = r#"User crash with message: "List.range: failed to generate enough elements to fill the range before overflowing the numeric type"#
)]
fn list_range_length_overflow() {
    assert_evals_to!(
        indoc!(
            r"
            List.range {start: At 255u8, end: Length 2}
               "
        ),
        RocList::<u8>::default(),
        RocList::<u8>
    );
}

mod pattern_match {
    #[allow(unused_imports)]
    use crate::helpers::with_larger_debug_stack;

    #[cfg(feature = "gen-llvm")]
    use crate::helpers::llvm::assert_evals_to;

    #[cfg(feature = "gen-wasm")]
    use crate::helpers::wasm::assert_evals_to;

    #[cfg(feature = "gen-dev")]
    use crate::helpers::dev::assert_evals_to;

    use super::{RocList, RocStr};

    #[test]
    #[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
    fn unary_exact_size_match() {
        assert_evals_to!(
            r"
            helper = \l -> when l is
                [] -> 1u8
                _ -> 2u8

            [ helper [], helper [{}] ]
            ",
            RocList::from_slice(&[1, 2]),
            RocList<u8>
        )
    }

    #[test]
    #[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
    fn many_exact_size_match() {
        assert_evals_to!(
            r"
            helper = \l -> when l is
                [] -> 1u8
                [_] -> 2u8
                [_, _] -> 3u8
                [_, _, _] -> 4u8
                _ -> 5u8

            [ helper [], helper [{}], helper [{}, {}], helper [{}, {}, {}], helper [{}, {}, {}, {}] ]
            ",
            RocList::from_slice(&[1, 2, 3, 4, 5]),
            RocList<u8>
        )
    }

    #[test]
    #[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
    fn ranged_matches_head() {
        with_larger_debug_stack(|| {
            assert_evals_to!(
                r"
                helper = \l -> when l is
                    [] -> 1u8
                    [A] -> 2u8
                    [A, A, ..] -> 3u8
                    [A, B, ..] -> 4u8
                    [B, ..] -> 5u8

                [
                    helper [],
                    helper [A],
                    helper [A, A], helper [A, A, A], helper [A, A, B], helper [A, A, B, A],
                    helper [A, B], helper [A, B, A], helper [A, B, B], helper [A, B, A, B],
                    helper [B], helper [B, A], helper [B, B], helper [B, A, B, B],
                ]
                ",
                RocList::from_slice(&[
                    1, //
                    2, //
                    3, 3, 3, 3, //
                    4, 4, 4, 4, //
                    5, 5, 5, 5, //
                ]),
                RocList<u8>
            )
        });
    }

    #[test]
    #[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
    fn ranged_matches_tail() {
        with_larger_debug_stack(|| {
            assert_evals_to!(
                r"
                helper = \l -> when l is
                    [] -> 1u8
                    [A] -> 2u8
                    [.., A, A] -> 3u8
                    [.., B, A] -> 4u8
                    [.., B] -> 5u8

                [
                    helper [],
                    helper [A],
                    helper [A, A], helper [A, A, A], helper [B, A, A], helper [A, B, A, A],
                    helper [B, A], helper [A, B, A], helper [B, B, A], helper [B, A, B, A],
                    helper [B], helper [A, B], helper [B, B], helper [B, A, B, B],
                ]
                ",
                RocList::from_slice(&[
                    1, //
                    2, //
                    3, 3, 3, 3, //
                    4, 4, 4, 4, //
                    5, 5, 5, 5, //
                ]),
                RocList<u8>
            )
        })
    }

    #[test]
    #[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
    fn bind_variables() {
        assert_evals_to!(
            r"
            helper : List U16 -> U16
            helper = \l -> when l is
                [] -> 1
                [x] -> x
                [.., w, x, y, z] -> w * x * y * z
                [x, y, ..] -> x * y

            [
                helper [],
                helper [5],
                helper [3, 5], helper [3, 5, 7],
                helper [2, 3, 5, 7], helper [11, 2, 3, 5, 7], helper [13, 11, 2, 3, 5, 7],
            ]
            ",
            RocList::from_slice(&[
                1, //
                5, //
                15, 15, //
                210, 210, 210, //
            ]),
            RocList<u16>
        )
    }

    #[test]
    #[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
    fn order_list_size_tests_issue_4732() {
        assert_evals_to!(
            r"
            helper : List U8 -> U8
            helper = \l -> when l is
                [1, ..]          -> 1
                [2, 1, ..]       -> 2
                [3, 2, 1, ..]    -> 3
                [4, 3, 2, 1, ..] -> 4
                [4, 3, 2, ..]    -> 5
                [4, 3, ..]       -> 6
                [4, ..]          -> 7
                _                -> 8

            [
                helper [1], helper [1, 2],

                helper [2, 1], helper [2, 1, 3],

                helper [3, 2, 1], helper [3, 2, 1, 4],

                helper [4, 3, 2, 1], helper [4, 3, 2, 1, 5],

                helper [4, 3, 2], helper [4, 3, 2, 5],

                helper [4, 3], helper [4, 3, 5],

                helper [4], helper [4, 5],

                helper [], helper [7],
            ]
            ",
            RocList::from_slice(&[
                1, 1, //
                2, 2, //
                3, 3, //
                4, 4, //
                5, 5, //
                6, 6, //
                7, 7, //
                8, 8, //
            ]),
            RocList<u8>
        )
    }

    #[test]
    #[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
    fn rest_as() {
        assert_evals_to!(
            r"
            helper : List U8 -> U8
            helper = \l -> when l is
                [1, .. as rest, 1] -> helper rest
                [1, .. as rest] -> helper rest
                [.. as rest, 1] -> helper rest
                [first, .., last] | [first as last] -> first + last
                [] -> 0
            [
                helper [1, 1, 1],
                helper [2, 1],
                helper [1, 1, 2, 4, 1],
                helper [1, 1, 8, 7, 3, 1, 1, 1],
            ]
            ",
            RocList::from_slice(&[0, 4, 6, 11]),
            RocList<u8>
        )
    }

    #[test]
    #[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
    fn list_str() {
        assert_evals_to!(
            r#"
            when ["d"] is
                [alone] -> [alone]
                other -> other
            "#,
            RocList::from_slice(&[RocStr::from("d")]),
            RocList<RocStr>
        )
    }
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_concat_utf8() {
    assert_evals_to!(
        r#"
        List.concat_utf8 [1, 2, 3, 4] "🐦"
        "#,
        RocList::from_slice(&[1u8, 2, 3, 4, 240, 159, 144, 166]),
        RocList<u8>
    )
}
