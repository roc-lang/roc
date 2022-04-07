#![cfg(feature = "gen-llvm")]

#[cfg(feature = "gen-llvm")]
use crate::helpers::llvm::assert_evals_to;

// #[cfg(feature = "gen-dev")]
// use crate::helpers::dev::assert_evals_to;

// #[cfg(feature = "gen-wasm")]
// use crate::helpers::wasm::assert_evals_to;

use indoc::indoc;
use roc_std::{RocList, RocStr};

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn dict_empty_len() {
    assert_evals_to!(
        indoc!(
            r#"
                Dict.len Dict.empty
            "#
        ),
        0,
        usize
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn dict_insert_empty() {
    assert_evals_to!(
        indoc!(
            r#"
            Dict.insert Dict.empty 42 32
                |> Dict.len
            "#
        ),
        1,
        usize
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn dict_empty_contains() {
    assert_evals_to!(
        indoc!(
            r#"
            empty : Dict I64 F64
            empty = Dict.empty

            Dict.contains empty 42
            "#
        ),
        false,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn dict_nonempty_contains() {
    assert_evals_to!(
        indoc!(
            r#"
            empty : Dict I64 F64
            empty = Dict.insert Dict.empty 42 3.14

            Dict.contains empty 42
            "#
        ),
        true,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn dict_empty_remove() {
    assert_evals_to!(
        indoc!(
            r#"
            empty : Dict I64 F64
            empty = Dict.empty

            empty
                |> Dict.remove 42
                |> Dict.len
            "#
        ),
        0,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn dict_nonempty_remove() {
    assert_evals_to!(
        indoc!(
            r#"
            empty : Dict I64 F64
            empty = Dict.insert Dict.empty 42 3.14

            empty
                |> Dict.remove 42
                |> Dict.len
            "#
        ),
        0,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn dict_nonempty_get() {
    assert_evals_to!(
        indoc!(
            r#"
            empty : Dict I64 F64
            empty = Dict.insert Dict.empty 42 3.14

            withDefault = \x, def ->
                when  x is
                    Ok v -> v
                    Err _ -> def

            empty
                |> Dict.insert 42 3.14
                |> Dict.get 42
                |> withDefault 0
            "#
        ),
        3.14,
        f64
    );

    assert_evals_to!(
        indoc!(
            r#"
            withDefault = \x, def ->
                when  x is
                    Ok v -> v
                    Err _ -> def

            Dict.empty
                |> Dict.insert 42 3.14
                |> Dict.get 43
                |> withDefault 0
            "#
        ),
        0.0,
        f64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn keys() {
    assert_evals_to!(
        indoc!(
            r#"
            myDict : Dict I64 I64
            myDict =
                Dict.empty
                    |> Dict.insert 0 100
                    |> Dict.insert 1 100
                    |> Dict.insert 2 100


            Dict.keys myDict
            "#
        ),
        RocList::from_slice(&[0, 1, 2]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn values() {
    assert_evals_to!(
        indoc!(
            r#"
            myDict : Dict I64 I64
            myDict =
                Dict.empty
                    |> Dict.insert 0 100
                    |> Dict.insert 1 200
                    |> Dict.insert 2 300


            Dict.values myDict
            "#
        ),
        RocList::from_slice(&[100, 200, 300]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn from_list_with_fold_simple() {
    assert_evals_to!(
        indoc!(
            r#"
            myDict : Dict I64 I64
            myDict =
                [1,2,3]
                    |> List.walk Dict.empty (\accum, value -> Dict.insert accum value value)

            Dict.values myDict
            "#
        ),
        RocList::from_slice(&[2, 3, 1]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn from_list_with_fold_reallocates() {
    assert_evals_to!(
        indoc!(
            r#"
            range : I64, I64, List I64-> List I64
            range = \low, high, accum ->
                if low < high then
                    range (low + 1) high (List.append accum low)
                else
                    accum

            myDict : Dict I64 I64
            myDict =
                # 25 elements (8 + 16 + 1) is guaranteed to overflow/reallocate at least twice
                range 0 25 []
                    |> List.walk Dict.empty (\accum, value -> Dict.insert accum value value)

            Dict.values myDict
            "#
        ),
        RocList::from_slice(&[
            4, 5, 20, 0, 7, 3, 1, 21, 10, 6, 13, 9, 14, 19, 2, 15, 12, 17, 16, 18, 22, 8, 11, 24,
            23
        ]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn small_str_keys() {
    assert_evals_to!(
        indoc!(
            r#"
            myDict : Dict Str I64
            myDict =
                Dict.empty
                    |> Dict.insert "a" 100
                    |> Dict.insert "b" 100
                    |> Dict.insert "c" 100


            Dict.keys myDict
            "#
        ),
        RocList::from_slice(&[RocStr::from("c"), RocStr::from("a"), RocStr::from("b"),],),
        RocList<RocStr>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn big_str_keys() {
    assert_evals_to!(
        indoc!(
            r#"
            myDict : Dict Str I64
            myDict =
                Dict.empty
                    |> Dict.insert "Leverage agile frameworks to provide a robust" 100
                    |> Dict.insert "synopsis for high level overviews. Iterative approaches" 200
                    |> Dict.insert "to corporate strategy foster collaborative thinking to" 300

            Dict.keys myDict
            "#
        ),
        RocList::from_slice(&[
            RocStr::from("Leverage agile frameworks to provide a robust"),
            RocStr::from("to corporate strategy foster collaborative thinking to"),
            RocStr::from("synopsis for high level overviews. Iterative approaches"),
        ]),
        RocList<RocStr>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn big_str_values() {
    assert_evals_to!(
        indoc!(
            r#"
            myDict : Dict I64 Str
            myDict =
                Dict.empty
                    |> Dict.insert 100 "Leverage agile frameworks to provide a robust"
                    |> Dict.insert 200 "synopsis for high level overviews. Iterative approaches"
                    |> Dict.insert 300 "to corporate strategy foster collaborative thinking to"

            Dict.values myDict
            "#
        ),
        RocList::from_slice(&[
            RocStr::from("Leverage agile frameworks to provide a robust"),
            RocStr::from("to corporate strategy foster collaborative thinking to"),
            RocStr::from("synopsis for high level overviews. Iterative approaches"),
        ]),
        RocList<RocStr>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn unit_values() {
    assert_evals_to!(
        indoc!(
            r#"
            myDict : Dict I64 {}
            myDict =
                Dict.empty
                    |> Dict.insert 0 {}
                    |> Dict.insert 1 {}
                    |> Dict.insert 2 {}
                    |> Dict.insert 3 {}

            Dict.len myDict
            "#
        ),
        4,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn single() {
    assert_evals_to!(
        indoc!(
            r#"
            myDict : Dict I64 {}
            myDict =
                Dict.single 0 {}

            Dict.len myDict
            "#
        ),
        1,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn union() {
    assert_evals_to!(
        indoc!(
            r#"
            myDict : Dict I64 {}
            myDict =
                Dict.union (Dict.single 0 {}) (Dict.single 1 {})

            Dict.len myDict
            "#
        ),
        2,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn union_prefer_first() {
    assert_evals_to!(
        indoc!(
            r#"
            myDict : Dict I64 I64
            myDict =
                Dict.union (Dict.single 0 100) (Dict.single 0 200)

            Dict.values myDict
            "#
        ),
        RocList::from_slice(&[100]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn intersection() {
    assert_evals_to!(
        indoc!(
            r#"
            dict1 : Dict I64 {}
            dict1 =
                Dict.empty
                    |> Dict.insert 1 {}
                    |> Dict.insert 2 {}
                    |> Dict.insert 3 {}
                    |> Dict.insert 4 {}
                    |> Dict.insert 5 {}

            dict2 : Dict I64 {}
            dict2 =
                Dict.empty
                    |> Dict.insert 0 {}
                    |> Dict.insert 2 {}
                    |> Dict.insert 4 {}

            Dict.intersection dict1 dict2
                |> Dict.len
            "#
        ),
        2,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn intersection_prefer_first() {
    assert_evals_to!(
        indoc!(
            r#"
            dict1 : Dict I64 I64
            dict1 =
                Dict.empty
                    |> Dict.insert 1 1
                    |> Dict.insert 2 2
                    |> Dict.insert 3 3
                    |> Dict.insert 4 4
                    |> Dict.insert 5 5

            dict2 : Dict I64 I64
            dict2 =
                Dict.empty
                    |> Dict.insert 0 100
                    |> Dict.insert 2 200
                    |> Dict.insert 4 300

            Dict.intersection dict1 dict2
                |> Dict.values
            "#
        ),
        RocList::from_slice(&[4, 2]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn difference() {
    assert_evals_to!(
        indoc!(
            r#"
            dict1 : Dict I64 {}
            dict1 =
                Dict.empty
                    |> Dict.insert 1 {}
                    |> Dict.insert 2 {}
                    |> Dict.insert 3 {}
                    |> Dict.insert 4 {}
                    |> Dict.insert 5 {}

            dict2 : Dict I64 {}
            dict2 =
                Dict.empty
                    |> Dict.insert 0 {}
                    |> Dict.insert 2 {}
                    |> Dict.insert 4 {}

            Dict.difference dict1 dict2
                |> Dict.len
            "#
        ),
        3,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn difference_prefer_first() {
    assert_evals_to!(
        indoc!(
            r#"
            dict1 : Dict I64 I64
            dict1 =
                Dict.empty
                    |> Dict.insert 1 1
                    |> Dict.insert 2 2
                    |> Dict.insert 3 3
                    |> Dict.insert 4 4
                    |> Dict.insert 5 5

            dict2 : Dict I64 I64
            dict2 =
                Dict.empty
                    |> Dict.insert 0 100
                    |> Dict.insert 2 200
                    |> Dict.insert 4 300

            Dict.difference dict1 dict2
                |> Dict.values
            "#
        ),
        RocList::from_slice(&[5, 3, 1]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn walk_sum_keys() {
    assert_evals_to!(
        indoc!(
            r#"
            dict1 : Dict I64 I64
            dict1 =
                Dict.empty
                    |> Dict.insert 1 1
                    |> Dict.insert 2 2
                    |> Dict.insert 3 3
                    |> Dict.insert 4 4
                    |> Dict.insert 5 5

            Dict.walk dict1 0 \k, _, a -> k + a
            "#
        ),
        15,
        i64
    );
}
