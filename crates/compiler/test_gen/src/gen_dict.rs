#![cfg(all(
    any(feature = "gen-llvm", feature = "gen-wasm"),
    not(debug_assertions) // https://github.com/roc-lang/roc/issues/3898
))]

#[cfg(feature = "gen-llvm")]
use crate::helpers::llvm::assert_evals_to;

// #[cfg(feature = "gen-dev")]
// use crate::helpers::dev::assert_evals_to;

#[cfg(feature = "gen-wasm")]
use crate::helpers::wasm::assert_evals_to;

use indoc::indoc;
use roc_std::{RocList, RocStr};

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn dict_empty_len() {
    assert_evals_to!(
        indoc!(
            r"
            Dict.len(Dict.empty())
            "
        ),
        0,
        u64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn dict_insert_empty() {
    assert_evals_to!(
        indoc!(
            r"
            Dict.empty()
                |> Dict.insert(42, 32)
                |> Dict.len
            "
        ),
        1,
        u64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn dict_empty_contains() {
    assert_evals_to!(
        indoc!(
            r"
            empty : Dict.Dict I64 F64
            empty = Dict.empty()

            Dict.contains(empty, 42)
            "
        ),
        false,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn dict_nonempty_contains() {
    assert_evals_to!(
        indoc!(
            r"
            empty : Dict.Dict I64 F64
            empty = Dict.insert(Dict.empty(), 42, 1.23)

            Dict.contains(empty, 42)
            "
        ),
        true,
        bool
    );
}

#[test]
#[ignore = "TODO figure out why this is broken with llvm wasm tests"]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn dict_empty_remove() {
    assert_evals_to!(
        indoc!(
            r"
            empty : Dict.Dict I64 F64
            empty = Dict.empty()

            empty
                |> Dict.remove(42)
                |> Dict.len
            "
        ),
        0,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn dict_nonempty_remove() {
    assert_evals_to!(
        indoc!(
            r"
            empty : Dict.Dict I64 F64
            empty = Dict.insert(Dict.empty(), 42, 1.23)

            empty
                |> Dict.remove(42)
                |> Dict.len
                |> Num.to_i64
            "
        ),
        0,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn dict_nonempty_get() {
    assert_evals_to!(
        indoc!(
            r"
            empty : Dict.Dict I64 F64
            empty = Dict.insert(Dict.empty(), 42, 1.23)

            with_default = |x, def|
                when x is
                    Ok(v) -> v
                    Err(_) -> def

            empty
                |> Dict.insert(42, 1.23f64)
                |> Dict.get(42)
                |> with_default(0)
            "
        ),
        1.23,
        f64
    );

    assert_evals_to!(
        indoc!(
            r"
            with_default = |x, def|
                when x is
                    Ok(v) -> v
                    Err(_) -> def

            Dict.empty()
                |> Dict.insert(42, 1.23f64)
                |> Dict.get(43)
                |> with_default(0)
            "
        ),
        0.0,
        f64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn keys() {
    assert_evals_to!(
        indoc!(
            r"
            my_dict : Dict.Dict I64 I64
            my_dict =
                Dict.empty()
                    |> Dict.insert(0, 100)
                    |> Dict.insert(1, 100)
                    |> Dict.insert(2, 100)


            Dict.keys my_dict
            "
        ),
        RocList::from_slice(&[0, 1, 2]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn values() {
    assert_evals_to!(
        indoc!(
            r"
            my_dict : Dict.Dict I64 I64
            my_dict =
                Dict.empty()
                    |> Dict.insert(0, 100)
                    |> Dict.insert(1, 200)
                    |> Dict.insert(2, 300)


            Dict.values(my_dict)
            "
        ),
        RocList::from_slice(&[100, 200, 300]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn from_list_with_fold_simple() {
    assert_evals_to!(
        indoc!(
            r"
            my_dict : Dict.Dict I64 I64
            my_dict =
                [1,2,3]
                |> List.walk(Dict.empty(), |accum, value| Dict.insert(accum, value, value))

            Dict.values(my_dict)
            "
        ),
        RocList::from_slice(&[1, 2, 3]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn from_list_with_fold_reallocates() {
    assert_evals_to!(
        indoc!(
            r"
            range : I64, I64, List I64-> List I64
            range = \low, high, accum ->
                if low < high then
                    range(low + 1, high, List.append(accum, low))
                else
                    accum

            my_dict : Dict.Dict I64 I64
            my_dict =
                # 25 elements (8 + 16 + 1) is guaranteed to overflow/reallocate at least twice
                range(0, 25, [])
                |> List.walk(Dict.empty(), |accum, value| Dict.insert(accum, value, value))

            Dict.values(my_dict)
            "
        ),
        RocList::from_slice(&[
            0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23,
            24
        ]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn small_str_keys() {
    assert_evals_to!(
        indoc!(
            r#"
            my_dict : Dict.Dict Str I64
            my_dict =
                Dict.empty()
                    |> Dict.insert("a", 100)

            Dict.keys my_dict
            "#
        ),
        RocList::from_slice(&["a".into()]),
        RocList<RocStr>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn big_str_keys() {
    assert_evals_to!(
        indoc!(
            r#"
            my_dict : Dict.Dict Str I64
            my_dict =
                Dict.empty()
                    |> Dict.insert("Leverage agile frameworks to provide a robust", 100)
                    |> Dict.insert("synopsis for high level overviews. Iterative approaches", 200)
                    |> Dict.insert("to corporate strategy foster collaborative thinking to", 300)

            Dict.keys(my_dict)
            "#
        ),
        RocList::from_slice(&[
            RocStr::from("Leverage agile frameworks to provide a robust"),
            RocStr::from("synopsis for high level overviews. Iterative approaches"),
            RocStr::from("to corporate strategy foster collaborative thinking to"),
        ]),
        RocList<RocStr>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn big_str_values() {
    assert_evals_to!(
        indoc!(
            r#"
            my_dict : Dict.Dict I64 Str
            my_dict =
                Dict.empty()
                    |> Dict.insert(100, "Leverage agile frameworks to provide a robust")
                    |> Dict.insert(200, "synopsis for high level overviews. Iterative approaches")
                    |> Dict.insert(300, "to corporate strategy foster collaborative thinking to")

            Dict.values(my_dict)
            "#
        ),
        RocList::from_slice(&[
            RocStr::from("Leverage agile frameworks to provide a robust"),
            RocStr::from("synopsis for high level overviews. Iterative approaches"),
            RocStr::from("to corporate strategy foster collaborative thinking to"),
        ]),
        RocList<RocStr>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn unit_values() {
    assert_evals_to!(
        indoc!(
            r"
            my_dict : Dict.Dict I64 {}
            my_dict =
                Dict.empty()
                    |> Dict.insert(0, {})
                    |> Dict.insert(1, {})
                    |> Dict.insert(2, {})
                    |> Dict.insert(3, {})

            Num.to_i64(Dict.len(my_dict))
            "
        ),
        4,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn single() {
    assert_evals_to!(
        indoc!(
            r"
            my_dict : Dict.Dict I64 {}
            my_dict =
                Dict.single 12345 {}

            Num.to_i64 (Dict.len my_dict)
            "
        ),
        1,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn insert_all() {
    assert_evals_to!(
        indoc!(
            r"
            my_dict : Dict I64 {}
            my_dict =
                Dict.insert_all (Dict.single 0 {}) (Dict.single 1 {})

            Dict.len my_dict
                |> Num.to_i64
            "
        ),
        2,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn insert_all_prefer_second() {
    assert_evals_to!(
        indoc!(
            r"
            my_dict : Dict.Dict I64 I64
            my_dict =
                (Dict.single 0 100)
                    |> Dict.insert_all (Dict.single 0 200)

            Dict.values my_dict
            "
        ),
        RocList::from_slice(&[200]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn keep_shared() {
    assert_evals_to!(
        indoc!(
            r"
            dict1 : Dict.Dict I64 {}
            dict1 =
                Dict.empty()
                    |> Dict.insert(1, {})
                    |> Dict.insert(2, {})
                    |> Dict.insert(3, {})
                    |> Dict.insert(4, {})
                    |> Dict.insert(5, {})

            dict2 : Dict.Dict I64 {}
            dict2 =
                Dict.empty()
                    |> Dict.insert(0, {})
                    |> Dict.insert(2, {})
                    |> Dict.insert(4, {})

            Dict.keep_shared(dict1, dict2)
                |> Dict.len
                |> Num.to_i64
            "
        ),
        2,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn keep_shared_value_must_match() {
    assert_evals_to!(
        indoc!(
            r"
            dict1 : Dict.Dict I64 I64
            dict1 =
                Dict.empty()
                    |> Dict.insert(1, 1)
                    |> Dict.insert(2, 2)
                    |> Dict.insert(3, 3)
                    |> Dict.insert(4, 4)
                    |> Dict.insert(5, 5)

            dict2 : Dict.Dict I64 I64
            dict2 =
                Dict.empty()
                    |> Dict.insert(0, 100)
                    |> Dict.insert(2, 2)
                    |> Dict.insert(4, 300)

            Dict.keep_shared(dict1, dict2)
                |> Dict.values
            "
        ),
        RocList::from_slice(&[2]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn remove_all() {
    assert_evals_to!(
        indoc!(
            r"
            dict1 : Dict.Dict I64 {}
            dict1 =
                Dict.empty()
                    |> Dict.insert(1, {})
                    |> Dict.insert(2, {})
                    |> Dict.insert(3, {})
                    |> Dict.insert(4, {})
                    |> Dict.insert(5, {})

            dict2 : Dict.Dict I64 {}
            dict2 =
                Dict.empty()
                    |> Dict.insert(0, {})
                    |> Dict.insert(2, {})
                    |> Dict.insert(4, {})

            Dict.remove_all(dict1, dict2)
                |> Dict.len
                |> Num.to_i64
            "
        ),
        3,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn remove_all_prefer_first() {
    assert_evals_to!(
        indoc!(
            r"
            dict1 : Dict.Dict I64 I64
            dict1 =
                Dict.empty()
                    |> Dict.insert(1, 1)
                    |> Dict.insert(2, 2)
                    |> Dict.insert(3, 3)
                    |> Dict.insert(4, 4)
                    |> Dict.insert(5, 5)

            dict2 : Dict.Dict I64 I64
            dict2 =
                Dict.empty()
                    |> Dict.insert(0, 100)
                    |> Dict.insert(2, 200)
                    |> Dict.insert(4, 300)

            Dict.remove_all(dict1, dict2)
                |> Dict.values
            "
        ),
        RocList::from_slice(&[1, 5, 3]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn walk_sum_keys() {
    assert_evals_to!(
        indoc!(
            r"
            dict1 : Dict.Dict I64 I64
            dict1 =
                Dict.empty()
                    |> Dict.insert(1, 1)
                    |> Dict.insert(2, 2)
                    |> Dict.insert(3, 3)
                    |> Dict.insert(4, 4)
                    |> Dict.insert(5, 5)

            Dict.walk(dict1, 0, |k, _, a| k + a)
            "
        ),
        15,
        i64
    );
}
