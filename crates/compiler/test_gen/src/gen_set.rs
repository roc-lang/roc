#![cfg(feature = "gen-llvm")]

#[cfg(feature = "gen-llvm")]
use crate::helpers::llvm::assert_evals_to;

// #[cfg(feature = "gen-dev")]
// use crate::helpers::dev::assert_evals_to;

// #[cfg(feature = "gen-wasm")]
// use crate::helpers::wasm::assert_evals_to;

use indoc::indoc;
use roc_std::RocList;

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn empty_len() {
    assert_evals_to!(
        indoc!(
            r#"
            Set.len Set.empty
            "#
        ),
        0,
        usize
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn single_len() {
    assert_evals_to!(
        indoc!(
            r#"
            Set.len (Set.single 42)
            "#
        ),
        1,
        usize
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn single_to_list() {
    assert_evals_to!(
        indoc!(
            r#"
            Set.toList (Set.single 42)
            "#
        ),
        RocList::from_slice(&[42]),
        RocList<i64>
    );

    assert_evals_to!(
        indoc!(
            r#"
            Set.toList (Set.single 1)
            "#
        ),
        RocList::from_slice(&[1]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn insert() {
    assert_evals_to!(
        indoc!(
            r#"
            Set.empty
                |> Set.insert 0
                |> Set.insert 1
                |> Set.insert 2
                |> Set.toList
            "#
        ),
        RocList::from_slice(&[0, 1, 2]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn remove() {
    assert_evals_to!(
        indoc!(
            r#"
            Set.empty
                |> Set.insert 0
                |> Set.insert 1
                |> Set.remove 1
                |> Set.remove 2
                |> Set.toList
            "#
        ),
        RocList::from_slice(&[0]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn union() {
    assert_evals_to!(
        indoc!(
            r#"
            set1 : Set.Set I64
            set1 = Set.fromList [1,2]

            set2 : Set.Set I64
            set2 = Set.fromList [1,3,4]

            Set.union set1 set2
                |> Set.toList
            "#
        ),
        RocList::from_slice(&[1, 2, 3, 4]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn difference() {
    assert_evals_to!(
        indoc!(
            r#"
            set1 : Set.Set I64
            set1 = Set.fromList [1,2]

            set2 : Set.Set I64
            set2 = Set.fromList [1,3,4]

            Set.difference set1 set2
                |> Set.toList
            "#
        ),
        RocList::from_slice(&[2]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn intersection() {
    assert_evals_to!(
        indoc!(
            r#"
            set1 : Set.Set I64
            set1 = Set.fromList [1,2]

            set2 : Set.Set I64
            set2 = Set.fromList [1,3,4]

            Set.intersection set1 set2
                |> Set.toList
            "#
        ),
        RocList::from_slice(&[1]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn walk_sum() {
    assert_evals_to!(
        indoc!(
            r#"
            Set.walk (Set.fromList [1,2,3]) 0 (\x, y -> x + y)
            "#
        ),
        6,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn contains() {
    assert_evals_to!(
        indoc!(
            r#"
            Set.contains (Set.fromList [1,3,4]) 4
            "#
        ),
        true,
        bool
    );

    assert_evals_to!(
        indoc!(
            r#"
            Set.contains (Set.fromList [1,3,4]) 2
            "#
        ),
        false,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn from_list() {
    assert_evals_to!(
        indoc!(
            r#"
            [1,2,2,3,1,4]
                |> Set.fromList
                |> Set.toList
            "#
        ),
        RocList::from_slice(&[1, 2, 3, 4]),
        RocList<i64>
    );

    assert_evals_to!(
        indoc!(
            r#"
            empty : List I64
            empty = []

            empty
                |> Set.fromList
                |> Set.toList
            "#
        ),
        RocList::<i64>::default(),
        RocList<i64>
    );
}

#[test]
#[ignore]
#[cfg(any(feature = "gen-llvm"))]
fn from_list_void() {
    assert_evals_to!(
        indoc!(
            r#"
            []
                |> Set.fromList
                |> Set.toList
            "#
        ),
        RocList::<i64>::default(),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn to_list_empty() {
    assert_evals_to!(
        indoc!(
            r#"
            Set.toList Set.empty
            "#
        ),
        RocList::<std::convert::Infallible>::default(),
        RocList<std::convert::Infallible>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn from_list_result() {
    assert_evals_to!(
        indoc!(
            r#"
            x : Result Str {}
            x = Ok "foo"

            [x]
                |> Set.fromList
                |> Set.toList
                |> List.len
                |> Num.toI64
            "#
        ),
        1,
        i64
    );
}
