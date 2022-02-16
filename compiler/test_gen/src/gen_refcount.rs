#[cfg(feature = "gen-wasm")]
use crate::helpers::wasm::assert_refcounts;

#[allow(unused_imports)]
use indoc::indoc;

#[allow(unused_imports)]
use roc_std::{RocList, RocStr};

// A "good enough" representation of a pointer for these tests, because
// we ignore the return value. As long as it's the right stack size, it's fine.
#[allow(dead_code)]
type Pointer = usize;

#[test]
#[cfg(any(feature = "gen-wasm"))]
fn str_inc() {
    assert_refcounts!(
        indoc!(
            r#"
                s = Str.concat "A long enough string " "to be heap-allocated"

                [s, s, s]
            "#
        ),
        RocList<RocStr>,
        &[
            3, // s
            1  // result
        ]
    );
}

#[test]
#[cfg(any(feature = "gen-wasm"))]
fn str_dealloc() {
    assert_refcounts!(
        indoc!(
            r#"
                s = Str.concat "A long enough string " "to be heap-allocated"

                Str.isEmpty s
            "#
        ),
        bool,
        &[0]
    );
}

#[test]
#[cfg(any(feature = "gen-wasm"))]
fn list_int_inc() {
    assert_refcounts!(
        indoc!(
            r#"
                list = [0x111, 0x222, 0x333]
                [list, list, list]
            "#
        ),
        RocList<RocList<i64>>,
        &[
            // TODO be smarter about coalescing polymorphic list values
            1, // list0
            1, // list1
            1, // list2
            1  // result
        ]
    );
}

#[test]
#[cfg(any(feature = "gen-wasm"))]
fn list_int_dealloc() {
    assert_refcounts!(
        indoc!(
            r#"
                list = [0x111, 0x222, 0x333]
                List.len [list, list, list]
            "#
        ),
        usize,
        &[
            // TODO be smarter about coalescing polymorphic list values
            0, // list0
            0, // list1
            0, // list2
            0  // result
        ]
    );
}

#[test]
#[cfg(any(feature = "gen-wasm"))]
fn list_str_inc() {
    assert_refcounts!(
        indoc!(
            r#"
                s = Str.concat "A long enough string " "to be heap-allocated"
                list = [s, s, s]
                [list, list]
            "#
        ),
        RocList<RocList<RocStr>>,
        &[
            6, // s
            2, // list
            1  // result
        ]
    );
}

#[test]
#[cfg(any(feature = "gen-wasm"))]
fn list_str_dealloc() {
    assert_refcounts!(
        indoc!(
            r#"
                s = Str.concat "A long enough string " "to be heap-allocated"
                list = [s, s, s]
                List.len [list, list]
            "#
        ),
        usize,
        &[
            0, // s
            0, // list
            0  // result
        ]
    );
}

#[test]
#[cfg(any(feature = "gen-wasm"))]
fn struct_inc() {
    assert_refcounts!(
        indoc!(
            r#"
                s = Str.concat "A long enough string " "to be heap-allocated"
                r1 : { a: I64, b: Str, c: Str }
                r1 = { a: 123, b: s, c: s }
                { y: r1, z: r1 }
            "#
        ),
        [(i64, RocStr, RocStr); 2],
        &[4] // s
    );
}

#[test]
#[cfg(any(feature = "gen-wasm"))]
fn struct_dealloc() {
    assert_refcounts!(
        indoc!(
            r#"
            s = Str.concat "A long enough string " "to be heap-allocated"
            r1 : { a: I64, b: Str, c: Str }
            r1 = { a: 123, b: s, c: s }
            r2 = { x: 456, y: r1, z: r1 }
            r2.x
    "#
        ),
        i64,
        &[0] // s
    );
}

#[test]
#[cfg(any(feature = "gen-wasm"))]
fn union_nonrecursive_inc() {
    type TwoStr = (RocStr, RocStr, i64);

    assert_refcounts!(
        indoc!(
            r#"
                TwoOrNone a: [ Two a a, None ]

                s = Str.concat "A long enough string " "to be heap-allocated"

                two : TwoOrNone Str
                two = Two s s

                four : TwoOrNone (TwoOrNone Str)
                four = Two two two

                four
            "#
        ),
        (TwoStr, TwoStr, i64),
        &[4]
    );
}

#[test]
#[cfg(any(feature = "gen-wasm"))]
fn union_nonrecursive_dec() {
    assert_refcounts!(
        indoc!(
            r#"
                TwoOrNone a: [ Two a a, None ]

                s = Str.concat "A long enough string " "to be heap-allocated"

                two : TwoOrNone Str
                two = Two s s

                when two is
                    Two x _ -> x
                    None -> ""
            "#
        ),
        RocStr,
        &[1] // s
    );
}

#[test]
#[cfg(any(feature = "gen-wasm"))]
fn union_recursive_inc() {
    assert_refcounts!(
        indoc!(
            r#"
                Expr : [ Sym Str, Add Expr Expr ]

                s = Str.concat "heap_allocated" "_symbol_name"

                x : Expr
                x = Sym s

                e : Expr
                e = Add x x

                Pair e e
            "#
        ),
        (Pointer, Pointer),
        &[
            4, // s
            4, // sym
            2, // e
        ]
    );
}

#[test]
#[cfg(any(feature = "gen-wasm"))]
fn union_recursive_dec() {
    assert_refcounts!(
        indoc!(
            r#"
                Expr : [ Sym Str, Add Expr Expr ]

                s = Str.concat "heap_allocated" "_symbol_name"

                x : Expr
                x = Sym s

                e : Expr
                e = Add x x

                when e is
                    Add y _ -> y
                    Sym _ -> e
            "#
        ),
        Pointer,
        &[
            1, // s
            1, // sym
            0  // e
        ]
    );
}

#[test]
#[cfg(any(feature = "gen-wasm"))]
fn refcount_different_rosetrees_inc() {
    // Requires two different Inc procedures for `List (Rose I64)` and `List (Rose Str)`
    // even though both appear in the mono Layout as `List(RecursivePointer)`
    assert_refcounts!(
        indoc!(
            r#"
                Rose a : [ Rose a (List (Rose a)) ]

                s = Str.concat "A long enough string " "to be heap-allocated"

                i1 : Rose I64
                i1 = Rose 999 []

                s1 : Rose Str
                s1 = Rose s []

                i2 : Rose I64
                i2 = Rose 0 [i1, i1, i1]

                s2 : Rose Str
                s2 = Rose "" [s1, s1]

                Tuple i2 s2
        "#
        ),
        (Pointer, Pointer),
        &[
            2, // s
            3, // i1
            2, // s1
            1, // [i1, i1]
            1, // i2
            1, // [s1, s1]
            1  // s2
        ]
    );
}

#[test]
#[cfg(any(feature = "gen-wasm"))]
fn refcount_different_rosetrees_dec() {
    // Requires two different Dec procedures for `List (Rose I64)` and `List (Rose Str)`
    // even though both appear in the mono Layout as `List(RecursivePointer)`
    assert_refcounts!(
        indoc!(
            r#"
                Rose a : [ Rose a (List (Rose a)) ]

                s = Str.concat "A long enough string " "to be heap-allocated"

                i1 : Rose I64
                i1 = Rose 999 []

                s1 : Rose Str
                s1 = Rose s []

                i2 : Rose I64
                i2 = Rose 0 [i1, i1]

                s2 : Rose Str
                s2 = Rose "" [s1, s1]

                when (Tuple i2 s2) is
                    Tuple (Rose x _) _ -> x
        "#
        ),
        i64,
        &[
            0, // s
            0, // i1
            0, // s1
            0, // [i1, i1]
            0, // i2
            0, // [s1, s1]
            0, // s2
        ]
    );
}

#[test]
#[cfg(any(feature = "gen-wasm"))]
fn union_linked_list_inc() {
    assert_refcounts!(
        indoc!(
            r#"
                LinkedList a : [ Nil, Cons a (LinkedList a) ]

                s = Str.concat "A long enough string " "to be heap-allocated"

                linked : LinkedList Str
                linked = Cons s (Cons s (Cons s Nil))

                Tuple linked linked
            "#
        ),
        (Pointer, Pointer),
        &[
            6, // s
            2, // Cons
            2, // Cons
            2, // Cons
        ]
    );
}

#[test]
#[cfg(any(feature = "gen-wasm"))]
fn union_linked_list_dec() {
    assert_refcounts!(
        indoc!(
            r#"
                LinkedList a : [ Nil, Cons a (LinkedList a) ]

                s = Str.concat "A long enough string " "to be heap-allocated"

                linked : LinkedList Str
                linked = Cons s (Cons s (Cons s Nil))

                when linked is
                    Cons x _ -> x
                    Nil -> ""
            "#
        ),
        RocStr,
        &[
            1, // s
            0, // Cons
            0, // Cons
            0, // Cons
        ]
    );
}

#[test]
#[cfg(any(feature = "gen-wasm"))]
fn union_linked_list_long_dec() {
    assert_refcounts!(
        indoc!(
            r#"
                app "test" provides [ main ] to "./platform"

                LinkedList a : [ Nil, Cons a (LinkedList a) ]

                prependOnes = \n, tail ->
                    if n == 0 then
                        tail
                    else
                        prependOnes (n-1) (Cons 1 tail)

                main =
                    n = 1_000

                    linked : LinkedList I64
                    linked = prependOnes n Nil

                    when linked is
                        Cons x _ -> x
                        Nil -> -1
                "#
        ),
        i64,
        &[0; 1_000]
    );
}
