#[cfg(feature = "gen-wasm")]
use crate::helpers::{wasm::assert_refcounts, RefCount::*};

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
            Live(3), // s
            Live(1)  // result
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
        &[Deallocated]
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
            Live(3), // list
            Live(1)  // result
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
            Deallocated, // list
            Deallocated  // result
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
            Live(6), // s
            Live(2), // list
            Live(1)  // result
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
            Deallocated, // s
            Deallocated, // list
            Deallocated  // result
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
        &[Live(4)] // s
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
        &[Deallocated] // s
    );
}

#[test]
#[cfg(any(feature = "gen-wasm"))]
fn union_nonrecursive_inc() {
    type TwoStr = (RocStr, RocStr, i64);

    assert_refcounts!(
        indoc!(
            r#"
                TwoOrNone a: [Two a a, None]

                s = Str.concat "A long enough string " "to be heap-allocated"

                two : TwoOrNone Str
                two = Two s s

                four : TwoOrNone (TwoOrNone Str)
                four = Two two two

                four
            "#
        ),
        (TwoStr, TwoStr, i64),
        &[Live(4)]
    );
}

#[test]
#[cfg(any(feature = "gen-wasm"))]
fn union_nonrecursive_dec() {
    assert_refcounts!(
        indoc!(
            r#"
                TwoOrNone a: [Two a a, None]

                s = Str.concat "A long enough string " "to be heap-allocated"

                two : TwoOrNone Str
                two = Two s s

                when two is
                    Two x _ -> x
                    None -> ""
            "#
        ),
        RocStr,
        &[Live(1)] // s
    );
}

#[test]
#[cfg(any(feature = "gen-wasm"))]
fn union_recursive_inc() {
    assert_refcounts!(
        indoc!(
            r#"
                Expr : [Sym Str, Add Expr Expr]

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
            Live(4), // s
            Live(4), // sym
            Live(2), // e
        ]
    );
}

#[test]
#[cfg(any(feature = "gen-wasm"))]
fn union_recursive_dec() {
    assert_refcounts!(
        indoc!(
            r#"
                Expr : [Sym Str, Add Expr Expr]

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
            Live(1),     // s
            Live(1),     // sym
            Deallocated  // e
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
                Rose a : [Rose a (List (Rose a))]

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
            Live(2), // s
            Live(3), // i1
            Live(2), // s1
            Live(1), // [i1, i1]
            Live(1), // i2
            Live(1), // [s1, s1]
            Live(1)  // s2
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
                Rose a : [Rose a (List (Rose a))]

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
            Deallocated, // s
            Deallocated, // i1
            Deallocated, // s1
            Deallocated, // [i1, i1]
            Deallocated, // i2
            Deallocated, // [s1, s1]
            Deallocated, // s2
        ]
    );
}

#[test]
#[cfg(any(feature = "gen-wasm"))]
fn union_linked_list_inc() {
    assert_refcounts!(
        indoc!(
            r#"
                LinkedList a : [Nil, Cons a (LinkedList a)]

                s = Str.concat "A long enough string " "to be heap-allocated"

                linked : LinkedList Str
                linked = Cons s (Cons s (Cons s Nil))

                Tuple linked linked
            "#
        ),
        (Pointer, Pointer),
        &[
            Live(6), // s
            Live(2), // Cons
            Live(2), // Cons
            Live(2), // Cons
        ]
    );
}

#[test]
#[cfg(any(feature = "gen-wasm"))]
fn union_linked_list_dec() {
    assert_refcounts!(
        indoc!(
            r#"
                LinkedList a : [Nil, Cons a (LinkedList a)]

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
            Live(1),     // s
            Deallocated, // Cons
            Deallocated, // Cons
            Deallocated, // Cons
        ]
    );
}

#[test]
#[cfg(any(feature = "gen-wasm"))]
fn union_linked_list_nil_dec() {
    let no_refcounts: &[crate::helpers::RefCount] = &[];
    assert_refcounts!(
        indoc!(
            r#"
                LinkedList a : [Nil, Cons a (LinkedList a)]

                linked : LinkedList Str
                linked = Nil

                when linked is
                    Cons x _ -> x
                    Nil -> ""
            "#
        ),
        RocStr,
        no_refcounts
    );
}

#[test]
#[cfg(any(feature = "gen-wasm"))]
fn union_linked_list_long_dec() {
    assert_refcounts!(
        indoc!(
            r#"
                app "test" provides [main] to "./platform"

                LinkedList a : [Nil, Cons a (LinkedList a)]

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
        &[Deallocated; 1_000]
    );
}

#[test]
#[cfg(any(feature = "gen-wasm"))]
fn boxed_str_inc() {
    assert_refcounts!(
        indoc!(
            r#"
                s = Str.concat "A long enough string " "to be heap-allocated"
                b = Box.box s

                Tuple b b
            "#
        ),
        (Pointer, Pointer),
        &[
            Live(2), // s
            Live(2), // b
        ]
    );
}

#[test]
#[cfg(any(feature = "gen-wasm"))]
fn boxed_str_dec() {
    assert_refcounts!(
        indoc!(
            r#"
                s = Str.concat "A long enough string " "to be heap-allocated"
                b = Box.box s

                if Bool.false then
                    ReturnTheBox b
                else
                    DeallocateEverything
            "#
        ),
        (i32, i32),
        &[
            Deallocated, // s
            Deallocated, // b
        ]
    );
}
