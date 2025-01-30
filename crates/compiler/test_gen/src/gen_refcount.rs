#[cfg(feature = "gen-wasm")]
use crate::helpers::{wasm::assert_refcounts, RefCount::*, RefCountLoc::*};

#[allow(unused_imports)]
use indoc::indoc;

#[allow(unused_imports)]
use roc_std::{RocList, RocStr};

// A "good enough" representation of a pointer for these tests, because
// we ignore the return value. As long as it's the right stack size, it's fine.
#[allow(dead_code)]
type Pointer = usize;

#[test]
#[cfg(feature = "gen-wasm")]
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
            (StandardRC, Live(3)), // s
            (AfterSize, Live(1))   // result
        ]
    );
}

#[test]
#[cfg(feature = "gen-wasm")]
fn str_dealloc() {
    assert_refcounts!(
        indoc!(
            r#"
                s = Str.concat "A long enough string " "to be heap-allocated"

                Str.is_empty s
            "#
        ),
        bool,
        &[(StandardRC, Deallocated)]
    );
}

#[test]
#[cfg(feature = "gen-wasm")]
fn str_to_utf8() {
    assert_refcounts!(
        indoc!(
            r#"
                s = Str.concat "A long enough string " "to be heap-allocated"

                Str.to_utf8 s
            "#
        ),
        RocStr,
        &[
            (StandardRC, Live(1)), // s
        ]
    );
}

#[test]
#[cfg(feature = "gen-wasm")]
fn str_from_utf8() {
    assert_refcounts!(
        indoc!(
            r#"
                s = Str.concat "A long enough string " "to be heap-allocated"

                Str.to_utf8 s
                |> Str.from_utf8
            "#
        ),
        RocStr,
        &[
            (StandardRC, Live(1)), // s
        ]
    );
}

#[test]
#[cfg(feature = "gen-wasm")]
fn str_to_utf8_dealloc() {
    assert_refcounts!(
        indoc!(
            r#"
                s = Str.concat "A long enough string " "to be heap-allocated"

                Str.to_utf8 s
                |> List.len
            "#
        ),
        i64,
        &[
            (StandardRC, Deallocated), // s
        ]
    );
}

#[test]
#[cfg(feature = "gen-wasm")]
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
            (StandardRC, Live(3)), // list
            (AfterSize, Live(1))   // result
        ]
    );
}

#[test]
#[cfg(feature = "gen-wasm")]
fn list_int_dealloc() {
    assert_refcounts!(
        indoc!(
            r#"
                list = [0x111, 0x222, 0x333]
                List.len [list, list, list]
            "#
        ),
        u64,
        &[
            (StandardRC, Deallocated), // list
            (StandardRC, Deallocated)  // result
        ]
    );
}

#[test]
#[cfg(feature = "gen-wasm")]
fn list_str() {
    assert_refcounts!(
        indoc!(
            r#"
                s = Str.concat "A long enough string " "to be heap-allocated"
                [s, s, s]
            "#
        ),
        RocList<RocStr>,
        &[
            (StandardRC, Live(3)), // s
            (AfterSize, Live(1)),  // Result
        ]
    );
}

#[test]
#[cfg(feature = "gen-wasm")]
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
            (StandardRC, Live(3)), // s
            (AfterSize, Live(2)),  // list
            (AfterSize, Live(1))   // result
        ]
    );
}

#[test]
#[cfg(feature = "gen-wasm")]
fn list_str_drop_first() {
    assert_refcounts!(
        indoc!(
            r#"
                s = Str.concat "A long enough string " "to be heap-allocated"
                list = [s, s, s]
                List.drop_first list 1
            "#
        ),
        RocList<RocList<RocStr>>,
        &[
            // Still has 3 refcounts cause the slice holds onto the list.
            // So nothing in the list is freed yet.
            (StandardRC, Live(3)), // s
            (AfterSize, Live(1))   // result
        ]
    );
}

#[test]
#[cfg(feature = "gen-wasm")]
fn list_str_take_first() {
    assert_refcounts!(
        indoc!(
            r#"
                s = Str.concat "A long enough string " "to be heap-allocated"
                list = [s, s, s]
                List.take_first list 1
            "#
        ),
        RocList<RocList<RocStr>>,
        &[
            // Take will free tail of a unique list.
            (StandardRC, Live(1)), // s
            (AfterSize, Live(1))   // result
        ]
    );
}

#[test]
#[cfg(feature = "gen-wasm")]
fn list_str_split_on() {
    assert_refcounts!(
        indoc!(
            r#"
                s = Str.concat "A long enough string " "to be heap-allocated"
                list = [s, s, s]
                List.split_at list 1
            "#
        ),
        (RocList<RocStr>, RocList<RocStr>),
        &[
            (StandardRC, Live(3)), // s
            (AfterSize, Live(2)),  // list
        ]
    );
}

#[test]
#[cfg(feature = "gen-wasm")]
fn list_str_split_on_zero() {
    assert_refcounts!(
        indoc!(
            r#"
                s = Str.concat "A long enough string " "to be heap-allocated"
                list = [s, s, s]
                List.split_at list 0
            "#
        ),
        (RocList<RocStr>, RocList<RocStr>),
        &[
            (StandardRC, Live(3)), // s
            (AfterSize, Live(1)),  // list
        ]
    );
}

#[test]
#[cfg(feature = "gen-wasm")]
fn list_get() {
    assert_refcounts!(
        indoc!(
            r#"
                s = Str.concat "A long enough string " "to be heap-allocated"
                i1 = [s, s, s]
                List.get i1 1
                |> Result.with_default ""
            "#
        ),
        RocStr,
        &[
            (StandardRC, Live(1)),    // s
            (AfterSize, Deallocated), // i1
        ]
    );
}

#[test]
#[cfg(feature = "gen-wasm")]
fn list_map() {
    assert_refcounts!(
        indoc!(
            r#"
                s = Str.concat "A long enough string " "to be heap-allocated"
                i1 = [s, s, s]
                List.map i1 Str.to_utf8
            "#
        ),
        RocList<RocStr>,
        &[
            (StandardRC, Live(3)),    // s
            (AfterSize, Deallocated), // i1
            (AfterSize, Live(1)),     // Result
        ]
    );
}

#[test]
#[cfg(feature = "gen-wasm")]
fn list_map_dealloc() {
    assert_refcounts!(
        indoc!(
            r#"
                s = Str.concat "A long enough string " "to be heap-allocated"
                i1 = [s, s, s]
                List.map i1 Str.to_utf8
                |> List.len
            "#
        ),
        i64,
        &[
            (StandardRC, Deallocated), // s
            (AfterSize, Deallocated),  // i1
            (AfterSize, Deallocated),  // Result
        ]
    );
}

#[test]
#[cfg(feature = "gen-wasm")]
fn list_map2_dealloc_tail() {
    assert_refcounts!(
        indoc!(
            r#"
                s = Str.concat "A long enough string " "to be heap-allocated"
                i1 = [s, s, s]
                i2 = [1i32, 2]
                List.map2 i1 i2 \a, b -> (a, b)
            "#
        ),
        RocList<(RocStr, i64)>,
        &[
            (StandardRC, Live(2)),     // s
            (AfterSize, Deallocated),  // i1
            (StandardRC, Deallocated), // i2
            (AfterSize, Live(1)),      // Result
        ]
    );
}

#[test]
#[cfg(feature = "gen-wasm")]
fn list_map2_dealloc() {
    assert_refcounts!(
        indoc!(
            r#"
                s = Str.concat "A long enough string " "to be heap-allocated"
                i1 = [s, s, s]
                List.map2 i1 i1 \a, b -> (a, b)
                |> List.len
            "#
        ),
        i64,
        &[
            (StandardRC, Deallocated), // s
            (AfterSize, Deallocated),  // i1
            (AfterSize, Deallocated),  // Result
        ]
    );
}

#[test]
#[cfg(feature = "gen-wasm")]
fn list_str_dealloc() {
    assert_refcounts!(
        indoc!(
            r#"
                s = Str.concat "A long enough string " "to be heap-allocated"
                list = [s, s, s]
                List.len [list, list]
            "#
        ),
        u64,
        &[
            (StandardRC, Deallocated), // s
            (AfterSize, Deallocated),  // list
            (AfterSize, Deallocated)   // result
        ]
    );
}

#[test]
#[cfg(feature = "gen-wasm")]
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
        &[(StandardRC, Live(4))] // s
    );
}

#[test]
#[cfg(feature = "gen-wasm")]
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
        &[(StandardRC, Deallocated)] // s
    );
}

#[test]
#[cfg(feature = "gen-wasm")]
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
        &[(StandardRC, Live(4))]
    );
}

#[test]
#[cfg(feature = "gen-wasm")]
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
        &[(StandardRC, Live(1))] // s
    );
}

#[test]
#[cfg(feature = "gen-wasm")]
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
            (StandardRC, Live(1)), // s
            (StandardRC, Live(2)), // x
            (StandardRC, Live(2)), // e
        ]
    );
}

#[test]
#[cfg(feature = "gen-wasm")]
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
            (StandardRC, Live(1)),     // s
            (StandardRC, Live(1)),     // sym
            (StandardRC, Deallocated)  // e
        ]
    );
}

#[test]
#[cfg(feature = "gen-wasm")]
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
            (StandardRC, Live(1)), // s
            (StandardRC, Live(3)), // i1
            (StandardRC, Live(2)), // s1
            (AfterSize, Live(1)),  // [i1, i1]
            (StandardRC, Live(1)), // i2
            (AfterSize, Live(1)),  // [s1, s1]
            (StandardRC, Live(1))  // s2
        ]
    );
}

#[test]
#[cfg(feature = "gen-wasm")]
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
            (StandardRC, Deallocated), // s
            (StandardRC, Deallocated), // i1
            (StandardRC, Deallocated), // s1
            (StandardRC, Deallocated), // [i1, i1]
            (StandardRC, Deallocated), // i2
            (StandardRC, Deallocated), // [s1, s1]
            (StandardRC, Deallocated), // s2
        ]
    );
}

#[test]
#[cfg(feature = "gen-wasm")]
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
            (StandardRC, Live(3)), // s
            (StandardRC, Live(1)), // inner-most Cons
            (StandardRC, Live(1)), // middle Cons
            (StandardRC, Live(2)), // linked
        ]
    );
}

#[test]
#[cfg(feature = "gen-wasm")]
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
            (StandardRC, Live(1)),     // s
            (StandardRC, Deallocated), // Cons
            (StandardRC, Deallocated), // Cons
            (StandardRC, Deallocated), // Cons
        ]
    );
}

#[test]
#[cfg(feature = "gen-wasm")]
fn union_linked_list_nil_dec() {
    let no_refcounts: &[(crate::helpers::RefCountLoc, crate::helpers::RefCount)] = &[];
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
#[cfg(feature = "gen-wasm")]
fn union_linked_list_long_dec() {
    assert_refcounts!(
        indoc!(
            r#"
                app "test" provides [main] to "./platform"

                LinkedList a : [Nil, Cons a (LinkedList a)]

                prepend_ones = \n, tail ->
                    if n == 0 then
                        tail
                    else
                        prepend_ones (n-1) (Cons 1 tail)

                main =
                    n = 1_000

                    linked : LinkedList I64
                    linked = prepend_ones n Nil

                    when linked is
                        Cons x _ -> x
                        Nil -> -1
                "#
        ),
        i64,
        &[(StandardRC, Deallocated); 1_000]
    );
}

#[test]
#[cfg(feature = "gen-wasm")]
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
            (StandardRC, Live(1)), // s
            (StandardRC, Live(2)), // b
        ]
    );
}

#[test]
#[cfg(feature = "gen-wasm")]
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
            (StandardRC, Deallocated), // s
            (StandardRC, Deallocated), // b
        ]
    );
}

#[test]
#[cfg(feature = "gen-wasm")]
fn non_nullable_unwrapped_alignment_8() {
    assert_refcounts!(
        indoc!(
            r#"
            Expr : [ZAdd Expr Expr, Val I64, Var I64]

            eval : Expr -> I64
            eval = \e ->
                when e is
                    Var _ -> 0
                    Val v -> v
                    ZAdd l r -> eval l + eval r

            expr : Expr
            expr = (ZAdd (Val 4) (Val 5))

            eval expr
            "#
        ),
        i64,
        &[
            (StandardRC, Deallocated), // Val 4
            (StandardRC, Deallocated), // Val 5
            (StandardRC, Deallocated), // ZAdd _ _
        ]
    );
}

#[test]
#[cfg(feature = "gen-wasm")]
fn reset_reuse_alignment_8() {
    assert_refcounts!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            Expr : [ZAdd Expr Expr, Val I64, Var I64]

            eval : Expr -> I64
            eval = \e ->
                when e is
                    Var _ -> 0
                    Val v -> v
                    ZAdd l r -> eval l + eval r

            const_folding : Expr -> Expr
            const_folding = \e ->
                when e is
                    ZAdd e1 e2 ->
                        when Pair e1 e2 is
                            Pair (Val a) (Val b) -> Val (a+b)
                            Pair _ _             -> ZAdd e1 e2


                    _ -> e


            expr : Expr
            expr = ZAdd (Val 4) (Val 5)

            main : I64
            main = eval (const_folding expr)
            "#
        ),
        i64,
        &[
            (StandardRC, Deallocated), // Val 4
            (StandardRC, Deallocated), // Val 5
            (StandardRC, Deallocated), // ZAdd _ _
        ]
    );
}

#[test]
#[cfg(feature = "gen-wasm")]
fn basic_cli_parser() {
    assert_refcounts!(
        indoc!(
            r#"
            in =
                "d"
                |> Str.to_utf8
                |> List.keep_oks \c -> Str.from_utf8 [c]

            out =
                when in is
                    [alone] -> [alone]
                    other -> other

            List.len out
            "#
        ),
        i64,
        &[
            (StandardRC, Deallocated), // str
            (StandardRC, Deallocated), // [c]
            (AfterSize, Deallocated),  // in
            (AfterSize, Deallocated),  // out
        ]
    );
}
