#[allow(unused_imports)]
use crate::helpers::with_larger_debug_stack;

#[cfg(feature = "gen-llvm")]
use crate::helpers::llvm::assert_evals_to;

#[cfg(feature = "gen-dev")]
use crate::helpers::dev::assert_evals_to;

#[cfg(feature = "gen-wasm")]
use crate::helpers::wasm::assert_evals_to;

#[cfg(test)]
use indoc::indoc;

use roc_mono::layout::{LayoutRepr, STLayoutInterner};
#[cfg(test)]
use roc_std::{RocList, RocStr, I128, U128};

#[test]
fn width_and_alignment_u8_u8() {
    use roc_mono::layout::Layout;
    use roc_mono::layout::UnionLayout;

    let target = roc_target::Target::LinuxX64;
    let interner = STLayoutInterner::with_capacity(4, target);

    let t = &[Layout::U8] as &[_];
    let tt = [t, t];

    let layout = LayoutRepr::Union(UnionLayout::NonRecursive(&tt));

    assert_eq!(layout.alignment_bytes(&interner), 1);
    assert_eq!(layout.stack_size(&interner), 2);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn applied_tag_nothing() {
    assert_evals_to!(
        indoc!(
            r"
                Maybe a : [Just a, Nothing]

                x : Maybe I64
                x = Nothing

                x
                "
        ),
        1,
        (i64, u8),
        |(_, tag)| tag
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn applied_tag_just() {
    assert_evals_to!(
        indoc!(
            r"
                Maybe a : [Just a, Nothing]

                y : Maybe I64
                y = Just 0x4

                y
                "
        ),
        (0x4, 0),
        (i64, u8)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn applied_tag_just_enum() {
    assert_evals_to!(
        indoc!(
            r"
                Fruit : [Orange, Apple, Banana]
                Maybe a : [Just a, Nothing]

                orange : Fruit
                orange = Orange

                y : Maybe Fruit
                y = Just orange

                y
                "
        ),
        (2, 0),
        (u8, u8)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn true_is_true() {
    assert_evals_to!(
        indoc!(
            r"
                   bool : Bool
                   bool = Bool.true

                   bool
                "
        ),
        true,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn false_is_false() {
    assert_evals_to!(
        indoc!(
            r"
                   bool : Bool
                   bool = Bool.false

                   bool
                "
        ),
        false,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn basic_enum() {
    assert_evals_to!(
        indoc!(
            r"
                Fruit : [Apple, Orange, Banana]

                apple : Fruit
                apple = Apple

                orange : Fruit
                orange = Orange

                apple == orange
                "
        ),
        false,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn even_odd() {
    assert_evals_to!(
        indoc!(
            r"
                even = |n|
                    when n is
                        0 -> Bool.true
                        1 -> Bool.false
                        _ -> odd(n - 1)

                odd = |n|
                    when n is
                        0 -> Bool.false
                        1 -> Bool.true
                        _ -> even(n - 1)

                odd(5) and even(42)
                "
        ),
        true,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_literal_true() {
    assert_evals_to!(
        indoc!(
            r"
                if Bool.true then -1 else 1
                "
        ),
        -1,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_if_float() {
    assert_evals_to!(
        indoc!(
            r"
                if Bool.true then -1.0 else 1.0f64
                "
        ),
        -1.0,
        f64
    );
}
#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn when_on_nothing() {
    assert_evals_to!(
        indoc!(
            r"
                x : [Nothing, Just I64]
                x = Nothing

                when x is
                    Nothing -> 0x2
                    Just _ -> 0x1
                "
        ),
        2,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn when_on_just() {
    assert_evals_to!(
        indoc!(
            r"
                x : [Nothing, Just I64]
                x = Just 41

                when x is
                    Just v -> v + 0x1
                    Nothing -> 0x1
                "
        ),
        42,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn when_on_result() {
    assert_evals_to!(
        indoc!(
            r"
                x : Result I64 I64
                x = Err 41

                when x is
                    Err v ->  v + 1
                    Ok _ -> 1
                "
        ),
        42,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn when_on_these() {
    assert_evals_to!(
        indoc!(
            r"
                These a b : [This a, That b, These a b]

                x : These I64 I64
                x = These 0x3 0x2

                when x is
                    These a b -> a + b
                    That v -> v
                    This v -> v
                "
        ),
        5,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn match_on_two_values() {
    // this will produce a Chain internally
    assert_evals_to!(
        indoc!(
            r"
                when Pair 2 3 is
                    Pair 4 3 -> 9
                    Pair a b -> a + b
                "
        ),
        5,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn pair_with_underscore() {
    assert_evals_to!(
        indoc!(
            r"
                when Pair 2 3 is
                    Pair 4 _ -> 1
                    Pair 3 _ -> 2
                    Pair a b -> a + b
                "
        ),
        5,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn result_with_underscore() {
    // This test revealed an issue with hashing Test values
    assert_evals_to!(
        indoc!(
            r"
            x : Result I64 I64
            x = Ok 2

            when x is
                Ok 3 -> 1
                Ok _ -> 2
                Err _ -> 3
            "
        ),
        2,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn maybe_is_just_not_nested() {
    assert_evals_to!(
        indoc!(
            r#"
                app "test" provides [main] to "./platform"

                Maybe a : [Just a, Nothing]

                is_just : Maybe a -> Bool
                is_just = \list ->
                    when list is
                        Nothing -> Bool.false
                        Just _ -> Bool.true

                main =
                    is_just (Just 42)
                "#
        ),
        true,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn maybe_is_just_nested() {
    assert_evals_to!(
        indoc!(
            r"
                Maybe a : [Just a, Nothing]

                is_just : Maybe a -> Bool
                is_just = \list ->
                    when list is
                        Nothing -> Bool.false
                        Just _ -> Bool.true

                is_just (Just 42)
                "
        ),
        true,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn nested_pattern_match() {
    assert_evals_to!(
        indoc!(
            r"
                Maybe a : [Nothing, Just a]

                x : Maybe (Maybe I64)
                x = Just (Just 41)

                when x is
                    Just (Just v) -> v + 0x1
                    _ -> 0x1
                "
        ),
        42,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn if_guard_vanilla() {
    assert_evals_to!(
        indoc!(
            r#"
                when "fooz" is
                    s if s == "foo" -> []
                    s -> Str.to_utf8 s
                "#
        ),
        RocList::from_slice(b"fooz"),
        RocList<u8>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn when_on_single_value_tag() {
    assert_evals_to!(
        indoc!(
            r"
            when Identity 0 is
                Identity 0 -> 6
                Identity s -> s
            "
        ),
        6,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn if_guard_multiple() {
    assert_evals_to!(
        indoc!(
            r"
            f = \n ->
                when Identity n 0 is
                        Identity x _ if x == 0 -> x + 0
                        Identity x _ if x == 1 -> x + 0
                        Identity x _ if x == 2 -> x + 0
                        Identity x _ -> x - x

            { a: f 0, b: f 1, c: f 2, d: f 4 }
                "
        ),
        [0, 1, 2, 0],
        [i64; 4]
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn if_guard_constructor_switch() {
    assert_evals_to!(
        indoc!(
            r"
            when Identity 32 0 is
                    Identity 41 _ -> 0
                    Identity s 0 if s == 32 -> 3
                    # Identity s 0 -> s
                    Identity z _ -> z
                "
        ),
        3,
        i64
    );

    assert_evals_to!(
        indoc!(
            r#"
            when Identity 42 "" is
                    Identity 41 _ -> 0
                    Identity 42 _ if 3 == 3 -> 1
                    Identity z _ -> z
                "#
        ),
        1,
        i64
    );

    assert_evals_to!(
        indoc!(
            r#"
            when Identity 42 "" is
                    Identity 41 _ -> 0
                    Identity 42 _ if 3 != 3 -> 1
                    Identity z _ -> z
                "#
        ),
        42,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn if_guard_constructor_chain() {
    assert_evals_to!(
        indoc!(
            r"
            when Identity 43 0 is
                    Identity 42 _ if 3 == 3 -> 43
                    # Identity 42 _ -> 1
                    Identity z _ -> z
                "
        ),
        43,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn if_guard_pattern_false() {
    assert_evals_to!(
        indoc!(
            r"
                wrapper = \{} ->
                    when 2 is
                        2 if Bool.false -> 0
                        _ -> 42

                wrapper {}
                "
        ),
        42,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn if_guard_switch() {
    assert_evals_to!(
        indoc!(
            r"
                wrapper = \{} ->
                    when 2 is
                        2 | 3 if Bool.false -> 0
                        _ -> 42

                wrapper {}
                "
        ),
        42,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn if_guard_pattern_true() {
    assert_evals_to!(
        indoc!(
            r"
                wrapper = \{} ->
                    when 2 is
                        2 if Bool.true -> 42
                        _ -> 0

                wrapper {}
                "
        ),
        42,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn if_guard_exhaustiveness() {
    assert_evals_to!(
        indoc!(
            r"
                wrapper = \{} ->
                    when 2 is
                        _ if Bool.false -> 0
                        _ -> 42

                wrapper {}
                "
        ),
        42,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn when_on_enum() {
    assert_evals_to!(
        indoc!(
            r"
                Fruit : [Apple, Orange, Banana]

                apple : Fruit
                apple = Apple

                when apple is
                    Apple -> 1
                    Banana -> 2
                    Orange -> 3
                "
        ),
        1,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn pattern_matching_unit() {
    assert_evals_to!(
        indoc!(
            r"
                Unit : [Unit]

                f : Unit -> I64
                f = \Unit -> 42

                f Unit
                "
        ),
        42,
        i64
    );

    assert_evals_to!(
        indoc!(
            r"
                Unit : [Unit]

                x : Unit
                x = Unit

                when x is
                    Unit -> 42
                "
        ),
        42,
        i64
    );

    assert_evals_to!(
        indoc!(
            r"
                f : {} -> I64
                f = \{} -> 42

                f {}
                "
        ),
        42,
        i64
    );

    assert_evals_to!(
        indoc!(
            r"
                when {} is
                    {} -> 42
                "
        ),
        42,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn one_element_tag() {
    assert_evals_to!(
        indoc!(
            r"
                x : [Pair I64]
                x = Pair 2

                x
                "
        ),
        2,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn nested_tag_union() {
    assert_evals_to!(
        indoc!(
            r#"
                app "test" provides [main] to "./platform"

                Maybe a : [Nothing, Just a]

                x : Maybe (Maybe I64)
                x = Just (Just 41)

                main =
                    x
                "#
        ),
        ((41, 0), 0),
        ((i64, u8), u8)
    );
}
#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn unit_type() {
    assert_evals_to!(
        indoc!(
            r"
                Unit : [Unit]

                v : Unit
                v = Unit

                v
                "
        ),
        (),
        ()
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn join_point_if() {
    assert_evals_to!(
        indoc!(
            r"
                x =
                    if Bool.true then 1 else 2

                x
                "
        ),
        1,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn join_point_when() {
    assert_evals_to!(
        indoc!(
            r"
            wrapper = \{} ->
                x : [Red, White, Blue]
                x = Blue

                y =
                    when x is
                        Red -> 1
                        White -> 2
                        Blue -> 3.1f64

                y

            wrapper {}
            "
        ),
        3.1,
        f64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn join_point_with_cond_expr() {
    assert_evals_to!(
        indoc!(
            r"
                wrapper = \{} ->
                    y =
                        when 1 + 2 is
                            3 -> 3
                            1 -> 1
                            _ -> 0

                    y

                wrapper {}
            "
        ),
        3,
        i64
    );

    assert_evals_to!(
        indoc!(
            r"
            y =
                if 1 + 2 > 0 then
                    3
                else
                    0

            y
            "
        ),
        3,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn alignment_in_single_tag_construction() {
    assert_evals_to!(indoc!("Three (1 == 1) 32"), (32i64, true), (i64, bool));

    assert_evals_to!(
        indoc!("Three (1 == 1) (if Bool.true then Red else if Bool.true then Green else Blue) 32"),
        (32i64, true, 2u8),
        (i64, bool, u8)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn alignment_in_single_tag_pattern_match() {
    assert_evals_to!(
        indoc!(
            r"#
                x = Three (1 == 1) 32

                when x is
                    Three bool int ->
                        { bool, int }
                #"
        ),
        (32i64, true),
        (i64, bool)
    );

    assert_evals_to!(
        indoc!(
            r"#
                x = Three (1 == 1) (if Bool.true then Red else if Bool.true then Green else Blue) 32

                when x is
                    Three bool color int ->
                        { bool, color, int }
                #"
        ),
        (32i64, true, 2u8),
        (i64, bool, u8)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn alignment_in_multi_tag_construction_two() {
    assert_evals_to!(
        indoc!(
            r"#
                x : [Three Bool I64, Empty]
                x = Three (1 == 1) 32

                x

                #"
        ),
        ((32i64, true), 1),
        ((i64, bool), u8)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn alignment_in_multi_tag_construction_three() {
    assert_evals_to!(
        indoc!(
            r"#
                x : [Three Bool [Red, Green, Blue] I64, Empty]
                x = Three (1 == 1) (if Bool.true then Red else if Bool.true then Green else Blue) 32

                x
                #"
        ),
        ((32i64, true, 2u8), 1),
        ((i64, bool, u8), u8)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn alignment_in_multi_tag_pattern_match() {
    assert_evals_to!(
        indoc!(
            r"#
                x : [Three Bool I64, Empty]
                x = Three (1 == 1) 32

                when x is
                    Three bool int ->
                        { bool, int }

                    Empty ->
                        # dev backend codegen bug means we cannot use this inline
                        false = Bool.false
                        { bool: false, int: 0 }
                #"
        ),
        (32i64, true),
        (i64, bool)
    );

    assert_evals_to!(
        indoc!(
            r"#
                x : [Three Bool [Red, Green, Blue] I64, Empty]
                x = Three (1 == 1) (if Bool.true then Red else if Bool.true then Green else Blue) 32

                when x is
                    Three bool color int ->
                        { bool, color, int }
                    Empty ->
                        false = Bool.false
                        { bool: false, color: Red, int: 0 }
                #"
        ),
        (32i64, true, 2u8),
        (i64, bool, u8)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn phantom_polymorphic() {
    assert_evals_to!(
        indoc!(
            r"#
                Point coordinate : [Point coordinate I64 I64]

                World := {}

                zero : Point World
                zero = Point (@World {}) 0 0

                add : Point a -> Point a
                add = \(Point c x y) -> (Point c x y)

                add zero
                #"
        ),
        (0, 0),
        (i64, i64)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn phantom_polymorphic_record() {
    assert_evals_to!(
        indoc!(
            r#"
                app "test" provides [main] to "./platform"

                Point coordinate : { coordinate : coordinate, x : I64, y : I64 }

                zero : Point I64
                zero = { coordinate : 0, x : 0, y : 0 }

                add : Point a -> Point a
                add = \{ coordinate } -> { coordinate, x: 0 + 0, y: 0 }

                main = add zero
                "#
        ),
        (0, 0, 0),
        (i64, i64, i64)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn result_never() {
    assert_evals_to!(
        indoc!(
            r"#
                res : Result I64 []
                res = Ok 4

                when res is
                    Ok v -> v
                #"
        ),
        4,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn nested_recursive_literal() {
    assert_evals_to!(
        indoc!(
            r"#
                Expr : [Add Expr Expr, Val I64, Var I64]

                e : Expr
                e = Add (Add (Val 3) (Val 1)) (Add (Val 1) (Var 1))

                e
                #"
        ),
        0,
        usize,
        |_| 0
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn newtype_wrapper() {
    assert_evals_to!(
        indoc!(
            r#"
                app "test" provides [main] to "./platform"

                ConsList a : [Nil, Cons a (ConsList a)]

                foo : ConsList I64 -> ConsList I64
                foo = \t ->
                    when Delmin (Del t 0.0) is
                        Delmin (Del ry _) -> Cons 42 ry

                main = foo Nil
                "#
        ),
        42,
        roc_std::RocBox<i64>,
        |x: roc_std::RocBox<i64>| {
            let value = *x;
            std::mem::forget(x);
            value
        }
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn applied_tag_function() {
    assert_evals_to!(
        indoc!(
            r#"
            x : List [Foo Str]
            x = List.map ["a", "b"] Foo

            x
            "#
        ),
        RocList::from_slice(&[RocStr::from("a"), RocStr::from("b")]),
        RocList<RocStr>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn applied_tag_function_result() {
    assert_evals_to!(
        indoc!(
            r#"
            x : List (Result Str _)
            x = List.map ["a", "b"] Ok

            List.keep_oks x (\y -> y)
            "#
        ),
        RocList::from_slice(&[(RocStr::from("a")), (RocStr::from("b"))]),
        RocList<RocStr>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
#[ignore = "This test has incorrect refcounts: https://github.com/roc-lang/roc/issues/2968"]
fn applied_tag_function_linked_list() {
    assert_evals_to!(
        indoc!(
            r#"
            ConsList a : [Nil, Cons a (ConsList a)]

            x : List (ConsList Str)
            x = List.map2 ["a", "b"] [Nil, Cons "c" Nil] Cons

            when List.first x is
                Ok (Cons "a" Nil) -> 1
                _ -> 0
            "#
        ),
        1,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn applied_tag_function_pair() {
    assert_evals_to!(
        indoc!(
            r#"
            Pair a : [Pair a a]

            x : List (Pair Str)
            x = List.map2 ["a", "b"] ["c", "d"] Pair

            when List.first x is
                Ok (Pair "a" "c") -> 1
                _ -> 0
            "#
        ),
        1,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
#[should_panic(expected = "")] // TODO: this only panics because it returns 0 instead of 1!
fn tag_must_be_its_own_type() {
    assert_evals_to!(
        indoc!(
            r"
            z : [A, B, C]
            z = Z

            z
            "
        ),
        1,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn recursive_tag_union_into_flat_tag_union() {
    // Comprehensive test for correctness in cli/tests/repl_eval
    assert_evals_to!(
        indoc!(
            r#"
            Item : [Shallow [L Str, R Str], Deep Item]
            i : Item
            i = Deep (Shallow (R "woo"))
            i
            "#
        ),
        0,
        usize,
        |_| 0
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn monomorphized_tag() {
    assert_evals_to!(
        indoc!(
            r"
            b = \{} -> Bar
            f : [Foo, Bar], [Bar, Baz] -> U8
            f = \_, _ -> 18
            f (b {}) (b {})
            "
        ),
        18,
        u8
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn monomorphized_applied_tag() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            main =
                a = A "abc"
                f = \x ->
                    when x is
                        A y -> y
                        B y -> y
                f a
            "#
        ),
        RocStr::from("abc"),
        RocStr
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn monomorphized_tag_with_polymorphic_arg() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            main =
                a = \{} -> A
                wrap = \{} -> Wrapped (a {})

                use_wrap1 : [Wrapped [A], Other] -> U8
                use_wrap1 =
                    \w -> when w is
                        Wrapped A -> 2
                        Other -> 3

                use_wrap2 : [Wrapped [A, B]] -> U8
                use_wrap2 =
                    \w -> when w is
                        Wrapped A -> 5
                        Wrapped B -> 7

                if Bool.true then use_wrap1 (wrap {}) else use_wrap2 (wrap {})
            "#
        ),
        2,
        u8
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn monomorphized_tag_with_polymorphic_and_monomorphic_arg() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            main =
                mono : U8
                mono = 15
                poly = \{} -> A
                wrap = \{} -> Wrapped (poly {}) mono

                use_wrap1 : [Wrapped [A] U8, Other] -> U8
                use_wrap1 =
                    \w -> when w is
                        Wrapped A n -> n
                        Other -> 0

                use_wrap2 : [Wrapped [A, B] U8] -> U8
                use_wrap2 =
                    \w -> when w is
                        Wrapped A n -> n
                        Wrapped B _ -> 0

                use_wrap1 (wrap {}) * use_wrap2 (wrap {})
            "#
        ),
        225,
        u8
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn issue_2365_monomorphize_tag_with_non_empty_ext_var() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            Single a : [A, B, C]a
            Compound a : Single [D, E, F]a

            single : {} -> Single *
            single = \{} -> C

            compound : {} -> Compound *
            compound = \{} -> single {}

            main = compound {}
            "#
        ),
        2, // C
        u8
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn issue_2365_monomorphize_tag_with_non_empty_ext_var_wrapped() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            Single a : [A, B, C]a
            Compound a : Single [D, E, F]a
            Padding : { a: U64, b: U64, c: U64 }

            single : {} -> Result Padding (Single *)
            single = \{} -> Err C

            compound : {} -> Result Padding (Compound *)
            compound = \{} ->
                when single {} is
                    Ok s -> Ok s
                    Err e -> Err e

            main = compound {}
            "#
        ),
        (0, 2), // Err, C
        ([u8; std::mem::size_of::<(u64, u64, u64)>()], u8),
        |(err_tag, wrap_tag): ([u8; std::mem::size_of::<(u64, u64, u64)>()], u8)| (
            wrap_tag, err_tag[0]
        )
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn issue_2365_monomorphize_tag_with_non_empty_ext_var_wrapped_nested() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            Single a : [A, B, C]a
            Compound a : Single [D, E, F]a
            Padding : { a: U64, b: U64, c: U64 }

            main =
                single : {} -> Result Padding (Single *)
                single = \{} -> Err C

                compound : {} -> Result Padding (Compound *)
                compound = \{} ->
                    when single {} is
                        Ok s -> Ok s
                        Err e -> Err e

                compound {}
            "#
        ),
        (0, 2), // Err, C
        ([u8; std::mem::size_of::<(u64, u64, u64)>()], u8),
        |(err_tag, wrap_tag): ([u8; std::mem::size_of::<(u64, u64, u64)>()], u8)| (
            wrap_tag, err_tag[0]
        )
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn issue_2445() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            none : [None, Update _]
            none = None

            press : [None, Update U8]
            press = none

            main =
                when press is
                    None -> 15
                    Update _ -> 25
            "#
        ),
        15,
        i64
    );
}

#[test]
#[cfg(feature = "gen-llvm")]
fn issue_2458() {
    assert_evals_to!(
        indoc!(
            r"
            Foo a : [Blah (Bar a), Nothing {}]
            Bar a : Foo a

            v : Bar {}
            v = Blah (Blah (Nothing {}))

            when v is
                Blah (Blah (Nothing {})) -> 15
                _ -> 25
            "
        ),
        15,
        u8
    )
}

#[test]
#[ignore = "See https://github.com/roc-lang/roc/issues/2466"]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn issue_2458_deep_recursion_var() {
    assert_evals_to!(
        indoc!(
            r#"
            Foo a : [Blah (Result (Bar a) {})]
            Bar a : Foo a

            v : Bar {}

            when v is
                Blah (Ok (Blah (Err {}))) -> "1"
                _ -> "2"
            "#
        ),
        15,
        u8
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn issue_1162() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            RBTree k : [Node k (RBTree k) (RBTree k), Empty]

            balance : a, RBTree a -> RBTree a
            balance = \key, left ->
                  when left is
                    Node _ _ l_right ->
                        Node key l_right Empty

                    _ ->
                        Empty


            tree : RBTree {}
            tree =
                balance {} Empty

            main : U8
            main =
                when tree is
                    Empty -> 15
                    _ -> 25
            "#
        ),
        15,
        u8
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn polymorphic_tag() {
    assert_evals_to!(
        indoc!(
            r"
            x : [Y U8]
            x = Y 3
            x
            "
        ),
        3, // Y is a newtype, it gets unwrapped
        u8
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn issue_2725_alias_polymorphic_lambda() {
    assert_evals_to!(
        indoc!(
            r"
            wrap = \value -> Tag value
            wrap_it = wrap
            wrap_it 42
            "
        ),
        42, // Tag is a newtype, it gets unwrapped
        i64
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn opaque_assign_to_symbol() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [out] to "./platform"

            Variable := U8

            from_utf8 : U8 -> Result Variable [InvalidVariableUtf8]
            from_utf8 = \char ->
                Ok (@Variable char)

            out =
                when from_utf8 98 is
                    Ok (@Variable n) -> n
                    _ -> 1
            "#
        ),
        98,
        u8
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn issue_2777_default_branch_codegen() {
    assert_evals_to!(
        indoc!(
            r#"
            f1 = \color ->
              when color is
                Red -> "red"
                Yellow -> "yellow"
                _ -> "unknown"

            r1 = Red |> f1 |> Str.concat (f1 Orange)

            f2 = \color ->
              when color is
                Red -> "red"
                Yellow -> "yellow"
                Green -> "green"
                _ -> "unknown"

            r2 = Red |> f2 |> Str.concat (f2 Orange)

            f3 = \color ->
              when color is
                Red -> "red"
                Yellow -> "yellow"
                Green -> "green"
                _ -> "unknown"

            r3 = Orange |> f3 |> Str.concat (f3 Red)

            f4 = \color ->
              when color is
                Red -> "red"
                Yellow | Gold -> "yellow"
                _ -> "unknown"

            r4 = Red |> f4 |> Str.concat (f4 Orange)

            [r1, r2, r3, r4]
            "#
        ),
        RocList::from_slice(&[
            RocStr::from("redunknown"),
            RocStr::from("redunknown"),
            RocStr::from("unknownred"),
            RocStr::from("redunknown"),
        ]),
        RocList<RocStr>
    )
}

#[test]
// This doesn't work on Windows. If you make it return a `bool`, e.g. with `|> Str.isEmpty` at the end,
// then it works. We don't know what the problem is here!
#[cfg(all(
    not(target_family = "windows"),
    any(feature = "gen-llvm", feature = "gen-wasm")
))]
#[should_panic(expected = r#"Roc failed with message: "Tag Foo was part of a type error!""#)]
fn issue_2900_unreachable_pattern() {
    assert_evals_to!(
        indoc!(
            r#"
            foo : [Foo, Bar, Baz, Blah] -> Str
            foo = \arg ->
                when arg is
                    Foo -> "foo"
                    AnUnreachableTag -> "blah"
                    _ -> "other"

            foo Foo
            "#
        ),
        RocStr::from("foo"),
        RocStr,
        |x| x,
        true // ignore type errors
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn issue_3261_non_nullable_unwrapped_recursive_union_at_index() {
    assert_evals_to!(
        indoc!(
            r#"
            Named : [Named Str (List Named)]

            foo : Named
            foo = Named "outer" [Named "inner" []]

            Named name outer_list = foo

            {name, outer_list}.name
            "#
        ),
        RocStr::from("outer"),
        RocStr
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn instantiate_annotated_as_recursive_alias_toplevel() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            Value : [Nil, Array (List Value)]

            foo : [Nil]_
            foo = Nil

            it : Value
            it = foo

            main =
                when it is
                    Nil -> 123i64
                    _ -> -1i64
            "#
        ),
        123,
        i64
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn instantiate_annotated_as_recursive_alias_polymorphic_expr() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            main =
                Value : [Nil, Array (List Value)]

                foo : [Nil]_
                foo = Nil

                it : Value
                it = foo

                when it is
                    Nil -> 123i64
                    _ -> -1i64
            "#
        ),
        123,
        i64
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn instantiate_annotated_as_recursive_alias_multiple_polymorphic_expr() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            main =
                Value : [Nil, Array (List Value)]

                foo : {} -> [Nil]
                foo = \{} -> Nil

                v1 : Value
                v1 = foo {}

                Value2 : [Nil, B U16, Array (List Value)]

                v2 : Value2
                v2 = foo {}

                when {v1, v2} is
                    {v1: Nil, v2: Nil} -> 123i64
                    {v1: _, v2: _} -> -1i64
            "#
        ),
        123,
        i64
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn issue_3560_nested_tag_constructor_is_newtype() {
    assert_evals_to!(
        indoc!(
            r"
            f : _ -> u8
            f = \t ->
                when t is
                    Wrapper (Payload it) -> it
                    Wrapper (AlternatePayload it) -> it

            {a: f (Wrapper (Payload 15u8)), b: f(Wrapper (AlternatePayload 31u8))}
            "
        ),
        (15, 31),
        (u8, u8)
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn issue_3560_nested_tag_constructor_is_record_newtype() {
    assert_evals_to!(
        indoc!(
            r"
            f : _ -> u8
            f = \t ->
                when t is
                    {wrapper: (Payload it)} -> it
                    {wrapper: (AlternatePayload it)} -> it

            {a: f {wrapper: (Payload 15u8)}, b: f {wrapper: (AlternatePayload 31u8)}}
            "
        ),
        (15, 31),
        (u8, u8)
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn issue_3560_newtype_tag_constructor_has_nested_constructor_with_no_payload() {
    assert_evals_to!(
        indoc!(
            r#"
            when Wrapper (Payload "err") is
                Wrapper (Payload str) -> str
                Wrapper NoPayload -> "nothing"
            "#
        ),
        RocStr::from("err"),
        RocStr
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn alignment_i128() {
    assert_evals_to!(
        indoc!(
            r"#
                x : [One I128 Bool, Empty]
                x = One 42 (1 == 1)
                x
                #"
        ),
        // NOTE: roc_std::I128 is always aligned to 16, unlike rust's i128
        ((I128::from(42), true), 1),
        ((I128, bool), u8)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn alignment_u128() {
    assert_evals_to!(
        indoc!(
            r"#
                x : [One U128 Bool, Empty]
                x = One 42 (1 == 1)
                x
                #"
        ),
        // NOTE: roc_std::U128 is always aligned to 16, unlike rust's i128
        ((U128::from(42), true), 1),
        ((U128, bool), u8)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
#[ignore = "causes alias analysis panics, should roc_panic"]
fn error_type_in_tag_union_payload() {
    assert_evals_to!(
        indoc!(
            r"
            f : ([] -> Bool) -> Bool
            f = \fun ->
              if Bool.true then
                fun 42
              else
                Bool.false

            f (\x -> x)
            "
        ),
        0,
        u8,
        |x| x,
        true // ignore type errors
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn issue_3653_recursion_pointer_in_naked_opaque() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            Peano := [ Zero, Succ Peano ]

            recurse = \@Peano peano ->
                when peano is
                    Succ inner -> recurse inner
                    _ -> {}

            main =
                when recurse (@Peano Zero) is
                    _ -> "we're back"
            "#
        ),
        RocStr::from("we're back"),
        RocStr
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn issue_3653_recursion_pointer_in_naked_opaque_localized() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            Peano := [ Zero, Succ Peano ]

            recurse = \peano ->
                when peano is
                    @Peano (Succ inner) -> recurse inner
                    @Peano Zero -> {}

            main =
                when recurse (@Peano Zero) is
                    _ -> "we're back"
            "#
        ),
        RocStr::from("we're back"),
        RocStr
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn issue_2165_recursive_tag_destructure() {
    assert_evals_to!(
        indoc!(
            r"
            SomeTag : [ Ctor { rec : List SomeTag } ]

            x : SomeTag
            x = Ctor { rec: [] }

            when x is
              Ctor { rec } -> Num.to_str (List.len rec)
            "
        ),
        RocStr::from("0"),
        RocStr
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn tag_union_let_generalization() {
    assert_evals_to!(
        indoc!(
            r#"
            many_aux : {} -> [ Loop, Done ]
            many_aux = \_ ->
                output = Done

                output

            when many_aux {} is
                Loop -> "loop"
                Done -> "done"
            "#
        ),
        RocStr::from("done"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn fit_recursive_union_in_struct_into_recursive_pointer() {
    assert_evals_to!(
        indoc!(
            r#"
            NonEmpty := [
                First Str,
                Next { item: Str, rest: NonEmpty },
            ]

            non_empty =
                a = "abcdefgh"
                b = @NonEmpty (First "ijkl")
                c = Next { item: a, rest: b }
                @NonEmpty c

            when non_empty is
                @NonEmpty (Next r) -> r.item
                _ -> "<bad>"
            "#
        ),
        RocStr::from("abcdefgh"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn match_on_result_with_uninhabited_error_branch() {
    assert_evals_to!(
        indoc!(
            r#"
            x : Result Str []
            x = Ok "abc"

            when x is
                Ok s -> s
            "#
        ),
        RocStr::from("abc"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn dispatch_tag_union_function_inferred() {
    assert_evals_to!(
        indoc!(
            r#"
            g = \b -> if b then H else J

            when P ((g Bool.true) "") ((g Bool.false) "") is
                P (H _) (J _) -> "okay"
                _ -> "FAIL"
            "#
        ),
        RocStr::from("okay"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn issue_4077_fixed_fixpoint() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            Input : [FromProjectSource, FromJob Job]

            Job : [Job { inputs : List Input }]

            job : { inputs : List Input } -> Job
            job = \config -> Job config

            main =
                when job { inputs: [] } is
                    _ -> "OKAY"
            "#
        ),
        RocStr::from("OKAY"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn unify_types_with_fixed_fixpoints_outside_fixing_region() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            Input := [
                FromJob Job (List Str),
            ]

            Job := [
                Job (List Input)
            ]

            job : List Input -> Job
            job = \inputs ->
                @Job (Job inputs)

            hello_world : Job
            hello_world =
                @Job ( Job [ @Input (FromJob greeting []) ] )

            greeting : Job
            greeting =
                job []

            main = (\_ -> "OKAY") hello_world
            "#
        ),
        RocStr::from("OKAY"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn lambda_set_with_imported_toplevels_issue_4733() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            fn = \s ->
                instr = if s == "*" then (Op Num.mul) else (Op Num.add)

                Op op = instr

                \a -> op a a

            main = ((fn "*") 3) * ((fn "+") 5)
            "#
        ),
        90,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn non_unary_union_with_lambda_set_with_imported_toplevels_issue_4733() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            fn = \s ->
                instr =
                    if s == "*" then (Op Num.mul)
                    else if s == "+" then (Op Num.add)
                    else Noop

                when instr is
                    Op op -> (\a -> op a a)
                    _ -> (\a -> a)


            main = ((fn "*") 3i64) * ((fn "+") 5)
            "#
        ),
        90,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn nullable_wrapped_with_non_nullable_singleton_tags() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            F : [
                A F,
                B,
                C,
            ]

            g : F -> Str
            g = \f -> when f is
                    A _ -> "A"
                    B -> "B"
                    C -> "C"

            main =
                g (A (B))
                |> Str.concat (g B)
                |> Str.concat (g C)
            "#
        ),
        RocStr::from("ABC"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn nullable_wrapped_with_nullable_not_last_index() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            Parser : [
                CharLiteral,
                Keyword Str,
                OneOrMore Parser,
            ]

            to_id_parser : Parser -> Str
            to_id_parser = \parser ->
                when parser is
                    OneOrMore _ -> "a"
                    Keyword _ -> "b"
                    CharLiteral -> "c"

            main =
                to_id_parser (OneOrMore CharLiteral)
                |> Str.concat (to_id_parser (Keyword "try"))
                |> Str.concat (to_id_parser CharLiteral)
            "#
        ),
        RocStr::from("abc"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn refcount_nullable_unwrapped_needing_no_refcount_issue_5027() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            Effect : {} -> Str

            after = \effect, build_next ->
                \{} ->
                    when build_next (effect {}) is
                        thunk -> thunk {}

            line : Effect
            line = \{} -> "done"

            await : Effect, (Str -> Effect) -> Effect
            await = \fx, cont ->
                after
                    fx
                    cont

            succeed : {} -> Effect
            succeed = \{} -> (\{} -> "success")

            test =
                await line \s ->
                    if s == "done" then succeed {} else test

            main = test {}
            "#
        ),
        RocStr::from("success"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn issue_5162_recast_nested_nullable_unwrapped_layout() {
    with_larger_debug_stack(|| {
        assert_evals_to!(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                Concept : [
                    AtomicConcept,
                    ExistentialRestriction { role : Str, concept : Concept }
                ]

                bottom : Concept
                bottom = AtomicConcept

                main =
                    when Dict.single bottom 0 is
                        _ -> Bool.true
                "#
            ),
            true,
            bool
        );
    });
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn nullable_wrapped_eq_issue_5434() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            Value : [
                A,
                B I64,
                C,
                D (List [T Str Value]),
            ]

            main =
                x : Value
                x = B 32
                y : Value
                y = B 0
                if x == y then
                    Bool.true
                else
                    Bool.false
            "#
        ),
        false,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn recursive_tag_id_in_allocation_basic() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            Value : [
                A Value,
                B I64,
                C I64,
                D I64,
                E I64,
                F I64,
                G I64,
                H I64,
                I I64,
            ]

            x : Value
            x = H 42

            main =
                when x is
                    A _ -> "A"
                    B _ -> "B"
                    C _ -> "C"
                    D _ -> "D"
                    E _ -> "E"
                    F _ -> "F"
                    G _ -> "G"
                    H _ -> "H"
                    I _ -> "I"
            "#
        ),
        RocStr::from("H"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn recursive_tag_id_in_allocation_eq() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            Value : [
                A Value,
                B I64,
                C I64,
                D I64,
                E I64,
                F I64,
                G I64,
                H I64,
                I I64,
            ]

            x : Value
            x = G(42)

            y : Value
            y = H(42)

            main = x == x and x != y and y == y
            "#
        ),
        true,
        bool
    );
}
