#[cfg(feature = "gen-llvm")]
use crate::helpers::llvm::assert_evals_to;

#[cfg(feature = "gen-dev")]
use crate::helpers::dev::assert_evals_to;

#[cfg(feature = "gen-wasm")]
use crate::helpers::wasm::assert_evals_to;

use indoc::indoc;
#[allow(unused_imports)]
use roc_std::{RocBox, RocDec, RocList, RocStr};

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn basic_int() {
    assert_evals_to!("123", 123, i64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn basic_float() {
    assert_evals_to!("1234.0f64", 1234.0, f64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn branch_first_float() {
    assert_evals_to!(
        indoc!(
            r"
                when 1.23f64 is
                    1.23 -> 12
                    _ -> 34
            "
        ),
        12,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn branch_second_float() {
    assert_evals_to!(
        indoc!(
            r"
                when 2.34 is
                    1.23 -> 63
                    _ -> 48
            "
        ),
        48,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn branch_third_float() {
    assert_evals_to!(
        indoc!(
            r"
               when 10.0 is
                   1.0 -> 63
                   2.0 -> 48
                   _ -> 112
            "
        ),
        112,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn branch_first_int() {
    assert_evals_to!(
        indoc!(
            r"
                when 1 is
                    1 -> 12
                    _ -> 34
            "
        ),
        12,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn branch_second_int() {
    assert_evals_to!(
        indoc!(
            r"
                when 2 is
                    1 -> 63
                    _ -> 48
            "
        ),
        48,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev"))]
fn branch_third_int() {
    assert_evals_to!(
        indoc!(
            r"
                when 10 is
                    1 -> 63
                    2 -> 48
                    _ -> 112
            "
        ),
        112,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn branch_store_variable() {
    assert_evals_to!(
        indoc!(
            r"
                when 0 is
                    1 -> 12
                    a -> a
            "
        ),
        0,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn when_one_element_tag() {
    assert_evals_to!(
        indoc!(
            r"
            x : [Pair (Int a) (Int a)]
            x = Pair 0x2 0x3

            when x is
                Pair l r -> l + r
            "
        ),
        5,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn when_two_element_tag_first() {
    assert_evals_to!(
        indoc!(
            r"
            x : [A (Int a), B (Int a)]
            x = A 0x2

            when x is
                A v -> v
                B v -> v
            "
        ),
        2,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn when_two_element_tag_second() {
    assert_evals_to!(
        indoc!(
            r"
            x : [A (Int a), B (Int a)]
            x = B 0x3

            when x is
                A v -> v
                B v -> v
            "
        ),
        3,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_when_one_branch() {
    assert_evals_to!(
        indoc!(
            r"
                when 1.23 is
                    _ -> 23
            "
        ),
        23,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_large_when_int() {
    assert_evals_to!(
        indoc!(
            r"
                foo = \num ->
                    when num is
                        0 -> 200
                        -3 -> 111 # TODO adding more negative numbers reproduces parsing bugs here
                        3 -> 789
                        1 -> 123
                        2 -> 456
                        _ -> 1000

                foo -3
            "
        ),
        111,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_large_when_float() {
    assert_evals_to!(
        indoc!(
            r"
                foo = \num ->
                    when num is
                        0.5f64 -> 200.1
                        -3.6 -> 111.2 # TODO adding more negative numbers reproduces parsing bugs here
                        3.6 -> 789.5
                        1.7 -> 123.3
                        2.8 -> 456.4
                        _ -> 1000.6f64

                foo -3.6
            "
        ),
        111.2,
        f64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn or_pattern() {
    assert_evals_to!(
        indoc!(
            r"
            when 2 is
                1 | 2 -> 42
                _ -> 1
            "
        ),
        42,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn apply_identity() {
    assert_evals_to!(
        indoc!(
            r"
                identity = \a -> a

                identity 5
            "
        ),
        5,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn apply_unnamed_identity() {
    assert_evals_to!(
        indoc!(
            r"
            wrapper = \{} ->
                (\a -> a) 5

            wrapper {}
            "
        ),
        5,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn return_unnamed_fn() {
    assert_evals_to!(
        indoc!(
            r"
            wrapper = \{} ->
                always_float_identity : Int * -> (Frac a -> Frac a)
                always_float_identity = \_ ->
                    (\a -> a)

                (always_float_identity 2) 1.23f64

            wrapper {}
            "
        ),
        1.23,
        f64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_when_fn() {
    assert_evals_to!(
        indoc!(
            r"
                limited_negate = \num ->
                    when num is
                        1 -> -1
                        -1 -> 1
                        _ -> num

                limited_negate 1
            "
        ),
        -1,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_basic_def() {
    assert_evals_to!(
        indoc!(
            r"
                answer = 42

                answer
            "
        ),
        42,
        i64
    );

    assert_evals_to!(
        indoc!(
            r"
                float = 1.23f64

                float
            "
        ),
        1.23,
        f64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn gen_multiple_defs() {
    assert_evals_to!(
        indoc!(
            r"
                answer = 42

                float = 1.23f64

                if float > 3 then answer else answer
            "
        ),
        42,
        i64
    );

    assert_evals_to!(
        indoc!(
            r"
                answer = 42

                float = 1.23f64

                if answer > 3 then float else float
            "
        ),
        1.23,
        f64
    );
}

// These tests caught a bug in how Defs are converted to the mono IR
// but they have UnusedDef or UnusedArgument problems, and don't run any more
//    #[test]
// #[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
//    fn gen_chained_defs() {
//        assert_evals_to!(
//            indoc!(
//                r"
//                    x = i1
//                    i3 = i2
//                    i1 = 1337
//                    i2 = i1
//                    y = 12.4
//
//                    i3
//                "
//            ),
//            1337,
//            i64
//        );
//    }
//
//    #[test]
// #[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
//    fn gen_nested_defs_old() {
//        assert_evals_to!(
//            indoc!(
//                r"
//                    x = 5
//
//                    answer =
//                        i3 = i2
//
//                        nested =
//                            a = 1.0
//                            b = 5
//
//                            i1
//
//                        i1 = 1337
//                        i2 = i1
//
//
//                        nested
//
//                    # None of this should affect anything, even though names
//                    # overlap with the previous nested defs
//                    unused =
//                        nested = 17
//
//                        i1 = 84.2
//
//                        nested
//
//                    y = 12.4
//
//                    answer
//                "
//            ),
//            1337,
//            i64
//        );
//    }
//
//    #[test]
// #[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
//    fn let_x_in_x() {
//        assert_evals_to!(
//            indoc!(
//                r"
//                    x = 5
//
//                    answer =
//                        1337
//
//                    unused =
//                        nested = 17
//                        nested
//
//                    answer
//                "
//            ),
//            1337,
//            i64
//        );
//    }

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn factorial() {
    assert_evals_to!(
        indoc!(
            r"
            factorial = \n, accum ->
                when n is
                    0 ->
                        accum

                    _ ->
                        factorial (n - 1) (n * accum)

            factorial 10 1
            "
        ),
        3628800,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn peano1() {
    assert_evals_to!(
        indoc!(
            r"
                Peano : [S Peano, Z]

                three : Peano
                three = S (S (S Z))

                when three is
                    Z -> 2
                    S _ -> 1
                "
        ),
        1,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn peano2() {
    assert_evals_to!(
        indoc!(
            r"
                Peano : [S Peano, Z]

                three : Peano
                three = S (S (S Z))

                when three is
                    S (S _) -> 1
                    S (_) -> 0
                    Z -> 0
                "
        ),
        1,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn top_level_constant() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            float = 1.2315f64

            main =
                float + float
                "#
        ),
        1.2315 + 1.2315,
        f64
    );
}

#[test]
#[ignore]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn top_level_destructure() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            {a, b} = { a: 1, b: 2 }

            main =

                a + b
                "#
        ),
        3,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn linked_list_len_0() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            LinkedList a : [Nil, Cons a (LinkedList a)]

            len : LinkedList a -> Int *
            len = \list ->
                when list is
                    Nil -> 0
                    Cons _ rest -> 1 + len rest

            main =
                nil : LinkedList F64
                nil = Nil

                len nil
            "#
        ),
        0,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn linked_list_len_twice_0() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            LinkedList a : [Nil, Cons a (LinkedList a)]

            nil : LinkedList I64
            nil = Nil

            length : LinkedList a -> Int *
            length = \list ->
                when list is
                    Nil -> 0
                    Cons _ rest -> 1 + length rest

            main =
                length nil + length nil
            "#
        ),
        0,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn linked_list_len_1() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            LinkedList a : [Nil, Cons a (LinkedList a)]

            one : LinkedList (Int *)
            one = Cons 1 Nil

            length : LinkedList a -> Int *
            length = \list ->
                when list is
                    Nil -> 0
                    Cons _ rest -> 1 + length rest

            main =
                length one
            "#
        ),
        1,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn linked_list_len_twice_1() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            LinkedList a : [Nil, Cons a (LinkedList a)]

            one : LinkedList (Int *)
            one = Cons 1 Nil

            length : LinkedList a -> Int *
            length = \list ->
                when list is
                    Nil -> 0
                    Cons _ rest -> 1 + length rest

            main =
                length one + length one
                "#
        ),
        2,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn linked_list_len_3() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            LinkedList a : [Nil, Cons a (LinkedList a)]

            three : LinkedList (Int *)
            three = Cons 3 (Cons 2 (Cons 1 Nil))

            length : LinkedList a -> Int *
            length = \list ->
                when list is
                    Nil -> 0
                    Cons _ rest -> 1 + length rest


            main =
                length three
            "#
        ),
        3,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn linked_list_sum_num_a() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            LinkedList a : [Nil, Cons a (LinkedList a)]

            three : LinkedList (Int *)
            three = Cons 3 (Cons 2 (Cons 1 Nil))


            sum : LinkedList (Num a) -> Num a
            sum = \list ->
                when list is
                    Nil -> 0
                    Cons x rest -> x + sum rest

            main =
                sum three
            "#
        ),
        3 + 2 + 1,
        i64
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn linked_list_sum_int() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            LinkedList a : [Nil, Cons a (LinkedList a)]

            zero : LinkedList (Int *)
            zero = Nil

            sum : LinkedList (Int a) -> Int a
            sum = \list ->
                when list is
                    Nil -> 0
                    Cons x rest -> x + sum rest

            main =
                sum zero
            "#
        ),
        0,
        i64
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn linked_list_map() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            LinkedList a : [Nil, Cons a (LinkedList a)]

            three : LinkedList (Int *)
            three = Cons 3 (Cons 2 (Cons 1 Nil))

            sum : LinkedList (Num a) -> Num a
            sum = \list ->
                when list is
                    Nil -> 0
                    Cons x rest -> x + sum rest

            map : (a -> b), LinkedList a -> LinkedList b
            map = \f, list ->
                when list is
                    Nil -> Nil
                    Cons x rest -> Cons (f x) (map f rest)

            main =
                sum (map (\_ -> 1) three)
            "#
        ),
        3,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn when_nested_maybe() {
    assert_evals_to!(
        indoc!(
            r"
            Maybe a : [Nothing, Just a]

            x : Maybe (Maybe (Int a))
            x = Just (Just 41)

            when x is
                Just (Just v) -> v + 0x1
                _ -> 0x1
                "
        ),
        42,
        i64
    );

    assert_evals_to!(
        indoc!(
            r"
            Maybe a : [Nothing, Just a]

            x : Maybe (Maybe (Int *))
            x = Just Nothing

            when x is
                Just (Just v) -> v + 0x1
                Just Nothing -> 0x2
                Nothing -> 0x1
                "
        ),
        2,
        i64
    );

    assert_evals_to!(
        indoc!(
            r"
            Maybe a : [Nothing, Just a]

            x : Maybe (Maybe (Int *))
            x = Nothing

            when x is
                Just (Just v) -> v + 0x1
                Just Nothing -> 0x2
                Nothing -> 0x1
                "
        ),
        1,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn when_peano() {
    assert_evals_to!(
        indoc!(
            r"
                Peano : [S Peano, Z]

                three : Peano
                three = S (S (S Z))

                when three is
                    S (S _) -> 1
                    S (_) -> 2
                    Z -> 3
                "
        ),
        1,
        i64
    );

    assert_evals_to!(
        indoc!(
            r"
                Peano : [S Peano, Z]

                three : Peano
                three = S Z

                when three is
                    S (S _) -> 1
                    S (_) -> 2
                    Z -> 3
                "
        ),
        2,
        i64
    );

    assert_evals_to!(
        indoc!(
            r"
                Peano : [S Peano, Z]

                three : Peano
                three = Z

                when three is
                    S (S _) -> 1
                    S (_) -> 2
                    Z -> 3
                "
        ),
        3,
        i64
    );
}

#[test]
// This doesn't work on Windows. If you make it return a Result.with_default 0 (so it's returning
// an integer instead of a Result), then it works on Windows, suggesting this is a C ABI issue.
// We should try this out on Windows again after making adjustments to the Result C ABI!
#[cfg(all(
    not(target_family = "windows"),
    any(feature = "gen-llvm", feature = "gen-wasm")
))]
#[should_panic(expected = "Roc failed with message: ")]
fn overflow_frees_list() {
    assert_evals_to!(
        indoc!(
            r"
            my_list = [1,2,3]

            # integer overflow; must use the list so it is defined before the overflow
            # the list will then be freed in a cleanup block
            n : I64
            n = 9_223_372_036_854_775_807 + (Num.int_cast (List.len my_list))

            index : U64
            index = Num.int_cast n

            List.get my_list index
                 "
        ),
        (3, 0),
        (i64, i8)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
#[should_panic(expected = "Roc failed with message: ")]
fn undefined_variable() {
    assert_evals_to!(
        indoc!(
            r"
                 if Bool.true then
                     x + z
                 else
                     y + z
                 "
        ),
        3,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
#[should_panic(expected = "User crash with message: \"a crash\"")]
fn a_crash() {
    assert_evals_to!(
        indoc!(
            r#"
            if Bool.true then
                crash "a crash"
            else
                0u64
            "#
        ),
        3,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
#[should_panic(expected = "Roc failed with message: ")]
fn annotation_without_body() {
    assert_evals_to!(
        indoc!(
            r"
            foo : Int *


            foo
            "
        ),
        3,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn simple_closure() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            x = 42

            f = \{} -> x


            main =
                f {}
            "#
        ),
        42,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn nested_closure() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            foo = \{} ->
                x = 41
                y = 1
                f = \{} -> x + y
                f

            main =
                g = foo {}
                g {}
            "#
        ),
        42,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn closure_in_list() {
    use roc_std::RocList;

    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            foo = \{} ->
                x = 41

                f = \{} -> x

                [f]

            main =
                items = foo {}

                items
            "#
        ),
        RocList::from_slice(&[41]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn specialize_closure() {
    use roc_std::RocList;

    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            foo = \{} ->
                x = 41
                y = [1]

                f = \{} -> x
                g = \{} -> x + Num.int_cast (List.len y)

                [f, g]

            apply = \f -> f {}

            main =
                items = foo {}

                List.map items apply
            "#
        ),
        RocList::from_slice(&[41, 42]),
        RocList<i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn io_poc_effect() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            Effect a := {} -> a

            succeed : a -> Effect a
            succeed = \x -> @Effect \{} -> x

            run_effect : Effect a -> a
            run_effect = \@Effect thunk -> thunk {}

            foo : Effect F64
            foo =
                succeed 1.23

            main : F64
            main =
                run_effect foo

            "#
        ),
        1.23,
        f64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn io_poc_desugared() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            # succeed : a -> ({} -> a)
            succeed = \x -> \_ -> x

            foo : Str -> F64
            foo =
                succeed 1.23

            # run_effect : ({} ->  a) -> a
            run_effect = \thunk -> thunk ""

            main : F64
            main =
                run_effect foo
            "#
        ),
        1.23,
        f64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn return_wrapped_function_a() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            Effect a := {} -> a

            foo : Effect {}
            foo = @Effect \{} -> {}

            main : Effect {}
            main = foo
            "#
        ),
        (),
        ()
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn return_wrapped_function_b() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"


            foo : { x: (I64 -> Str) }
            foo = { x:  (\_ -> "foobar") }

            main : { x:  (I64 -> Str) }
            main = foo
            "#
        ),
        (),
        ()
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn return_wrapped_closure() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            Effect a := {} -> a

            foo : Effect {}
            foo =
                x = 5

                @Effect (\{} -> if x > 3 then {} else {})

            main : Effect {}
            main = foo
            "#
        ),
        5,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn linked_list_is_singleton() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            ConsList a : [Cons a (ConsList a), Nil]

            empty : {} -> ConsList a
            empty = \{} -> Nil

            is_singleton : ConsList a -> Bool
            is_singleton = \list ->
                when list is
                    Cons _ Nil ->
                        Bool.true

                    _ ->
                        Bool.false

            main : Bool
            main =
                my_list : ConsList I64
                my_list = empty {}

                is_singleton my_list
            "#
        ),
        false,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn linked_list_is_empty_1() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            ConsList a : [Cons a (ConsList a), Nil]

            empty : {} -> ConsList a
            empty = \{} -> Nil

            is_empty : ConsList a -> Bool
            is_empty = \list ->
                when list is
                    Cons _ _ ->
                        Bool.false

                    Nil ->
                        Bool.true

            main : Bool
            main =
                my_list : ConsList U8
                my_list = empty {}

                is_empty my_list
            "#
        ),
        true,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn linked_list_is_empty_2() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            ConsList a : [Cons a (ConsList a), Nil]

            is_empty : ConsList a -> Bool
            is_empty = \list ->
                when list is
                    Cons _ _ ->
                        Bool.false

                    Nil ->
                        Bool.true

            main : Bool
            main =
                my_list : ConsList I64
                my_list = Cons 0x1 Nil

                is_empty my_list
            "#
        ),
        false,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn linked_list_singleton() {
    // verifies only that valid llvm is produced

    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            ConsList a : [Cons a (ConsList a), Nil]

            main : ConsList I64
            main = Cons 0x1 Nil
            "#
        ),
        0,
        usize,
        |_| 0
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn recursive_function_with_rigid() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            State a : { count : I64, x : a }

            foo : State a -> Int *
            foo = \state ->
                if state.count == 0 then
                    0
                else
                    1 + foo { count: state.count - 1, x: state.x }

            main : Int *
            main =
                foo { count: 3, x: {} }
            "#
        ),
        3,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn rbtree_insert() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            NodeColor : [Red, Black]

            RedBlackTree k v : [Node NodeColor k v (RedBlackTree k v) (RedBlackTree k v), Empty]

            Key k : Num k

            insert : Key k, v, RedBlackTree (Key k) v -> RedBlackTree (Key k) v
            insert = \key, value, dict ->
                when insert_help key value dict is
                    Node Red k v l r ->
                        Node Black k v l r

                    x ->
                        x

            insert_help : (Key k), v, RedBlackTree (Key k) v -> RedBlackTree (Key k) v
            insert_help = \key, value, dict ->
              when dict is
                Empty ->
                  # New nodes are always red. If it violates the rules, it will be fixed
                  # when balancing.
                  Node Red key value Empty Empty

                Node n_color n_key n_value n_left n_right ->
                  when Num.compare key n_key is
                    LT ->
                      balance n_color n_key n_value (insert_help key value n_left) n_right

                    EQ ->
                      Node n_color n_key value n_left n_right

                    GT ->
                      balance n_color n_key n_value n_left (insert_help key value n_right)

            balance : NodeColor, k, v, RedBlackTree k v, RedBlackTree k v -> RedBlackTree k v
            balance = \color, key, value, left, right ->
              when right is
                Node Red r_k r_v r_left r_right ->
                  when left is
                    Node Red l_k l_v l_left l_right ->
                      Node
                        Red
                        key
                        value
                        (Node Black l_k l_v l_left l_right)
                        (Node Black r_k r_v r_left r_right)

                    _ ->
                      Node color r_k r_v (Node Red key value left r_left) r_right

                _ ->
                  when left is
                    Node Red l_k l_v (Node Red ll_k ll_v ll_left ll_right) l_right ->
                      Node
                        Red
                        l_k
                        l_v
                        (Node Black ll_k ll_v ll_left ll_right)
                        (Node Black key value l_right right)

                    _ ->
                      Node color key value left right

            show : RedBlackTree I64 {} -> Str
            show = \tree ->
                when tree is
                    Empty -> "Empty"
                    Node _ _ _ _ _ -> "Node"


            main : Str
            main =
                show (insert 0 {} Empty)
            "#
        ),
        RocStr::from("Node"),
        RocStr
    );
}

#[test]
#[cfg(all(
    any(feature = "gen-llvm", feature = "gen-wasm"),
    not(feature = "gen-llvm-wasm")
))]
fn rbtree_balance_3() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            RedBlackTree k : [Node k (RedBlackTree k) (RedBlackTree k), Empty]

            balance : k, RedBlackTree k -> RedBlackTree k
            balance = \key, left ->
                Node key left Empty

            main : RedBlackTree (Int *)
            main =
                balance 0 Empty
            "#
        ),
        false,
        usize,
        |x: usize| x == 0
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
#[ignore]
fn rbtree_layout_issue() {
    // there is a flex var in here somewhere that blows up layout creation
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            NodeColor : [Red, Black]

            RedBlackTree k v : [Node NodeColor k v (RedBlackTree k v) (RedBlackTree k v), Empty]

            # balance : NodeColor, k, v, RedBlackTree k v -> RedBlackTree k v
            balance = \color, key, value, right ->
              when right is
                Node Red _ _ r_left r_right ->
                    Node color key value r_left r_right


                _ ->
                    Empty

            show : RedBlackTree * * -> Str
            show = \tree ->
                when tree is
                    Empty -> "Empty"
                    Node _ _ _ _ _ -> "Node"

            zero : I64
            zero = 0

            main : Str
            main = show (balance Red zero zero Empty)
            "#
        ),
        RocStr::from("Empty"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
#[ignore]
fn rbtree_balance_mono_problem() {
    // because of how the function is written, only `Red` is used and so in the function's
    // type, the first argument is a unit and dropped. Apparently something is weird with
    // constraint generation where the specialization required by `main` does not fix the
    // problem. As a result, the first argument is dropped and we run into issues down the line
    //
    // concretely, the `r_right` symbol will not be defined
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            NodeColor : [Red, Black]

            RedBlackTree k v : [Node NodeColor k v (RedBlackTree k v) (RedBlackTree k v), Empty]

            # balance : NodeColor, k, v, RedBlackTree k v, RedBlackTree k v -> RedBlackTree k v
            balance = \color, key, value, left, right ->
              when right is
                Node Red r_k r_v r_left r_right ->
                  when left is
                    Node Red l_k l_v l_left l_right ->
                      Node
                        Red
                        key
                        value
                        (Node Black l_k l_v l_left l_right)
                        (Node Black r_k r_v r_left r_right)

                    _ ->
                      Node color r_k r_v (Node Red key value left r_left) r_right

                _ ->
                    Empty

            show : RedBlackTree * * -> Str
            show = \tree ->
                when tree is
                    Empty -> "Empty"
                    Node _ _ _ _ _ -> "Node"


            main : Str
            main = show (balance Red 0 0 Empty Empty)
            "#
        ),
        RocStr::from("Empty"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn rbtree_balance_full() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            NodeColor : [Red, Black]

            RedBlackTree k v : [Node NodeColor k v (RedBlackTree k v) (RedBlackTree k v), Empty]

            balance : NodeColor, k, v, RedBlackTree k v, RedBlackTree k v -> RedBlackTree k v
            balance = \color, key, value, left, right ->
              when right is
                Node Red r_k r_v r_left r_right ->
                  when left is
                    Node Red l_k l_v l_left l_right ->
                      Node
                        Red
                        key
                        value
                        (Node Black l_k l_v l_left l_right)
                        (Node Black r_k r_v r_left r_right)

                    _ ->
                      Node color r_k r_v (Node Red key value left r_left) r_right

                _ ->
                  when left is
                    Node Red l_k l_v (Node Red ll_k ll_v ll_left ll_right) l_right ->
                      Node
                        Red
                        l_k
                        l_v
                        (Node Black ll_k ll_v ll_left ll_right)
                        (Node Black key value l_right right)

                    _ ->
                      Node color key value left right

            main : RedBlackTree F64 F64
            main =
                balance Red 0 0 Empty Empty
            "#
        ),
        true,
        usize,
        |x| x != 0
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn nested_pattern_match_two_ways() {
    // exposed an issue in the ordering of pattern match checks when ran with `--release` mode
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            ConsList a : [Cons a (ConsList a), Nil]

            balance : ConsList (Int *) -> Int *
            balance = \right ->
              when right is
                Cons 1 foo ->
                    when foo is
                        Cons 1 _ -> 3
                        _ -> 3
                _ -> 3

            main : Int *
            main =
                when balance Nil is
                    _ -> 3
            "#
        ),
        3,
        i64
    );

    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            ConsList a : [Cons a (ConsList a), Nil]

            balance : ConsList (Int *) -> Int *
            balance = \right ->
              when right is
                Cons 1 (Cons 1 _) -> 3
                _ -> 3

            main : Int *
            main =
                when balance Nil is
                    _ -> 3
            "#
        ),
        3,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn linked_list_guarded_double_pattern_match() {
    // the important part here is that the first case (with the nested Cons) does not match
    // TODO this also has undefined behavior
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            ConsList a : [Cons a (ConsList a), Nil]

            balance : ConsList (Int *) -> Int *
            balance = \right ->
              when right is
                Cons 1 foo ->
                    when foo is
                        Cons 1 _ -> 3
                        _ -> 3
                _ -> 3

            main : Int *
            main =
                when balance Nil is
                    _ -> 3
            "#
        ),
        3,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn linked_list_double_pattern_match() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            ConsList a : [Cons a (ConsList a), Nil]

            foo : ConsList (Int a) -> Int a
            foo = \list ->
                when list is
                    Cons _ (Cons x _) -> x
                    _ -> 0

            main : Int *
            main =
                foo (Cons 1 (Cons 32 Nil))
            "#
        ),
        32,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
// dev backend: this test somehow corrupts the errors vector ?!
fn binary_tree_double_pattern_match() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            BTree : [Node BTree BTree, Leaf I64]

            foo : BTree -> I64
            foo = \btree ->
                when btree is
                    Node (Node (Leaf x) _) _ -> x
                    _ -> 1

            main : I64
            main =
                foo (Node (Node (Leaf 32) (Leaf 2)) (Leaf 3))
            "#
        ),
        32,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn unified_empty_closure_bool() {
    // none of the Closure tags will have a payload
    // this was not handled correctly in the past
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            foo = \{} ->
                when A is
                    A -> (\_ -> 1.23f64)
                    B -> (\_ -> 1.23f64)

            main : F64
            main =
                (foo {}) 0
            "#
        ),
        1.23,
        f64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn unified_empty_closure_byte() {
    // none of the Closure tags will have a payload
    // this was not handled correctly in the past
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            foo = \{} ->
                when A is
                    A -> (\_ -> 1.23f64)
                    B -> (\_ -> 1.23f64)
                    C -> (\_ -> 1.23)

            main : F64
            main =
                (foo {}) 0
            "#
        ),
        1.23,
        f64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn wildcard_rigid() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            Effect a := {} -> a

            MyTask a err : Effect (Result a err)

            # this failed because of the `*`, but worked with `err`
            always : a -> MyTask a *
            always = \x ->
                inner = \{} -> (Ok x)

                @Effect inner


            main : MyTask {} (Frac *)
            main = always {}
            "#
        ),
        (),
        ()
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn alias_of_alias_with_type_arguments() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            Effect a := a

            MyTask a err : Effect (Result a err)

            always : a -> MyTask a *
            always = \x ->
                inner = (Ok x)

                @Effect inner


            main : MyTask {} F64
            main = always {}
            "#
        ),
        (),
        (f64, u8),
        |_| ()
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
#[ignore]
fn todo_bad_error_message() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            Effect a := {} -> a

            effect_always : a -> Effect a
            effect_always = \x ->
                inner = \{} -> x

                @Effect inner

            effect_after : Effect a, (a -> Effect b) -> Effect b
            effect_after = \(@Effect thunk), transform -> transform (thunk {})

            MyTask a err : Effect (Result a err)

            always : a -> MyTask a (Frac *)
            always = \x -> effect_always (Ok x)

            # the problem is that this restricts to `MyTask {} *`
            fail : err -> MyTask {} err
            fail = \x -> effect_always (Err x)

            after : MyTask a err, (a -> MyTask b err) -> MyTask b err
            after = \task, transform ->
                effect_after task \res ->
                    when res is
                        Ok x -> transform x
                        # but here it must be `forall b. MyTask b {}`
                        Err e -> fail e

            main : MyTask {} (Frac *)
            main =
                after (always "foo") (\_ -> always {})
            "#
        ),
        0,
        i64,
        |_| 0
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn hof_conditional() {
    // exposed issue with the if condition being just a symbol
    assert_evals_to!(
        indoc!(
            r"
                pass_true = \f -> f Bool.true

                pass_true (\true_val -> if true_val then Bool.false else Bool.true)
            "
        ),
        0,
        u8
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
#[should_panic(
    expected = "Roc failed with message: \"Shadowing { original_region: @55-56, shadow: @88-89 Ident"
)]
fn pattern_shadowing() {
    assert_evals_to!(
        indoc!(
            r"
            x = 4

            when 4 is
                x -> x
            "
        ),
        0,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
#[ignore]
#[should_panic(expected = "")]
fn unsupported_pattern_str_interp() {
    assert_evals_to!(
        indoc!(
            r"
            { x: 4 } = { x : 4 }

            x
            "
        ),
        0,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
#[ignore]
fn fingertree_basic() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            Some a : [One a, Two a a, Three a a a]

            Tuple a : [Pair a a, Triple a a a]

            # a FingerTree implementation
            Seq a : [Nil, Unit a, More (Some a) (Seq (Tuple a)) (Some a)]

            # cons : a, Seq a -> Seq a
            cons = \x, s ->
                when s is
                    Nil -> Unit x
                    Unit y -> More (One x) Nil (One y)
                    More some q u ->
                        when some is
                            One y -> More (Two x y) q u
                            Two y z -> More (Three x y z) q u
                            Three y z w -> More (Two x y) (cons_tuple (Pair z w) q) u

            cons_tuple : Tuple a, Seq (Tuple a) -> Seq (Tuple a)
            cons_tuple = \a, b -> cons a b

            main : Bool
            main =
                when cons 0x1 Nil is
                    Unit 1 -> Bool.true
                    _ -> Bool.false
            "#
        ),
        true,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn case_or_pattern() {
    // the `0` branch body should only be generated once in the future
    // it is currently duplicated
    assert_evals_to!(
        indoc!(
            r"
            x : [Red, Green, Blue]
            x = Red

            when x is
                Red | Green -> 0
                Blue -> 1
            "
        ),
        0,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
#[ignore]
fn rosetree_basic() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            Tree a : [Tree a (List (Tree a))]

            singleton : a -> Tree a
            singleton = \x -> Tree x []

            main : Bool
            main =
                x : Tree F64
                x = singleton 3
                when x is
                    Tree 3.0 _ -> Bool.true
                    _ -> Bool.false
            "#
        ),
        true,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn case_jump() {
    // the decision tree will generate a jump to the `1` branch here
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            ConsList a : [Cons a (ConsList a), Nil]

            x : ConsList I64
            x = Nil

            main =
                when Pair x x is
                    Pair Nil _ -> 1
                    Pair _ Nil -> 2
                    Pair (Cons a _) (Cons b _) -> a + b + 3
                    Pair _ _ -> 4
            "#
        ),
        1,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn nullable_eval_cfold() {
    // the decision tree will generate a jump to the `1` branch here
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            Expr : [Var, Val I64, Add Expr Expr, Mul Expr Expr]

            mk_expr : I64, I64 -> Expr
            mk_expr = \n , v ->
                when n is
                    0 -> if v == 0 then Var else Val v
                    _ -> Add (mk_expr (n-1) (v+1)) (mk_expr (n-1) (max (v-1) 0))

            max : I64, I64 -> I64
            max = \a, b -> if a > b then a else b

            eval : Expr -> I64
            eval = \e ->
                when e is
                    Var   -> 0
                    Val v -> v
                    Add l r -> eval l + eval r
                    Mul l r -> eval l * eval r

            main : I64
            main = eval (mk_expr 3 1)
            "#
        ),
        11,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn nested_switch() {
    crate::helpers::with_larger_debug_stack(||
        // exposed bug with passing the right symbol/layout down into switch branch generation
        // This is also the only test_gen test that exercises Reset/Reuse (as of Aug 2022)
        assert_evals_to!(
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
                            Pair (Val a) (ZAdd x (Val b)) -> ZAdd (Val (a+b)) x
                            Pair _ _                     -> ZAdd e1 e2


                    _ -> e

            expr : Expr
            expr = ZAdd (Val 3) (ZAdd (Val 4) (Val 5))

            main : I64
            main = eval (const_folding expr)
            "#
        ),
        12,
        i64
    ));
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn count_deriv_x() {
    // exposed bug with basing the block_of_memory on a specific (smaller) tag layout
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            Expr : [Ln Expr, Pow Expr Expr, Var Str]

            count : Expr -> I64
            count = \expr ->
                when expr is
                    (Var _) -> 1
                    (Pow f g) -> count f + count g
                    (Ln f)    -> count f

            main : I64
            main = count (Var "x")
            "#
        ),
        1,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn deriv_pow() {
    // exposed bug with ordering of variable declarations before switch
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            Expr : [Ln Expr, Pow Expr Expr, Var Str, Val I64]

            count : Expr -> I64
            count = \expr ->
                when expr is
                    (Var _) -> 1
                    (Val n) -> n
                    (Pow f g) -> count f + count g
                    (Ln f)    -> count f

            pow : Expr, Expr -> Expr
            pow = \a,b ->
                when Pair a b is
                    Pair (Val _) (Val _) -> Val -1
                    Pair _       (Val 0) -> Val 1
                    Pair f       (Val 1) -> f
                    Pair (Val 0) _       -> Val 0
                    Pair f       g       -> Pow f g

            main : I64
            main = count (pow (Var "x") (Var "x"))
            "#
        ),
        2,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn multiple_increment() {
    // the `leaf` value will be incremented multiple times at once
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"


            Color : [Red, Black]

            Tree a b : [Leaf, Node Color (Tree a b) a b (Tree a b)]

            Map : Tree I64 Bool

            main : I64
            main =
                leaf : Map
                leaf = Leaf

                m : Map
                m = Node Black (Node Black leaf 10 Bool.false leaf) 11 Bool.false (Node Black leaf 12 Bool.false (Node Red leaf 13 Bool.false leaf))

                when m is
                    Leaf -> 0
                    Node _ _ _ _ _ -> 1
            "#
        ),
        1,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn switch_fuse_rc_non_exhaustive() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            Foo : [A I64 Foo, B I64 Foo, C I64 Foo, Empty]

            sum : Foo, I64 -> I64
            sum = \foo, accum ->
                when foo is
                    A x resta -> sum resta (x + accum)
                    B x restb -> sum restb (x + accum)
                    # Empty -> accum
                    # C x restc -> sum restc (x + accum)
                    _ -> accum

            main : I64
            main =
                A 1 (B 2 (C 3 Empty))
                    |> sum 0
            "#
        ),
        3,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn switch_fuse_rc_exhaustive() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            Foo : [A I64 Foo, B I64 Foo, C I64 Foo, Empty]

            sum : Foo, I64 -> I64
            sum = \foo, accum ->
                when foo is
                    A x resta -> sum resta (x + accum)
                    B x restb -> sum restb (x + accum)
                    C x restc -> sum restc (x + accum)
                    Empty -> accum

            main : I64
            main =
                A 1 (B 2 (C 3 Empty))
                    |> sum 0
            "#
        ),
        6,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn build_then_apply_closure() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            main : Str
            main =
                x = "long string that is malloced"

                (\_ -> x) {}
            "#
        ),
        RocStr::from("long string that is malloced"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn expanded_result() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            a : Result I64 Str
            a = Ok 4

            after = \x, f ->
                when x is
                    Ok v -> f v
                    Err e -> Err e

            main : I64
            main =
                helper = after a (\x -> Ok x)

                when helper is
                    Ok v -> v
                    Err _ -> 0

            "#
        ),
        4,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
#[should_panic(expected = "Shadowing { original_region: @55-56, shadow: @72-73 Ident")]
fn function_malformed_pattern() {
    assert_evals_to!(
        indoc!(
            r"
                x = 3

                (\x -> x) 42
            "
        ),
        3,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
#[ignore = "causes alias analysis panics, should roc_panic"]
fn call_invalid_layout() {
    assert_evals_to!(
        indoc!(
            r"
                f : I64 -> I64
                f = \x -> x

                f {}
            "
        ),
        3,
        i64,
        |x| x,
        true
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn increment_or_double_closure() {
    assert_evals_to!(
        indoc!(
            r#"
                app "test" provides [main] to "./platform"


                apply : (a -> a), a -> a
                apply = \f, x -> f x

                main =
                    one : I64
                    one = 1

                    two : I64
                    two = 2

                    b : Bool
                    b = Bool.true

                    increment : I64 -> I64
                    increment = \x -> x + one

                    double : I64 -> I64
                    double = \x -> if b then x * two else x

                    f = (if Bool.true then increment else double)

                    apply f 42
            "#
        ),
        43,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn module_thunk_is_function() {
    assert_evals_to!(
        indoc!(
            r#"
                app "test" provides [main] to "./platform"

                main = helper "foo" "bar"
                helper = Str.concat
            "#
        ),
        RocStr::from("foobar"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn pass_through_unresolved_type_variable() {
    assert_evals_to!(
        indoc!(
            r#"
                app "test" provides [main] to "./platform"

                main : Str
                main =
                    (accept \x -> x) "B"


                accept : * -> (b -> b)
                accept = \_ ->
                    \input -> input
            "#
        ),
        RocStr::from("B"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn pattern_match_empty_record() {
    assert_evals_to!(
        indoc!(
            r#"
                app "test" provides [main] to "./platform"

                main : I64
                main =
                    when {} is
                        {} -> 0

            "#
        ),
        0,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn pattern_match_unit_tag() {
    assert_evals_to!(
        indoc!(
            r#"
                app "test" provides [main] to "./platform"

                unit : [Unit]
                unit = Unit

                main : I64
                main =
                    when unit is
                        Unit -> 0

            "#
        ),
        0,
        i64
    );
}

// see for why this is disabled on wasm32 https://github.com/roc-lang/roc/issues/1687
#[cfg(not(feature = "gen-llvm-wasm"))]
#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn mirror_llvm_alignment_padding() {
    // see https://github.com/roc-lang/roc/issues/1569
    assert_evals_to!(
        indoc!(
            r#"
                app "test" provides [main] to "./platform"

                main : Str
                main =
                    p1 = {name : "test1", test: 1 == 1 }

                    List.map [p1, p1] (\{ test } -> if test  then "pass" else "fail")
                       |> Str.join_with "\n"

            "#
        ),
        RocStr::from("pass\npass"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn lambda_set_bool() {
    assert_evals_to!(
        indoc!(
            r#"
                app "test" provides [main] to "./platform"

                p1 = (\u -> u == 97)
                p2 = (\u -> u == 98)

                main : I64
                main =
                    one_of_result = List.map [p1, p2] (\p -> p 42)

                    when one_of_result is
                        _ -> 32

            "#
        ),
        32,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn lambda_set_byte() {
    assert_evals_to!(
        indoc!(
            r#"
                app "test" provides [main] to "./platform"

                p1 = (\u -> u == 97)
                p2 = (\u -> u == 98)
                p3 = (\u -> u == 99)

                main : I64
                main =
                    one_of_result = List.map [p1, p2, p3] (\p -> p 42)

                    when one_of_result is
                        _ -> 32

            "#
        ),
        32,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn lambda_set_struct_byte() {
    assert_evals_to!(
        indoc!(
            r#"
                app "test" provides [main] to "./platform"


                main : I64
                main =
                    r : [Red, Green, Blue]
                    r = Red

                    p1 = (\u -> r == u)
                    foobarbaz = (\p -> p Green)
                    one_of_result = List.map [p1, p1] foobarbaz

                    when one_of_result is
                        _ -> 32

            "#
        ),
        32,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn lambda_set_enum_byte_byte() {
    assert_evals_to!(
        indoc!(
            r#"
                app "test" provides [main] to "./platform"


                main : I64
                main =
                    r : [Red, Green, Blue]
                    r = Red

                    g : [Red, Green, Blue]
                    g = Green

                    p1 = (\u -> r == u)
                    p2 = (\u -> g == u)
                    one_of_result = List.map [p1, p2] (\p -> p Green)

                    when one_of_result is
                        _ -> 32

            "#
        ),
        32,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn list_walk_until() {
    // see https://github.com/roc-lang/roc/issues/1576
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"


            satisfy_a : {} -> List {}
            satisfy_a = \_ -> []

            one_of_result =
                List.walk_until [satisfy_a] [] \_, _ -> Break []

            main =
                when one_of_result is
                    _ -> 32
            "#
        ),
        32,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn int_literal_not_specialized_with_annotation() {
    // see https://github.com/roc-lang/roc/issues/1600
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            main =
                satisfy : (U8 -> Str) -> Str
                satisfy = \_ -> "foo"

                my_eq : a, a -> Str
                my_eq = \_, _ -> "bar"

                p1 : Num * -> Str
                p1 = (\u -> my_eq u 64)

                when satisfy p1 is
                    _ -> 32
            "#
        ),
        32,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn int_literal_not_specialized_no_annotation() {
    // see https://github.com/roc-lang/roc/issues/1600
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            main =
                satisfy : (U8 -> Str) -> Str
                satisfy = \_ -> "foo"

                my_eq : a, a -> Str
                my_eq = \_, _ -> "bar"

                p1 = (\u -> my_eq u 64)

                when satisfy p1 is
                    _ -> 32
            "#
        ),
        32,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn unresolved_tvar_when_capture_is_unused() {
    // see https://github.com/roc-lang/roc/issues/1585
    assert_evals_to!(
        indoc!(
            r#"
                app "test" provides [main] to "./platform"

                main : I64
                main =
                    r : U8
                    r = 1

                    p1 = (\_ -> r == 1)
                    one_of_result = List.map [p1] (\p -> p Green)

                    when one_of_result is
                        _ -> 32

            "#
        ),
        32,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
#[should_panic(expected = "Roc failed with message: ")]
fn value_not_exposed_hits_panic() {
    assert_evals_to!(
        indoc!(
            r#"
                app "test" provides [main] to "./platform"

                main : I64
                main =
                    Str.to_int 32
            "#
        ),
        32,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn mix_function_and_closure() {
    // see https://github.com/roc-lang/roc/pull/1706
    assert_evals_to!(
        indoc!(
            r#"
                app "test" provides [main] to "./platform"

                # foo does not capture any variables
                # but through unification will get a lambda set that does store information
                # we must handle that correctly
                foo = \x -> x

                bar = \y -> \_ -> y

                main : Str
                main =
                    (if 1 == 1 then foo else (bar "nope nope nope")) "hello world"
            "#
        ),
        RocStr::from("hello world"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn mix_function_and_closure_level_of_indirection() {
    // see https://github.com/roc-lang/roc/pull/1706
    assert_evals_to!(
        indoc!(
            r#"
                app "test" provides [main] to "./platform"

                foo = \x -> x

                bar = \y -> \_ -> y

                f = (if 1 == 1 then foo else (bar "nope nope nope"))

                main : Str
                main =
                    f "hello world"
            "#
        ),
        RocStr::from("hello world"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
#[cfg_attr(debug_assertions, ignore)] // this test stack-overflows the compiler in debug mode
fn do_pass_bool_byte_closure_layout() {
    // see https://github.com/roc-lang/roc/pull/1706
    // the distinction is actually important, dropping that info means some functions just get
    // skipped
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            ## PARSER

            Parser a : List U8 -> List [Pair a (List U8)]


            ## ANY

            # If successful, the any parser consumes one character

            any: Parser U8
            any = \inp ->
               when List.first inp is
                 Ok u -> [Pair u (List.drop_first inp 1)]
                 _ -> []



            ## SATISFY

            satisfy : (U8 -> Bool) -> Parser U8
            satisfy = \predicate ->
                \input ->
                    walker = \accum, (Pair u rest) ->
                        if predicate u then
                            Break [Pair u rest]

                        else
                            Break accum

                    List.walk_until (any input) [] walker



            one_of : List (Parser a) -> Parser a
            one_of = \parserList ->
                \input ->
                    walker = \accum, p ->
                        output = p input
                        if List.len output == 1 then
                            Break output

                        else
                            Continue accum

                    List.walk_until parserList [] walker


            satisfy_a = satisfy (\u -> u == 97) # recognize 97
            satisfy_b = satisfy (\u -> u == 98) # recognize 98

            test1 = if List.len ((one_of [satisfy_a, satisfy_b]) [97, 98, 99, 100] ) == 1  then "PASS" else "FAIL"
            test2 = if List.len ((one_of [satisfy_a, satisfy_b]) [98, 99, 100, 97] ) == 1  then "PASS" else "FAIL"
            test3 = if List.len ((one_of [satisfy_b , satisfy_a]) [98, 99, 100, 97] ) == 1  then "PASS" else "FAIL"
            test4 = if List.len ((one_of [satisfy_a, satisfy_b]) [99, 100, 101] ) == 0  then "PASS" else "FAIL"


            main : Str
            main = [test1, test2, test3, test4] |> Str.join_with ", "
       "#
        ),
        RocStr::from("PASS, PASS, PASS, PASS"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn nested_rigid_list() {
    assert_evals_to!(
        indoc!(
            r#"
                app "test" provides [main] to "./platform"

                foo : List a -> List a
                foo = \list ->
                    p2 : List a
                    p2 = list

                    p2

                main =
                    when foo [] is
                        _ -> "hello world"
            "#
        ),
        RocStr::from("hello world"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev"))]
fn nested_rigid_alias() {
    assert_evals_to!(
        indoc!(
            r#"
                app "test" provides [main] to "./platform"

                Identity a := a

                foo : Identity a -> Identity a
                foo = \list ->
                    p2 : Identity a
                    p2 = list

                    p2

                main =
                    when foo (@Identity "foo") is
                        _ -> "hello world"
            "#
        ),
        RocStr::from("hello world"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev"))]
fn nested_rigid_tag_union() {
    assert_evals_to!(
        indoc!(
            r#"
                app "test" provides [main] to "./platform"

                foo : [Identity a] -> [Identity a]
                foo = \list ->
                    p2 : [Identity a]
                    p2 = list

                    p2

                main =
                    when foo (Identity "foo") is
                        _ -> "hello world"
            "#
        ),
        RocStr::from("hello world"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn call_that_needs_closure_parameter() {
    // here both p2 is lifted to the top-level, which means that `list` must be
    // passed to it from `many_aux`.
    assert_evals_to!(
        indoc!(
            r#"
            Step state a : [Loop state, Done a]

            many_aux : List a -> [Pair (Step (List a) (List a))]
            many_aux = \list ->
                    p2 = \_ -> Pair (Done list)

                    p2 "foo"

            many_aux_test =  (many_aux []) == Pair (Loop [97])

            run_test = \t -> if t then "PASS" else "FAIL"

            run_test many_aux_test
            "#
        ),
        RocStr::from("FAIL"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn alias_defined_out_of_order() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            main : Foo
            main = "foo"

            Foo : Bar
            Bar : Str

            "#
        ),
        RocStr::from("foo"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn recursively_build_effect() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            greeting =
                hi = "Hello"
                name = "World"

                "$(hi), $(name)!"

            main =
                when nest_help 4 is
                    _ -> greeting

            nest_help : I64 -> XEffect {}
            nest_help = \m ->
                when m is
                    0 ->
                        always {}

                    _ ->
                        always {} |> after \_ -> nest_help (m - 1)


            XEffect a := {} -> a

            always : a -> XEffect a
            always = \x -> @XEffect (\{} -> x)

            after : XEffect a, (a -> XEffect b) -> XEffect b
            after = \(@XEffect e), toB ->
                @XEffect \{} ->
                    when toB (e {}) is
                        @XEffect e2 ->
                            e2 {}
            "#
        ),
        RocStr::from("Hello, World!"),
        RocStr
    );
}

#[test]
#[ignore = "TODO; currently generates bad code because `a` isn't specialized inside the closure."]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn polymophic_expression_captured_inside_closure() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            as_u8 : U8 -> U8
            as_u8 = \_ -> 30

            main =
                a = 15
                f = \{} ->
                    as_u8 a

                f {}
            "#
        ),
        30,
        u8
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn issue_2322() {
    assert_evals_to!(
        indoc!(
            r"
            double = \x -> x * 2
            double_bind = \x -> (\_ -> double x)
            double_three = double_bind 3
            double_three {}
            "
        ),
        6,
        i64
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn box_and_unbox_small_string() {
    assert_evals_to!(
        indoc!(
            r#"
            "short"
                |> Box.box
                |> Box.unbox
            "#
        ),
        RocStr::from("short"),
        RocStr
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn box_and_unbox_big_string() {
    assert_evals_to!(
        indoc!(
            r#"
            Str.concat "Leverage " "agile frameworks to provide a robust synopsis for high level overviews"
                |> Box.box
                |> Box.unbox
            "#
        ),
        RocStr::from(
            "Leverage agile frameworks to provide a robust synopsis for high level overviews"
        ),
        RocStr
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn box_and_unbox_nonrecursive_tag() {
    assert_evals_to!(
        indoc!(
            r"
            result : Result U64 U64
            result = Ok 42

            result
            |> Box.box
            |> Box.unbox
            "
        ),
        roc_std::RocResult::ok(42),
        roc_std::RocResult<u64, u64>
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn box_num() {
    assert_evals_to!("Box.box 123u64", RocBox::new(123), RocBox<u64>)
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn box_record_2_u64() {
    assert_evals_to!(
        "Box.box { x: 1u64, y: 2u64 }",
        RocBox::new((1u64, 2u64)),
        RocBox<(u64, u64)>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn box_record_3_u64() {
    assert_evals_to!(
        "Box.box { x: 1u64, y: 2u64, z: 3u64 }",
        RocBox::new((1u64, 2u64, 3u64)),
        RocBox<(u64, u64, u64)>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn box_str() {
    assert_evals_to!(
        "Box.box \"short\"",
        RocBox::new(RocStr::from("short")),
        RocBox<RocStr>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn box_and_unbox_u64() {
    assert_evals_to!("Box.unbox (Box.box (123u64))", 123, u64)
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn box_and_unbox_u32() {
    assert_evals_to!("Box.unbox (Box.box (123u32))", 123, u32)
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn box_and_unbox_u16() {
    assert_evals_to!("Box.unbox (Box.box (123u16))", 123, u16)
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn box_and_unbox_u8() {
    assert_evals_to!("Box.unbox (Box.box (123u8))", 123, u8)
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn box_and_unbox_bool() {
    assert_evals_to!("Box.unbox (Box.box (Bool.true))", true, bool)
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn box_and_unbox_f64() {
    assert_evals_to!("Box.unbox (Box.box (123.0f64))", 123.0, f64)
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn box_and_unbox_f32() {
    assert_evals_to!("Box.unbox (Box.box (123.0f32))", 123.0, f32)
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn box_and_unbox_record_2_u64() {
    assert_evals_to!(
        indoc!(
            r"
            Box.unbox (Box.box { a: 15u64, b: 27u64 })
            "
        ),
        (15, 27),
        (u64, u64)
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn box_and_unbox_record_3_u64() {
    assert_evals_to!(
        indoc!(
            r"
            Box.unbox (Box.box { a: 15u64, b: 27u64, c: 34u64 })
            "
        ),
        (15, 27, 34),
        (u64, u64, u64)
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn box_and_unbox_record_2_u8() {
    assert_evals_to!(
        indoc!(
            r"
            Box.unbox (Box.box { a: 15u8, b: 27u8 })
            "
        ),
        (15, 27),
        (u8, u8)
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn box_and_unbox_record_3_u8() {
    assert_evals_to!(
        indoc!(
            r"
            Box.unbox (Box.box { a: 15u8, b: 27u8, c: 34u8 })
            "
        ),
        (15, 27, 34),
        (u8, u8, u8)
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn box_and_unbox_tag_union() {
    assert_evals_to!(
        indoc!(
            r"
            v : [A U8, B U8] # usually stack allocated
            v = B 27u8

            Box.unbox (Box.box v)
            "
        ),
        (27, 1),
        (u8, u8)
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn closure_called_in_its_defining_scope() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            main : Str
            main =
                g : Str
                g = "hello world"

                get_g : {} -> Str
                get_g = \{} -> g

                get_g {}
            "#
        ),
        RocStr::from("hello world"),
        RocStr
    )
}

#[test]
#[ignore]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn issue_2894() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            main : U32
            main =
                g : { x : U32 }
                g = { x: 1u32 }

                get_g : {} -> { x : U32 }
                get_g = \{} -> g

                h : {} -> U32
                h = \{} -> (get_g {}).x

                h {}
            "#
        ),
        1u32,
        u32
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn polymorphic_def_used_in_closure() {
    assert_evals_to!(
        indoc!(
            r"
            a : I64 -> _
            a = \g ->
                f = { r: g, h: 32 }

                h1 : U64
                h1 = (\{} -> f.h) {}
                h1
            a 1
            "
        ),
        32,
        u64
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn polymorphic_lambda_set_usage() {
    assert_evals_to!(
        indoc!(
            r"
            id1 = \x -> x
            id2 = \y -> y
            id = if Bool.true then id1 else id2

            id 9u8
            "
        ),
        9,
        u8
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn polymorphic_lambda_set_multiple_specializations() {
    assert_evals_to!(
        indoc!(
            r"
            id1 = \x -> x
            id2 = \y -> y
            id = \z ->
                f = if Bool.true then id1 else id2
                f z

            (id 9u8) + Num.to_u8 (id 16u16)
            "
        ),
        25,
        u8
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_map2_conslist() {
    // this had an RC problem, https://github.com/roc-lang/roc/issues/2968
    assert_evals_to!(
        indoc!(
            r#"
            ConsList a : [Nil, Cons a (ConsList a)]

            x : List (ConsList Str)
            x = List.map2 [] [Nil] Cons

            when List.first x is
                _ -> ""
            "#
        ),
        RocStr::default(),
        RocStr
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn mutual_recursion_top_level_defs() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [ main ] to "./platform"

            is_even = \n ->
                when n is
                    0 -> Bool.true
                    1 -> Bool.false
                    _ -> is_odd (n - 1)

            is_odd = \n ->
                when n is
                    0 -> Bool.false
                    1 -> Bool.true
                    _ -> is_even (n - 1)

            main = is_odd 11
            "#
        ),
        true,
        bool
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn polymorphic_lambda_captures_polymorphic_value() {
    assert_evals_to!(
        indoc!(
            r"
            x = 2

            f1 = \_ -> x

            f = if Bool.true then f1 else f1
            f {}
            "
        ),
        2,
        u64
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn lambda_capture_niche_u64_vs_u8_capture() {
    assert_evals_to!(
        indoc!(
            r"
            capture : _ -> ({} -> Str)
            capture = \val ->
                \{} ->
                    Num.to_str val

            x : Bool
            x = Bool.true

            fun =
                if x then
                    capture 123u64
                else
                    capture 18u8

            fun {}
            "
        ),
        RocStr::from("123"),
        RocStr
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn lambda_capture_niches_with_other_lambda_capture() {
    assert_evals_to!(
        indoc!(
            r#"
            capture : _ -> ({} -> Str)
            capture = \val ->
                \{} ->
                    Num.to_str val

            capture2 = \val -> \{} -> val

            f = \x ->
                g =
                    when x is
                        A -> capture 11u8
                        B -> capture2 "lisa"
                        C -> capture 187128u64
                g {}

            {a: f A, b: f B, c: f C}
            "#
        ),
        (
            RocStr::from("11"),
            RocStr::from("lisa"),
            RocStr::from("187128")
        ),
        (RocStr, RocStr, RocStr)
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn lambda_capture_niches_with_non_capturing_function() {
    assert_evals_to!(
        indoc!(
            r#"
            capture : _ -> ({} -> Str)
            capture = \val ->
                \{} ->
                    Num.to_str val

            triv = \{} -> "triv"

            f = \x ->
                g =
                    when x is
                        A -> capture 11u8
                        B -> triv
                        C -> capture 187128u64
                g {}

            {a: f A, b: f B, c: f C}
            "#
        ),
        (
            RocStr::from("11"),
            RocStr::from("triv"),
            RocStr::from("187128")
        ),
        (RocStr, RocStr, RocStr)
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn lambda_capture_niches_have_captured_function_in_closure() {
    assert_evals_to!(
        indoc!(
            r#"
            Lazy a : {} -> a

            after : Lazy a, (a -> Lazy b) -> Lazy b
            after = \effect, map ->
                thunk = \{} ->
                    when map (effect {}) is
                        b -> b {}
                thunk

            f = \_ -> \_ -> "fun f"
            g = \{ s1 } -> \_ -> s1

            fun = \x ->
                h =
                    if x then
                        after (\{} -> "") f
                    else
                        after (\{} -> {s1: "s1"}) g
                h {}

            {a: fun Bool.false, b: fun Bool.true}
            "#
        ),
        (RocStr::from("s1"), RocStr::from("fun f")),
        (RocStr, RocStr)
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn recursive_call_capturing_function() {
    assert_evals_to!(
        indoc!(
            r"
            a = \b ->
                c = \d ->
                    if d == 7 then d else c (d + b)
                c 1

            a 6
            "
        ),
        7,
        i64
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn shared_pattern_variable_in_when_branches() {
    assert_evals_to!(
        indoc!(
            r"
            f = \t ->
                when t is
                    A x | B x -> x

            {a: f (A 15u8), b: (B 31u8)}
            "
        ),
        (15u8, 31u8),
        (u8, u8)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn symbol_not_bound_in_all_patterns_runs_when_no_bound_symbol_used() {
    assert_evals_to!(
        indoc!(
            r"
            f = \t -> when t is
                        A x | B y -> 31u8

            {a: f (A 15u8), b: f (B 15u8)}
            "
        ),
        (31u8, 31u8),
        (u8, u8),
        |x| x,
        true // allow errors
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn symbol_not_bound_in_all_patterns_runs_when_bound_pattern_reached() {
    assert_evals_to!(
        indoc!(
            r"
            when A 15u8 is
                A x | B y -> x
            "
        ),
        15u8,
        u8,
        |x| x,
        true // allow errors
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
#[should_panic(
    expected = r#"Roc failed with message: "Hit a branch pattern that does not bind all symbols its body needs"#
)]
fn runtime_error_when_degenerate_pattern_reached() {
    assert_evals_to!(
        indoc!(
            r"
            when B 15u8 is
                A x | B y -> x + 5u8
            "
        ),
        15u8,
        u8,
        |x| x,
        true // allow errors
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn recursive_lambda_set_issue_3444() {
    assert_evals_to!(
        indoc!(
            r#"
            combine = \f, g -> \x -> g (f x)
            const = \x -> (\_y -> x)

            list = [const "a", const "b", const "c"]

            res : Str -> Str
            res = List.walk list (const "z") (\c1, c2 -> combine c1 c2)
            res "hello"
            "#
        ),
        RocStr::from("c"),
        RocStr
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn recursive_lambda_set_toplevel_issue_3444() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            combine = \f, g -> \x -> g (f x)
            const = \x -> (\_y -> x)

            list = [const "a", const "b", const "c"]

            res : Str -> Str
            res = List.walk list (const "z") (\c1, c2 -> combine c1 c2)

            main = res "hello"
            "#
        ),
        RocStr::from("c"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn recursive_lambda_set_issue_3444_inferred() {
    assert_evals_to!(
        indoc!(
            r#"
            combine = \f, g -> \x -> g (f x)
            const = \x -> (\_y -> x)

            list = [const "a", const "b", const "c"]

            res = List.walk list (const "z") (\c1, c2 -> combine c1 c2)
            res "hello"
            "#
        ),
        RocStr::from("c"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn compose_recursive_lambda_set_productive_toplevel() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            compose = \f, g -> \x -> g (f x)

            identity = \x -> x
            exclaim = \s -> "$(s)!"
            whisper = \s -> "($(s))"

            main =
                res: Str -> Str
                res = List.walk [ exclaim, whisper ] identity compose
                res "hello"
            "#
        ),
        RocStr::from("(hello!)"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn compose_recursive_lambda_set_productive_nested() {
    assert_evals_to!(
        indoc!(
            r#"
            compose = \f, g -> \x -> g (f x)

            identity = \x -> x
            exclaim = \s -> "$(s)!"
            whisper = \s -> "($(s))"

            res: Str -> Str
            res = List.walk [ exclaim, whisper ] identity compose
            res "hello"
            "#
        ),
        RocStr::from("(hello!)"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn compose_recursive_lambda_set_productive_inferred() {
    assert_evals_to!(
        indoc!(
            r#"
            compose = \f, g -> \x -> g (f x)

            identity = \x -> x
            exclaim = \s -> "$(s)!"
            whisper = \s -> "($(s))"

            res = List.walk [ exclaim, whisper ] identity compose
            res "hello"
            "#
        ),
        RocStr::from("(hello!)"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn compose_recursive_lambda_set_productive_nullable_wrapped() {
    assert_evals_to!(
        indoc!(
            r#"
             app "test" provides [main] to "./platform"

             compose = \forward -> \f, g ->
                if forward
                then \x -> g (f x)
                else \x -> f (g x)

             identity = \x -> x
             exclame = \s -> "$(s)!"
             whisper = \s -> "($(s))"

             main =
                 res: Str -> Str
                 res = List.walk [ exclame, whisper ] identity (compose Bool.false)
                 res "hello"
             "#
        ),
        RocStr::from("(hello)!"),
        RocStr
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn local_binding_aliases_function() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [ main ] to "./platform"

            f : {} -> List a
            f = \_ -> []

            main : List U8
            main =
                g = f

                g {}
            "#
        ),
        RocList::<u8>::from_slice(&[]),
        RocList<u8>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn local_binding_aliases_function_inferred() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [ main ] to "./platform"

            f = \_ -> []

            main =
                g = f

                g {}
            "#
        ),
        RocList::from_slice(&[]),
        RocList<std::convert::Infallible>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn transient_captures() {
    assert_evals_to!(
        indoc!(
            r#"
            x = "abc"

            get_x = \{} -> x

            h = \{} -> get_x {}

            h {}
            "#
        ),
        RocStr::from("abc"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn transient_captures_after_def_ordering() {
    assert_evals_to!(
        indoc!(
            r#"
            h = \{} -> get_x {}

            get_x = \{} -> x

            x = "abc"

            h {}
            "#
        ),
        RocStr::from("abc"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn deep_transient_capture_chain() {
    assert_evals_to!(
        indoc!(
            r#"
            z = "abc"

            get_x = \{} -> get_y {}
            get_y = \{} -> get_z {}
            get_z = \{} -> z

            h = \{} -> get_x {}

            h {}
            "#
        ),
        RocStr::from("abc"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn deep_transient_capture_chain_with_multiple_captures() {
    assert_evals_to!(
        indoc!(
            r#"
            h = "h"
            x = "x"
            y = "y"
            z = "z"

            get_x = \{} -> Str.concat x (get_y {})
            get_y = \{} -> Str.concat y (get_z {})
            get_z = \{} -> z

            get_h = \{} -> Str.concat h (get_x {})

            get_h {}
            "#
        ),
        RocStr::from("hxyz"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn transient_captures_from_outer_scope() {
    assert_evals_to!(
        indoc!(
            r#"
            x = "abc"

            get_x = \{} -> x

            inner_scope =
                h = \{} -> get_x {}
                h {}

            inner_scope
            "#
        ),
        RocStr::from("abc"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn mutually_recursive_captures() {
    assert_evals_to!(
        indoc!(
            r#"
            x : Bool
            x = Bool.true

            y : Bool
            y = Bool.false

            a = "foo"
            b = "bar"

            foo = \{} -> if x then a else bar {}
            bar = \{} -> if y then b else foo {}

            bar {}
            "#
        ),
        RocStr::from("foo"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn int_let_generalization() {
    assert_evals_to!(
        indoc!(
            r#"
            many_aux : {} -> I32
            many_aux = \_ ->
                output = \_ -> 42

                output {}

            when many_aux {} is
                _ -> "done"
            "#
        ),
        RocStr::from("done"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn pattern_match_char() {
    assert_evals_to!(
        indoc!(
            r#"
            c = 'A'

            when c is
                'A' -> "okay"
                _ -> "FAIL"
            "#
        ),
        RocStr::from("okay"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn issue_4348() {
    assert_evals_to!(
        indoc!(
            r#"
            str = "z"
            (\_ ->
                when str is
                    "z" -> "okay"
                    _ -> "") "FAIL"
            "#
        ),
        RocStr::from("okay"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn issue_4349() {
    assert_evals_to!(
        indoc!(
            r#"
            ir = Ok ""
            res =
                Result.try ir \_ ->
                    when ir is
                        Ok "" -> Ok ""
                        _ -> Err Bad
            when res is
                Ok _ -> "okay"
                _ -> "FAIL"
            "#
        ),
        RocStr::from("okay"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn issue_4712() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            Parser a : {} -> a

            v1 : {}
            v1 = {}

            v2 : Str
            v2 = "cd"

            apply : Parser (a -> Str), a -> Parser Str
            apply = \fn_parser, val_parser ->
                \{} ->
                    (fn_parser {}) (val_parser)

            map : a, (a -> Str) -> Parser Str
            map = \simple_parser, transform ->
                apply (\{} -> transform) simple_parser

            gen = \{} ->
                [ map v1 (\{} -> "ab"), map v2 (\s -> s) ]
                |> List.map (\f -> f {})
                |> Str.join_with  ","

            main = gen {}
            "#
        ),
        RocStr::from("ab,cd"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn pattern_as_toplevel() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            record = { a: 42i64, b: "foo" }

            main =
                when record is
                    { a: 42i64 } as r -> record == r
                    _ -> Bool.false
            "#
        ),
        true,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn pattern_as_nested() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            record = { a: 42i64, b: "foo" }

            main =
                when Pair {} record is
                    Pair {} ({ a: 42i64 } as r) -> record == r
                    _ -> Bool.false
            "#
        ),
        true,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn pattern_as_of_symbol() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            main =
                when "foo" is
                    a as b -> a == b
            "#
        ),
        true,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn function_specialization_information_in_lambda_set_thunk() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            and_then = \{} ->
                x = 10u8
                \new_fn -> Num.add (new_fn {}) x

            between = and_then {}

            main = between \{} -> between \{} -> 10u8
            "#
        ),
        30,
        u8
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn function_specialization_information_in_lambda_set_thunk_independent_defs() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            and_then = \{} ->
                x = 10u8
                \new_fn -> Num.add (new_fn {}) x

            between1 = and_then {}

            between2 = and_then {}

            main = between1 \{} -> between2 \{} -> 10u8
            "#
        ),
        30,
        u8
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn when_guard_appears_multiple_times_in_compiled_decision_tree_issue_5176() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            go : U8 -> U8
            go = \byte ->
                when byte is
                    15 if Bool.true -> 1
                    b if Bool.true -> b + 2
                    _ -> 3

            main = go '.'
            "#
        ),
        b'.' + 2,
        u8
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn recursive_lambda_set_resolved_only_upon_specialization() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            fact_cps = \n, cont ->
                if n == 0 then
                    cont 1
                else
                    fact_cps (n - 1) \value -> cont (n * value)

            main =
                fact_cps 5u64 \x -> x
            "#
        ),
        120,
        u64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn layout_cache_structure_with_multiple_recursive_structures() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            Chain : [
                End,
                Link Chain,
            ]

            LinkedList : [Nil, Cons { first : Chain, rest : LinkedList }]

            main =
                base : LinkedList
                base = Nil

                walker : LinkedList, Chain -> LinkedList
                walker = \rest, first -> Cons { first, rest }

                list : List Chain
                list = []

                r = List.walk list base walker

                if r == base then 11u8 else 22u8
            "#
        ),
        11,
        u8
    );
}

#[test]
#[cfg(feature = "gen-llvm")]
fn reset_recursive_type_wraps_in_named_type() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            main : Str
            main =
              new_list = map_linked_list (Cons 1 (Cons 2 (Cons 3 Nil))) (\x -> x + 1)
              print_linked_list new_list Num.to_str

            LinkedList a : [Cons a (LinkedList a), Nil]

            map_linked_list : LinkedList a, (a -> b) -> LinkedList b
            map_linked_list = \linked_list, f -> when linked_list is
              Nil -> Nil
              Cons x xs ->
                s = if Bool.true then "true" else "false"
                expect s == "true"

                Cons (f x) (map_linked_list xs f)

            print_linked_list : LinkedList a, (a -> Str) -> Str
            print_linked_list = \linked_list, f ->
              when linked_list is
                Nil -> "Nil"
                Cons x xs ->
                  str_x = f x
                  str_xs = print_linked_list xs f
                  "Cons $(str_x) ($(str_xs))"
            "#
        ),
        RocStr::from("Cons 2 (Cons 3 (Cons 4 (Nil)))"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn pass_lambda_set_to_function() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            instr = if Bool.true then Num.mul else Num.add

            fn = \a -> instr a a

            main = fn 3
            "#
        ),
        3 * 3,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn linked_list_trmc() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            LinkedList a : [Nil, Cons a (LinkedList a)]

            repeat : a, U64 -> LinkedList a
            repeat = \value, n ->
                when n is
                    0 -> Nil
                    _ -> Cons value (repeat value (n - 1))

            length : LinkedList a -> I64
            length = \list ->
                when list is
                    Nil -> 0
                    Cons _ rest -> 1 + length rest

            main : I64
            main =
                repeat "foo" 5
                    |> length
            "#
        ),
        5,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn many_arguments() {
    // exhausts all argument registers on x86 and aarch
    assert_evals_to!(
        indoc!(
            r"
            fun = \a,b,c,d, e,f,g,h, i ->
                (a + b + c + d) + (e + f + g + h) + i

            fun 0i64 1 2 3 4 5 6 7 8
            "
        ),
        1 + 2 + 3 + 4 + 5 + 6 + 7 + 8,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn multiple_uses_of_bool_true_record() {
    assert_evals_to!(
        indoc!(
            r"
            (Bool.true, Bool.true).0
            "
        ),
        true,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn multiple_uses_of_bool_true_tag_union() {
    assert_evals_to!(
        indoc!(
            r"
            x : [ One Bool Bool, Empty ]
            x = One Bool.true Bool.true

            when x is
                One a _ -> a
                Empty -> Bool.false
            "
        ),
        true,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn issue_6139_contains() {
    assert_evals_to!(
        indoc!(
            r#"
            buggy = \node, seen ->
                if List.contains seen node then
                    seen
                else
                    # node = "B"
                    next_node = step_node node

                    # node = "C"
                    buggy next_node (List.append seen node)

            step_node = \node ->
                when node is
                    "A" -> "B"
                    "B" -> "C"
                    "C" -> "D"
                    "D" -> "A"
                    _ -> crash ""

            buggy "A" []
            "#
        ),
        RocList::from_slice(&[
            RocStr::from("A"),
            RocStr::from("B"),
            RocStr::from("C"),
            RocStr::from("D"),
        ]),
        RocList<RocStr>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn issue_6139_prefixes() {
    assert_evals_to!(
        indoc!(
            r#"
            prefixes = \str, soFar ->
                if Str.is_empty str then
                    soFar

                else
                    graphemes =
                        Str.to_utf8 str
                        |> List.map \c -> Str.from_utf8 [c] |> Result.with_default ""
                    remaining = List.drop_first graphemes 1
                    next = Str.join_with remaining ""

                    prefixes next (List.append soFar str)

            prefixes "abc" []
            "#
        ),
        RocList::from_slice(&[RocStr::from("abc"), RocStr::from("bc"), RocStr::from("c")]),
        RocList<RocStr>
    );
}
