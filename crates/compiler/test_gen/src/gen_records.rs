#[cfg(feature = "gen-llvm")]
use crate::helpers::llvm::assert_evals_to;

#[cfg(feature = "gen-dev")]
use crate::helpers::dev::assert_evals_to;

#[cfg(feature = "gen-wasm")]
use crate::helpers::wasm::assert_evals_to;

// use crate::assert_wasm_evals_to as assert_evals_to;
use indoc::indoc;

#[cfg(all(test, any(feature = "gen-llvm", feature = "gen-wasm")))]
use roc_std::{RocList, RocStr};

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn basic_record() {
    assert_evals_to!(
        indoc!(
            r"
                    { y: 17, x: 15, z: 19 }.x
                "
        ),
        15,
        i64
    );

    assert_evals_to!(
        indoc!(
            r"
                    { x: 15, y: 17, z: 19 }.y
                "
        ),
        17,
        i64
    );

    assert_evals_to!(
        indoc!(
            r"
                    { x: 15, y: 17, z: 19 }.z
                "
        ),
        19,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn f64_record() {
    assert_evals_to!(
        indoc!(
            r"
                   rec = { y: 17.2f64, x: 15.1f64, z: 19.3f64 }

                   rec.x
                "
        ),
        15.1,
        f64
    );

    assert_evals_to!(
        indoc!(
            r"
                   rec = { y: 17.2f64, x: 15.1f64, z: 19.3f64 }

                   rec.y
                "
        ),
        17.2,
        f64
    );

    assert_evals_to!(
        indoc!(
            r"
                    rec = { y: 17.2f64, x: 15.1f64, z: 19.3f64 }

                    rec.z
                "
        ),
        19.3,
        f64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn pass_bool_record() {
    // found a bug there the register to use was not incremented correctly
    assert_evals_to!(
        indoc!(
            r"
               true : Bool
               true = Bool.true

               f = \_, x -> x

               f { x: true, y: true } 23
               "
        ),
        23,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn fn_record() {
    assert_evals_to!(
        indoc!(
            r#"
                    get_rec = \x -> { y: "foo", x, z: 19 }

                    (get_rec 15).x
                "#
        ),
        15,
        i64
    );

    assert_evals_to!(
        indoc!(
            r"
                    rec = { x: 15, y: 17, z: 19 }

                    rec.z + rec.x
                "
        ),
        34,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn def_record() {
    assert_evals_to!(
        indoc!(
            r"
                    rec = { y: 17, x: 15, z: 19 }

                    rec.x
                "
        ),
        15,
        i64
    );

    assert_evals_to!(
        indoc!(
            r"
                    rec = { x: 15, y: 17, z: 19 }

                    rec.y
                "
        ),
        17,
        i64
    );

    assert_evals_to!(
        indoc!(
            r"
                    rec = { x: 15, y: 17, z: 19 }

                    rec.z
                "
        ),
        19,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn when_on_record() {
    assert_evals_to!(
        indoc!(
            r"
                when { x: 0x2 } is
                    { x } -> x + 3
                "
        ),
        5,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn when_record_with_guard_pattern() {
    assert_evals_to!(
        indoc!(
            r"
                when { x: 0x2, y: 1.23 } is
                    { x: var } -> var + 3
                "
        ),
        5,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn let_with_record_pattern() {
    assert_evals_to!(
        indoc!(
            r"
                { x } = { x: 0x2, y: 1.23 }

                x
                "
        ),
        2,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn record_guard_pattern() {
    assert_evals_to!(
        indoc!(
            r"
                when { x: 0x2, y: 1.23 } is
                    { x: 0x4 } -> 5
                    { x } -> x + 3
                "
        ),
        5,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn twice_record_access() {
    assert_evals_to!(
        indoc!(
            r"
                x =  {a: 0x2, b: 0x3 }

                x.a + x.b
                "
        ),
        5,
        i64
    );
}
#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn empty_record() {
    assert_evals_to!(
        indoc!(
            r"
                v = {}

                v
                "
        ),
        (),
        ()
    );
}
#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn i64_record2_literal() {
    assert_evals_to!(
        indoc!(
            r"
                   { x: 3, y: 5 }
                "
        ),
        (3, 5),
        (i64, i64)
    );
}

// #[test]
// #[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
// fn i64_record3_literal() {
//     assert_evals_to!(
//         indoc!(
//             r"
//                { x: 3, y: 5, z: 17 }
//             "
//         ),
//         (3, 5, 17),
//         (i64, i64, i64)
//     );
// }
#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn f64_record2_literal() {
    assert_evals_to!(
        indoc!(
            r"
                   { x: 3.1f64, y: 5.1f64 }
                "
        ),
        (3.1, 5.1),
        (f64, f64)
    );
}

// #[test]
// #[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
// fn f64_record3_literal() {
//     assert_evals_to!(
//         indoc!(
//             r"
//                { x: 3.1, y: 5.1, z: 17.1 }
//             "
//         ),
//         (3.1, 5.1, 17.1),
//         (f64, f64, f64)
//     );
// }

// #[test]
// #[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
// fn bool_record4_literal() {
//     assert_evals_to!(
//         indoc!(
//             r"
//                record : { a : Bool, b : Bool, c : Bool, d : Bool }
//                record = { a: Bool.true, b: Bool.true, c : Bool.true, d : Bool }

//                record
//             "
//         ),
//         (true, false, false, true),
//         (bool, bool, bool, bool)
//     );
// }
#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn i64_record1_literal() {
    assert_evals_to!(
        indoc!(
            r"
                   { a: 3 }
                "
        ),
        3,
        i64
    );
}

// #[test]
// #[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
// fn i64_record9_literal() {
//     assert_evals_to!(
//         indoc!(
//             r"
//                { a: 3, b: 5, c: 17, d: 1, e: 9, f: 12, g: 13, h: 14, i: 15 }
//             "
//         ),
//         (3, 5, 17, 1, 9, 12, 13, 14, 15),
//         (i64, i64, i64, i64, i64, i64, i64, i64, i64)
//     );
// }

// #[test]
// #[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
// fn f64_record3_literal() {
//     assert_evals_to!(
//         indoc!(
//             r"
//                { x: 3.1, y: 5.1, z: 17.1 }
//             "
//         ),
//         (3.1, 5.1, 17.1),
//         (f64, f64, f64)
//     );
// }
#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn bool_literal() {
    assert_evals_to!(
        indoc!(
            r"
                x : Bool
                x = Bool.true

                x
                "
        ),
        true,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn return_record() {
    assert_evals_to!(
        indoc!(
            r"
                x = 4
                y = 3

                { x, y }
                "
        ),
        (4, 3),
        (i64, i64)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn optional_field_when_use_default() {
    assert_evals_to!(
        indoc!(
            r#"
                app "test" provides [main] to "./platform"

                f = \r ->
                    when r is
                        { x: Blue, y ? 3 } -> y
                        { x: Red, y ? 5 } -> y


                main =
                    a = f { x: Blue, y: 7 }
                    b = f { x: Blue }
                    c = f { x: Red, y: 11 }
                    d = f { x: Red }

                    a * b * c * d
                "#
        ),
        3 * 5 * 7 * 11,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn optional_field_when_use_default_nested() {
    assert_evals_to!(
        indoc!(
            r"
                f = \r ->
                    when r is
                        { x: Blue, y ? 3 } -> y
                        { x: Red, y ? 5 } -> y

                a = f { x: Blue, y: 7 }
                b = f { x: Blue }
                c = f { x: Red, y: 11 }
                d = f { x: Red }

                a * b * c * d
                "
        ),
        3 * 5 * 7 * 11,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn optional_field_destructure_module() {
    assert_evals_to!(
        indoc!(
            r#"
                app "test" provides [main] to "./platform"

                f = \r ->
                    { x ? 10, y } = r
                    x + y

                main =
                    f { x: 4, y: 9 }
                "#
        ),
        13,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn optional_field_destructure_expr() {
    assert_evals_to!(
        indoc!(
            r"
                fn = \r ->
                    { x ? 10, y } = r
                    x + y

                fn { x: 4, y: 9 }
                "
        ),
        13,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn optional_field_let_use_default() {
    assert_evals_to!(
        indoc!(
            r#"
                app "test" provides [main] to "./platform"

                f = \r ->
                    { x ? 10, y } = r
                    x + y

                main =
                    f { y: 9 }
                "#
        ),
        19,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn optional_field_let_no_use_default() {
    assert_evals_to!(
        indoc!(
            r#"
                app "test" provides [main] to "./platform"

                f = \r ->
                    { x ? 10, y } = r
                    x + y

                main =
                    f { x: 4, y: 9 }
                "#
        ),
        13,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn optional_field_let_no_use_default_nested() {
    assert_evals_to!(
        indoc!(
            r"
                f = \r ->
                    { x ? 10, y } = r
                    x + y

                f { y: 9, x: 4 }
                "
        ),
        13,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn optional_field_function_use_default() {
    assert_evals_to!(
        indoc!(
            r"
                f = \{ x ? 10, y } -> x + y


                f { y: 9 }
                "
        ),
        19,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn optional_field_function_no_use_default() {
    assert_evals_to!(
        indoc!(
            r#"
                app "test" provides [main] to "./platform"

                f = \{ x ? 10, y } -> x + y

                main =
                    f { x: 4, y: 9 }
                "#
        ),
        13,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn optional_field_function_no_use_default_nested() {
    assert_evals_to!(
        indoc!(
            r"
                f = \{ x ? 10, y } -> x + y


                f { x: 4, y: 9 }
                "
        ),
        13,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn optional_field_singleton_record() {
    assert_evals_to!(
        indoc!(
            r"
                when { x : 4 } is
                    { x ? 3 } -> x
                "
        ),
        4,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev"))]
fn optional_field_empty_record() {
    assert_evals_to!(
        indoc!(
            r"
                when { } is
                    { x ? 3 } -> x
                "
        ),
        3,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn return_record_2() {
    assert_evals_to!(
        indoc!(
            r"
                { x: 3, y: 5 }
                "
        ),
        [3, 5],
        [i64; 2]
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn return_record_3() {
    assert_evals_to!(
        indoc!(
            r"
                { x: 3, y: 5, z: 4 }
                "
        ),
        (3, 5, 4),
        (i64, i64, i64)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn return_record_4() {
    assert_evals_to!(
        indoc!(
            r"
                { a: 3, b: 5, c: 4, d: 2 }
                "
        ),
        [3, 5, 4, 2],
        [i64; 4]
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn return_record_5() {
    assert_evals_to!(
        indoc!(
            r"
                { a: 3, b: 5, c: 4, d: 2, e: 1 }
                "
        ),
        [3, 5, 4, 2, 1],
        [i64; 5]
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn return_record_6() {
    assert_evals_to!(
        indoc!(
            r"
                { a: 3, b: 5, c: 4, d: 2, e: 1, f: 7 }
                "
        ),
        [3, 5, 4, 2, 1, 7],
        [i64; 6]
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn return_record_7() {
    assert_evals_to!(
        indoc!(
            r"
                { a: 3, b: 5, c: 4, d: 2, e: 1, f: 7, g: 8 }
                "
        ),
        [3, 5, 4, 2, 1, 7, 8],
        [i64; 7]
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn return_record_float_int() {
    assert_evals_to!(
        indoc!(
            r"
                { a: 1.23f64, b: 0x1 }
                "
        ),
        (1.23, 0x1),
        (f64, i64)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn return_record_int_float() {
    assert_evals_to!(
        indoc!(
            r"
                { a: 0x1, b: 1.23f64 }
                "
        ),
        (0x1, 1.23),
        (i64, f64)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn return_record_float_float() {
    assert_evals_to!(
        indoc!(
            r"
                { a: 2.46f64, b: 1.23f64 }
                "
        ),
        (2.46, 1.23),
        (f64, f64)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn return_record_float_float_float() {
    assert_evals_to!(
        indoc!(
            r"
                { a: 2.46f64, b: 1.23f64, c: 0.1f64 }
                "
        ),
        (2.46, 1.23, 0.1),
        (f64, f64, f64)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn return_nested_record() {
    assert_evals_to!(
        indoc!(
            r"
                { flag: 0x0, payload: { a: 2.46f64, b: 1.23f64, c: 0.1f64 } }
                "
        ),
        (0x0, (2.46, 1.23, 0.1)),
        (i64, (f64, f64, f64))
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn nested_record_load() {
    assert_evals_to!(
        indoc!(
            r"
                x = { a : { b : 0x5 } }

                y = x.a

                y.b
                "
        ),
        5,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn accessor_twice() {
    assert_evals_to!(".foo { foo: 4 }  + .foo { bar: 2.46f64, foo: 3 } ", 7, i64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn accessor_multi_element_record() {
    assert_evals_to!(
        indoc!(
            r#"
                .foo { foo: 4, bar: "foo" }
                "#
        ),
        4,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn accessor_single_element_record() {
    assert_evals_to!(
        indoc!(
            r"
                .foo { foo: 4 }
                "
        ),
        4,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn update_record() {
    assert_evals_to!(
        indoc!(
            r"
                rec = { foo: 42, bar: 2.46f64 }

                { rec & foo: rec.foo + 1 }
                "
        ),
        (2.46, 43),
        (f64, i64)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn update_single_element_record() {
    assert_evals_to!(
        indoc!(
            r"
                rec = { foo: 42}

                { rec & foo: rec.foo + 1 }
                "
        ),
        43,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn update_record_shorthand() {
    assert_evals_to!(
        indoc!(
            r"
                rec = { foo: 42, bar: 2.46f64 }

                rec |> &foo (rec.foo + 1)
                "
        ),
        (2.46, 43),
        (f64, i64)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn update_single_element_record_shorthand() {
    assert_evals_to!(
        indoc!(
            r"
                rec = { foo: 42}

                &foo rec (rec.foo + 1)
                "
        ),
        43,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn booleans_in_record() {
    assert_evals_to!(
        indoc!("{ x: 1 == 1, y: 1 == 1 }"),
        (true, true),
        (bool, bool)
    );
    assert_evals_to!(
        indoc!("{ x: 1 != 1, y: 1 == 1 }"),
        (false, true),
        (bool, bool)
    );
    assert_evals_to!(
        indoc!("{ x: 1 == 1, y: 1 != 1 }"),
        (true, false),
        (bool, bool)
    );
    assert_evals_to!(
        indoc!("{ x: 1 != 1, y: 1 != 1 }"),
        (false, false),
        (bool, bool)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn alignment_in_record() {
    assert_evals_to!(
        indoc!(
            "{ c: 32, b: if Bool.true then Red else if Bool.true then Green else Blue, a: 1 == 1 }"
        ),
        (32i64, true, 2u8),
        (i64, bool, u8)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn blue_and_present() {
    assert_evals_to!(
        indoc!(
            r"
                f = \r ->
                    when r is
                        { x: Blue, y ? 3 } -> y
                        { x: Red, y ? 5 } -> y

                f { x: Blue, y: 7 }
                "
        ),
        7,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn blue_and_absent() {
    assert_evals_to!(
        indoc!(
            r"
                f = \r ->
                    when r is
                        { x: Blue, y ? 3 } -> y
                        { x: Red, y ? 5 } -> y

                f { x: Blue }
                "
        ),
        3,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn update_the_only_field() {
    assert_evals_to!(
        indoc!(
            r"
            Model : { foo : I64 }

            model : Model
            model = { foo: 3 }

            foo = 4

            new_model : Model
            new_model = { model & foo }

            new_model.foo
                "
        ),
        4,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
// https://github.com/roc-lang/roc/issues/1513
fn both_have_unique_fields() {
    assert_evals_to!(
        indoc!(
            r"
            a = { x: 42, y: 43 }
            b = { x: 42, z: 44 }

            f : { x : I64 }a, { x : I64 }b -> I64
            f = \{ x: x1}, { x: x2 } -> x1 + x2

            f a b
            "
        ),
        84,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
// https://github.com/roc-lang/roc/issues/2535
fn different_proc_types_specialized_to_same_layout() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [nums] to "./platform"

            # Top-level values compile to procedure calls with no args
            # alpha has the generic type { a: Num *, b: Num * }
            # and gets specialized to two procedure calls below
            alpha = { a: 1, b: 2 }

            # The wider number always comes first in the layout,
            # which makes the two specializations look very similar.
            # Test that the compiler doesn't get them mixed up!
            nums : List U8
            nums =
                [
                    alpha.a,   # alpha specialized to layout { b: I64, a: U8 }
                    alpha.b,   # alpha specialized to layout { a: I64, b: U8 }
               ]
            "#
        ),
        RocList::from_slice(&[1, 2]),
        RocList<u8>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
#[should_panic(expected = r#"Roc failed with message: "Can't create record with improper layout""#)]
fn call_with_bad_record_runtime_error() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            main =
                get : {a: Bool} -> Bool
                get = \{a} -> a
                get {b: ""}
            "#
        ),
        true,
        bool,
        |x| x,
        true // ignore type errors
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn generalized_accessor() {
    assert_evals_to!(
        indoc!(
            r#"
            return_foo = .foo

            return_foo { foo: "foo" }
            "#
        ),
        RocStr::from("foo"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn update_record_that_is_a_thunk() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            main = Num.to_str from_original.birds

            original = { birds: 5, iguanas: 7, zebras: 2, goats: 1 }

            from_original = { original & birds: 4, iguanas: 3 }
            "#
        ),
        RocStr::from("4"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn update_record_that_is_a_thunk_single_field() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            main = Num.to_str from_original.birds

            original = { birds: 5 }

            from_original = { original & birds: 4 }
            "#
        ),
        RocStr::from("4"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn toplevel_accessor_fn_thunk() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            ra = .field

            main =
                ra { field : 15u8 }
            "#
        ),
        15u8,
        u8
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn pass_record_of_u8s() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            ra = \_ -> 1u8

            main =
                ra { a: 1u8, b: 0u8 }
            "#
        ),
        true,
        bool
    )
}
