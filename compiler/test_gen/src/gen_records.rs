#[cfg(feature = "gen-llvm")]
use crate::helpers::llvm::assert_evals_to;

#[cfg(feature = "gen-llvm")]
use crate::helpers::llvm::expect_runtime_error_panic;

#[cfg(feature = "gen-dev")]
use crate::helpers::dev::assert_evals_to;

#[cfg(feature = "gen-wasm")]
use crate::helpers::wasm::assert_evals_to;

// use crate::assert_wasm_evals_to as assert_evals_to;
use indoc::indoc;

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn basic_record() {
    assert_evals_to!(
        indoc!(
            r#"
                    { y: 17, x: 15, z: 19 }.x
                "#
        ),
        15,
        i64
    );

    assert_evals_to!(
        indoc!(
            r#"
                    { x: 15, y: 17, z: 19 }.y
                "#
        ),
        17,
        i64
    );

    assert_evals_to!(
        indoc!(
            r#"
                    { x: 15, y: 17, z: 19 }.z
                "#
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
            r#"
                   rec = { y: 17.2, x: 15.1, z: 19.3 }

                   rec.x
                "#
        ),
        15.1,
        f64
    );

    assert_evals_to!(
        indoc!(
            r#"
                   rec = { y: 17.2, x: 15.1, z: 19.3 }

                   rec.y
                "#
        ),
        17.2,
        f64
    );

    assert_evals_to!(
        indoc!(
            r#"
                    rec = { y: 17.2, x: 15.1, z: 19.3 }

                    rec.z
                "#
        ),
        19.3,
        f64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn fn_record() {
    assert_evals_to!(
        indoc!(
            r#"
                    getRec = \x -> { y: "foo", x, z: 19 }

                    (getRec 15).x
                "#
        ),
        15,
        i64
    );

    assert_evals_to!(
        indoc!(
            r#"
                    rec = { x: 15, y: 17, z: 19 }

                    rec.y
                "#
        ),
        17,
        i64
    );

    assert_evals_to!(
        indoc!(
            r#"
                    rec = { x: 15, y: 17, z: 19 }

                    rec.z
                "#
        ),
        19,
        i64
    );

    assert_evals_to!(
        indoc!(
            r#"
                    rec = { x: 15, y: 17, z: 19 }

                    rec.z + rec.x
                "#
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
            r#"
                    rec = { y: 17, x: 15, z: 19 }

                    rec.x
                "#
        ),
        15,
        i64
    );

    assert_evals_to!(
        indoc!(
            r#"
                    rec = { x: 15, y: 17, z: 19 }

                    rec.y
                "#
        ),
        17,
        i64
    );

    assert_evals_to!(
        indoc!(
            r#"
                    rec = { x: 15, y: 17, z: 19 }

                    rec.z
                "#
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
            r#"
                when { x: 0x2 } is
                    { x } -> x + 3
                "#
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
            r#"
                when { x: 0x2, y: 3.14 } is
                    { x: var } -> var + 3
                "#
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
            r#"
                { x } = { x: 0x2, y: 3.14 }

                x
                "#
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
            r#"
                when { x: 0x2, y: 3.14 } is
                    { x: 0x4 } -> 5
                    { x } -> x + 3
                "#
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
            r#"
                x =  {a: 0x2, b: 0x3 }

                x.a + x.b
                "#
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
            r#"
                v = {}

                v
                "#
        ),
        (),
        ()
    );
}
#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn i64_record2_literal() {
    assert_evals_to!(
        indoc!(
            r#"
                   { x: 3, y: 5 }
                "#
        ),
        (3, 5),
        (i64, i64)
    );
}

// #[test]
// #[cfg(any(feature = "gen-llvm"))]
// fn i64_record3_literal() {
//     assert_evals_to!(
//         indoc!(
//             r#"
//                { x: 3, y: 5, z: 17 }
//             "#
//         ),
//         (3, 5, 17),
//         (i64, i64, i64)
//     );
// }
#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn f64_record2_literal() {
    assert_evals_to!(
        indoc!(
            r#"
                   { x: 3.1, y: 5.1 }
                "#
        ),
        (3.1, 5.1),
        (f64, f64)
    );
}

// #[test]
// #[cfg(any(feature = "gen-llvm"))]
// fn f64_record3_literal() {
//     assert_evals_to!(
//         indoc!(
//             r#"
//                { x: 3.1, y: 5.1, z: 17.1 }
//             "#
//         ),
//         (3.1, 5.1, 17.1),
//         (f64, f64, f64)
//     );
// }

// #[test]
// #[cfg(any(feature = "gen-llvm"))]
// fn bool_record4_literal() {
//     assert_evals_to!(
//         indoc!(
//             r#"
//                record : { a : Bool, b : Bool, c : Bool, d : Bool }
//                record = { a: True, b: True, c : True, d : Bool }

//                record
//             "#
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
            r#"
                   { a: 3 }
                "#
        ),
        3,
        i64
    );
}

// #[test]
// #[cfg(any(feature = "gen-llvm"))]
// fn i64_record9_literal() {
//     assert_evals_to!(
//         indoc!(
//             r#"
//                { a: 3, b: 5, c: 17, d: 1, e: 9, f: 12, g: 13, h: 14, i: 15 }
//             "#
//         ),
//         (3, 5, 17, 1, 9, 12, 13, 14, 15),
//         (i64, i64, i64, i64, i64, i64, i64, i64, i64)
//     );
// }

// #[test]
// #[cfg(any(feature = "gen-llvm"))]
// fn f64_record3_literal() {
//     assert_evals_to!(
//         indoc!(
//             r#"
//                { x: 3.1, y: 5.1, z: 17.1 }
//             "#
//         ),
//         (3.1, 5.1, 17.1),
//         (f64, f64, f64)
//     );
// }
#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn bool_literal() {
    assert_evals_to!(
        indoc!(
            r#"
                x : Bool
                x = True

                x
                "#
        ),
        true,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn return_record() {
    assert_evals_to!(
        indoc!(
            r#"
                x = 4
                y = 3

                { x, y }
                "#
        ),
        (4, 3),
        (i64, i64)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn optional_field_when_use_default() {
    assert_evals_to!(
        indoc!(
            r#"
                app "test" provides [ main ] to "./platform"

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
#[cfg(any(feature = "gen-llvm"))]
fn optional_field_when_use_default_nested() {
    assert_evals_to!(
        indoc!(
            r#"
                f = \r ->
                    when r is
                        { x: Blue, y ? 3 } -> y
                        { x: Red, y ? 5 } -> y

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
#[cfg(any(feature = "gen-llvm"))]
fn optional_field_when_no_use_default() {
    assert_evals_to!(
        indoc!(
            r#"
                app "test" provides [ main ] to "./platform"

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
#[cfg(any(feature = "gen-llvm"))]
fn optional_field_when_no_use_default_nested() {
    assert_evals_to!(
        indoc!(
            r#"
                f = \r ->
                    { x ? 10, y } = r
                    x + y

                f { x: 4, y: 9 }
                "#
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
                app "test" provides [ main ] to "./platform"

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
#[cfg(any(feature = "gen-llvm"))]
fn optional_field_let_no_use_default() {
    assert_evals_to!(
        indoc!(
            r#"
                app "test" provides [ main ] to "./platform"

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
#[cfg(any(feature = "gen-llvm"))]
fn optional_field_let_no_use_default_nested() {
    assert_evals_to!(
        indoc!(
            r#"
                f = \r ->
                    { x ? 10, y } = r
                    x + y

                f { x: 4, y: 9 }
                "#
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
            r#"
                f = \{ x ? 10, y } -> x + y


                f { y: 9 }
                "#
        ),
        19,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
#[ignore]
fn optional_field_function_no_use_default() {
    // blocked on https://github.com/rtfeldman/roc/issues/786
    assert_evals_to!(
        indoc!(
            r#"
                app "test" provides [ main ] to "./platform"

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
#[cfg(any(feature = "gen-llvm"))]
#[ignore]
fn optional_field_function_no_use_default_nested() {
    // blocked on https://github.com/rtfeldman/roc/issues/786
    assert_evals_to!(
        indoc!(
            r#"
                f = \{ x ? 10, y } -> x + y


                f { x: 4, y: 9 }
                "#
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
            r#"
                when { x : 4 } is
                    { x ? 3 } -> x
                "#
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
            r#"
                when { } is
                    { x ? 3 } -> x
                "#
        ),
        3,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn return_record_2() {
    assert_evals_to!(
        indoc!(
            r#"
                { x: 3, y: 5 }
                "#
        ),
        [3, 5],
        [i64; 2]
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn return_record_3() {
    assert_evals_to!(
        indoc!(
            r#"
                { x: 3, y: 5, z: 4 }
                "#
        ),
        (3, 5, 4),
        (i64, i64, i64)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn return_record_4() {
    assert_evals_to!(
        indoc!(
            r#"
                { a: 3, b: 5, c: 4, d: 2 }
                "#
        ),
        [3, 5, 4, 2],
        [i64; 4]
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn return_record_5() {
    assert_evals_to!(
        indoc!(
            r#"
                { a: 3, b: 5, c: 4, d: 2, e: 1 }
                "#
        ),
        [3, 5, 4, 2, 1],
        [i64; 5]
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn return_record_6() {
    assert_evals_to!(
        indoc!(
            r#"
                { a: 3, b: 5, c: 4, d: 2, e: 1, f: 7 }
                "#
        ),
        [3, 5, 4, 2, 1, 7],
        [i64; 6]
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn return_record_7() {
    assert_evals_to!(
        indoc!(
            r#"
                { a: 3, b: 5, c: 4, d: 2, e: 1, f: 7, g: 8 }
                "#
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
            r#"
                { a: 3.14, b: 0x1 }
                "#
        ),
        (3.14, 0x1),
        (f64, i64)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn return_record_int_float() {
    assert_evals_to!(
        indoc!(
            r#"
                { a: 0x1, b: 3.14 }
                "#
        ),
        (0x1, 3.14),
        (i64, f64)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn return_record_float_float() {
    assert_evals_to!(
        indoc!(
            r#"
                { a: 6.28, b: 3.14 }
                "#
        ),
        (6.28, 3.14),
        (f64, f64)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn return_record_float_float_float() {
    assert_evals_to!(
        indoc!(
            r#"
                { a: 6.28, b: 3.14, c: 0.1 }
                "#
        ),
        (6.28, 3.14, 0.1),
        (f64, f64, f64)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn return_nested_record() {
    assert_evals_to!(
        indoc!(
            r#"
                { flag: 0x0, payload: { a: 6.28, b: 3.14, c: 0.1 } }
                "#
        ),
        (0x0, (6.28, 3.14, 0.1)),
        (i64, (f64, f64, f64))
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn nested_record_load() {
    assert_evals_to!(
        indoc!(
            r#"
                x = { a : { b : 0x5 } }

                y = x.a

                y.b
                "#
        ),
        5,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn accessor_twice() {
    assert_evals_to!(".foo { foo: 4 }  + .foo { bar: 6.28, foo: 3 } ", 7, i64);
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
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
#[cfg(any(feature = "gen-llvm"))]
fn accessor_single_element_record() {
    assert_evals_to!(
        indoc!(
            r#"
                .foo { foo: 4 }
                "#
        ),
        4,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn update_record() {
    assert_evals_to!(
        indoc!(
            r#"
                rec = { foo: 42, bar: 6.28 }

                { rec & foo: rec.foo + 1 }
                "#
        ),
        (6.28, 43),
        (f64, i64)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn update_single_element_record() {
    assert_evals_to!(
        indoc!(
            r#"
                rec = { foo: 42}

                { rec & foo: rec.foo + 1 }
                "#
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
#[cfg(any(feature = "gen-llvm"))]
fn alignment_in_record() {
    assert_evals_to!(
        indoc!("{ c: 32, b: if True then Red else if True then Green else Blue, a: 1 == 1 }"),
        (32i64, true, 2u8),
        (i64, bool, u8)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn blue_and_present() {
    assert_evals_to!(
        indoc!(
            r#"
                f = \r ->
                    when r is
                        { x: Blue, y ? 3 } -> y
                        { x: Red, y ? 5 } -> y

                f { x: Blue, y: 7 }
                "#
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
            r#"
                f = \r ->
                    when r is
                        { x: Blue, y ? 3 } -> y
                        { x: Red, y ? 5 } -> y

                f { x: Blue }
                "#
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
            r#"
            Model : { foo : I64 }

            model : Model
            model = { foo: 3 }

            foo = 4

            newModel : Model
            newModel = { model & foo }

            newModel.foo
                "#
        ),
        4,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
// https://github.com/rtfeldman/roc/issues/1513
fn both_have_unique_fields() {
    assert_evals_to!(
        indoc!(
            r#"
            a = { x: 42, y: 43 }
            b = { x: 42, z: 44 }

            f : { x : I64 }a, { x : I64 }b -> I64
            f = \{ x: x1}, { x: x2 } -> x1 + x2 

            f a b
            "#
        ),
        84,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
#[should_panic(
    // TODO: something upstream is escaping the '
    expected = r#"Roc failed with message: "Can\'t create record with improper layout""#
)]
fn call_with_bad_record_runtime_error() {
    expect_runtime_error_panic!(indoc!(
        r#"
            app "test" provides [ main ] to "./platform"

            main =
                get : {a: Bool} -> Bool
                get = \{a} -> a
                get {b: ""}
            "#
    ))
}
