#[cfg(feature = "gen-llvm")]
use crate::helpers::llvm::assert_evals_to;

#[cfg(feature = "gen-dev")]
use crate::helpers::dev::assert_evals_to;

#[cfg(feature = "gen-wasm")]
use crate::helpers::wasm::assert_evals_to;

// use crate::assert_wasm_evals_to as assert_evals_to;
use indoc::indoc;

#[cfg(all(test, any(feature = "gen-llvm", feature = "gen-wasm")))]
use roc_std::RocStr;

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn basic_tuple() {
    assert_evals_to!(
        indoc!(
            r"
                    ( 15, 17, 19 ).0
                "
        ),
        15,
        i64
    );

    assert_evals_to!(
        indoc!(
            r"
                    ( 15, 17, 19 ).1
                "
        ),
        17,
        i64
    );

    assert_evals_to!(
        indoc!(
            r"
                    ( 15, 17, 19 ).2
                "
        ),
        19,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn f64_tuple() {
    assert_evals_to!(
        indoc!(
            r"
                   tup = (17.2f64, 15.1f64, 19.3f64)

                   tup.0
                "
        ),
        17.2,
        f64
    );

    assert_evals_to!(
        indoc!(
            r"
                   tup = (17.2f64, 15.1f64, 19.3f64)

                   tup.1
                "
        ),
        15.1,
        f64
    );

    assert_evals_to!(
        indoc!(
            r"
                    tup = (17.2f64, 15.1f64, 19.3f64)

                    tup.2
                "
        ),
        19.3,
        f64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn fn_tuple() {
    assert_evals_to!(
        indoc!(
            r#"
                    get_rec = \x -> ("foo", x, 19)

                    (get_rec 15).1
                "#
        ),
        15,
        i64
    );

    assert_evals_to!(
        indoc!(
            r"
                    rec = (15, 17, 19)

                    rec.2 + rec.0
                "
        ),
        34,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn int_tuple() {
    assert_evals_to!(
        indoc!(
            r"
                    rec = (15, 17, 19)

                    rec.0
                "
        ),
        15,
        i64
    );

    assert_evals_to!(
        indoc!(
            r"
                    rec = (15, 17, 19)

                    rec.1
                "
        ),
        17,
        i64
    );

    assert_evals_to!(
        indoc!(
            r"
                    rec = (15, 17, 19)

                    rec.2
                "
        ),
        19,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn when_on_tuple() {
    assert_evals_to!(
        indoc!(
            r"
                when (0x2, 0x3) is
                    (x, y) -> x + y
                "
        ),
        5,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn when_tuple_with_guard_pattern() {
    assert_evals_to!(
        indoc!(
            r"
                when (0x2, 1.23) is
                    (var, _) -> var + 3
                "
        ),
        5,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn let_with_tuple_pattern() {
    assert_evals_to!(
        indoc!(
            r"
                (x, _ ) = (0x2, 1.23)

                x
                "
        ),
        2,
        i64
    );

    assert_evals_to!(
        indoc!(
            r"
                (_, y) = (0x2, 0x3)

                y
                "
        ),
        3,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn tuple_guard_pattern() {
    assert_evals_to!(
        indoc!(
            r"
                when (0x2, 1.23) is
                    (0x4, _) -> 5
                    (x, _) -> x + 4
                "
        ),
        6,
        i64
    );

    assert_evals_to!(
        indoc!(
            r"
                when (0x2, 0x3) is
                    (_, 0x4) -> 5
                    (_, x) -> x + 4
                "
        ),
        7,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn twice_tuple_access() {
    assert_evals_to!(
        indoc!(
            r"
                x =  (0x2, 0x3)

                x.0 + x.1
                "
        ),
        5,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn i64_tuple2_literal() {
    assert_evals_to!(
        indoc!(
            r"
                   (3, 5)
                "
        ),
        (3, 5),
        (i64, i64)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn i64_tuple3_literal() {
    assert_evals_to!(
        indoc!(
            r"
               (3, 5, 17)
            "
        ),
        (3, 5, 17),
        (i64, i64, i64)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn f64_tuple2_literal() {
    assert_evals_to!(
        indoc!(
            r"
                   (3.1f64, 5.1f64)
                "
        ),
        (3.1, 5.1),
        (f64, f64)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn bool_tuple4_literal() {
    assert_evals_to!(
        indoc!(
            r"
               tuple : (Bool, Bool, Bool, Bool)
               tuple = (Bool.true, Bool.false, Bool.false, Bool.true)

               tuple
            "
        ),
        (true, false, false, true),
        (bool, bool, bool, bool)
    );
}

// Not supported by wasm because of the size of the tuple:
// FromWasm32Memory is only implemented for tuples of up to 4 elements
#[test]
#[cfg(feature = "gen-llvm")]
fn i64_tuple9_literal() {
    assert_evals_to!(
        indoc!(
            r"
               ( 3, 5, 17, 1, 9, 12, 13, 14, 15 )
            "
        ),
        [3, 5, 17, 1, 9, 12, 13, 14, 15],
        [i64; 9]
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn return_tuple() {
    assert_evals_to!(
        indoc!(
            r"
                x = 4
                y = 3

                (x, y)
                "
        ),
        (4, 3),
        (i64, i64)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn return_tuple_2() {
    assert_evals_to!(
        indoc!(
            r"
                (3, 5)
                "
        ),
        [3, 5],
        [i64; 2]
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn return_tuple_3() {
    assert_evals_to!(
        indoc!(
            r"
                ( 3, 5, 4 )
                "
        ),
        (3, 5, 4),
        (i64, i64, i64)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn return_tuple_4() {
    assert_evals_to!(
        indoc!(
            r"
                ( 3, 5, 4, 2 )
                "
        ),
        [3, 5, 4, 2],
        [i64; 4]
    );
}

// Not supported by wasm because of the size of the tuple:
// FromWasm32Memory is only implemented for tuples of up to 4 elements
#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev"))]
fn return_tuple_5() {
    assert_evals_to!(
        indoc!(
            r"
                ( 3, 5, 4, 2, 1 )
                "
        ),
        [3, 5, 4, 2, 1],
        [i64; 5]
    );
}

// Not supported by wasm because of the size of the tuple:
// FromWasm32Memory is only implemented for tuples of up to 4 elements
#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev"))]
fn return_tuple_6() {
    assert_evals_to!(
        indoc!(
            r"
                ( 3, 5, 4, 2, 1, 7 )
                "
        ),
        [3, 5, 4, 2, 1, 7],
        [i64; 6]
    );
}

// Not supported by wasm because of the size of the tuple:
// FromWasm32Memory is only implemented for tuples of up to 4 elements
#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev"))]
fn return_tuple_7() {
    assert_evals_to!(
        indoc!(
            r"
                ( 3, 5, 4, 2, 1, 7, 8 )
                "
        ),
        [3, 5, 4, 2, 1, 7, 8],
        [i64; 7]
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn return_tuple_float_int() {
    assert_evals_to!(
        indoc!(
            r"
                (1.23f64, 0x1)
                "
        ),
        (1.23, 0x1),
        (f64, i64)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn return_tuple_int_float() {
    assert_evals_to!(
        indoc!(
            r"
                ( 0x1, 1.23f64 )
                "
        ),
        (0x1, 1.23),
        (i64, f64)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn return_tuple_float_float() {
    assert_evals_to!(
        indoc!(
            r"
                ( 2.46f64, 1.23f64 )
                "
        ),
        (2.46, 1.23),
        (f64, f64)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn return_tuple_float_float_float() {
    assert_evals_to!(
        indoc!(
            r"
                ( 2.46f64, 1.23f64, 0.1f64 )
                "
        ),
        (2.46, 1.23, 0.1),
        (f64, f64, f64)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn return_nested_tuple() {
    assert_evals_to!(
        indoc!(
            r"
                (0x0, (2.46f64, 1.23f64, 0.1f64))
                "
        ),
        (0x0, (2.46, 1.23, 0.1)),
        (i64, (f64, f64, f64))
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn nested_tuple_load() {
    assert_evals_to!(
        indoc!(
            r"
                x = (0, (0x2, 0x5, 0x6))

                y = x.1

                y.2
                "
        ),
        6,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn tuple_accessor_twice() {
    assert_evals_to!(".0 (4, 5)  + .1 ( 2.46, 3 ) ", 7, i64);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn tuple_accessor_multi_element_tuple() {
    assert_evals_to!(
        indoc!(
            r#"
                .0 (4, "foo")
                "#
        ),
        4,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn booleans_in_tuple() {
    assert_evals_to!(indoc!("(1 == 1, 1 == 1)"), (true, true), (bool, bool));
    assert_evals_to!(indoc!("(1 != 1, 1 == 1)"), (false, true), (bool, bool));
    assert_evals_to!(indoc!("(1 == 1, 1 != 1)"), (true, false), (bool, bool));
    assert_evals_to!(indoc!("(1 != 1, 1 != 1)"), (false, false), (bool, bool));
}

// TODO: this test fails for mysterious reasons
#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn alignment_in_tuple() {
    assert_evals_to!(
        indoc!("(32, 1 == 1, 78u16)"),
        (32i64, 78u16, true),
        (i64, u16, bool)
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn tuple_length_polymorphism() {
    assert_evals_to!(
        indoc!(
            r"
            a = (42, 43)
            b = (1, 2, 44)

            f : (I64, I64)a, (I64, I64)b -> I64
            f = \(x1, x2), (x3, x4) -> x1 + x2 + x3 + x4

            f a b
            "
        ),
        88,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn generalized_tuple_accessor() {
    assert_evals_to!(
        indoc!(
            r#"
            return0 = .0

            return0 ("foo", 1)
            "#
        ),
        RocStr::from("foo"),
        RocStr
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn generalized_explicit_tuple_accessor() {
    assert_evals_to!(
        indoc!(
            r#"
            return0 = \x -> x.0

            return0 ("foo", 1)
            "#
        ),
        RocStr::from("foo"),
        RocStr
    );
}
