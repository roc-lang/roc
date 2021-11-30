#[cfg(feature = "gen-llvm")]
use crate::helpers::llvm::assert_evals_to;

#[cfg(feature = "gen-dev")]
use crate::helpers::dev::assert_evals_to;

#[cfg(feature = "gen-wasm")]
use crate::helpers::wasm::assert_evals_to;

use indoc::indoc;

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn eq_i64() {
    assert_evals_to!(
        indoc!(
            r#"
                    i : I64
                    i = 1

                    i == i
                "#
        ),
        true,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn neq_i64() {
    assert_evals_to!(
        indoc!(
            r#"
                    i : I64
                    i = 1

                    i != i
                "#
        ),
        false,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn eq_u64() {
    assert_evals_to!(
        indoc!(
            r#"
                    i : U64
                    i = 1

                    i == i
                "#
        ),
        true,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn neq_u64() {
    assert_evals_to!(
        indoc!(
            r#"
                    i : U64
                    i = 1

                    i != i
                "#
        ),
        false,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn eq_f64() {
    assert_evals_to!(
        indoc!(
            r#"
                    i : F64
                    i = 1

                    i == i
                "#
        ),
        true,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn neq_f64() {
    assert_evals_to!(
        indoc!(
            r#"
                    i : F64
                    i = 1

                    i != i
                "#
        ),
        false,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn eq_bool_tag() {
    assert_evals_to!(
        indoc!(
            r#"
                true : Bool
                true = True

                true == True
                "#
        ),
        true,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn neq_bool_tag() {
    assert_evals_to!(
        indoc!(
            r#"
                true : Bool
                true = True

                true == False
                "#
        ),
        false,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn empty_record() {
    assert_evals_to!("{} == {}", true, bool);
    assert_evals_to!("{} != {}", false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn unit() {
    assert_evals_to!("Unit == Unit", true, bool);
    assert_evals_to!("Unit != Unit", false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn newtype() {
    assert_evals_to!("Identity 42 == Identity 42", true, bool);
    assert_evals_to!("Identity 42 != Identity 42", false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn small_str() {
    assert_evals_to!("\"aaa\" == \"aaa\"", true, bool);
    assert_evals_to!("\"aaa\" == \"bbb\"", false, bool);
    assert_evals_to!("\"aaa\" != \"aaa\"", false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn large_str() {
    assert_evals_to!(
        indoc!(
            r#"
                x = "Unicode can represent text values which span multiple languages"
                y = "Unicode can represent text values which span multiple languages"

                x == y
                "#
        ),
        true,
        bool
    );

    assert_evals_to!(
        indoc!(
            r#"
                x = "Unicode can represent text values which span multiple languages"
                y = "Here are some valid Roc strings"

                x != y
                "#
        ),
        true,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn eq_result_tag_true() {
    assert_evals_to!(
        indoc!(
            r#"
                x : Result I64 I64
                x = Ok 1

                y : Result I64 I64
                y = Ok 1

                x == y
                "#
        ),
        true,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn eq_result_tag_false() {
    assert_evals_to!(
        indoc!(
            r#"
                x : Result I64 I64
                x = Ok 1

                y : Result I64 I64
                y = Err 1

                x == y
                "#
        ),
        false,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn eq_expr() {
    assert_evals_to!(
        indoc!(
            r#"
                Expr : [ Add Expr Expr, Mul Expr Expr, Val I64, Var I64 ]

                x : Expr
                x = Val 0

                y : Expr
                y = Val 0

                x == y
                "#
        ),
        true,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn eq_linked_list() {
    assert_evals_to!(
        indoc!(
            r#"
                LinkedList a : [ Nil, Cons a (LinkedList a) ]

                x : LinkedList I64
                x = Nil

                y : LinkedList I64
                y = Nil

                x == y
                "#
        ),
        true,
        bool
    );

    assert_evals_to!(
        indoc!(
            r#"
                LinkedList a : [ Nil, Cons a (LinkedList a) ]

                x : LinkedList I64
                x = Cons 1 Nil

                y : LinkedList I64
                y = Cons 1 Nil

                x == y
                "#
        ),
        true,
        bool
    );

    assert_evals_to!(
        indoc!(
            r#"
                LinkedList a : [ Nil, Cons a (LinkedList a) ]

                x : LinkedList I64
                x = Cons 1 (Cons 2 Nil)

                y : LinkedList I64
                y = Cons 1 (Cons 2 Nil)

                x == y
                "#
        ),
        true,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn eq_linked_list_false() {
    assert_evals_to!(
        indoc!(
            r#"
                LinkedList a : [ Nil, Cons a (LinkedList a) ]

                x : LinkedList I64
                x = Cons 1 Nil

                y : LinkedList I64
                y = Cons 1 (Cons 2 Nil)

                y == x
                "#
        ),
        false,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn eq_nullable_expr() {
    assert_evals_to!(
        indoc!(
            r#"
                Expr : [ Add Expr Expr, Mul Expr Expr, Val I64, Empty ]

                x : Expr
                x = Val 0

                y : Expr
                y = Add x x

                x != y
                "#
        ),
        true,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn eq_rosetree() {
    assert_evals_to!(
        indoc!(
            r#"
                Rose a : [ Rose (List (Rose a)) ]

                x : Rose I64
                x = Rose []

                y : Rose I64
                y = Rose []

                x == y
                "#
        ),
        true,
        bool
    );

    assert_evals_to!(
        indoc!(
            r#"
                Rose a : [ Rose (List (Rose a)) ]

                x : Rose I64
                x = Rose []

                y : Rose I64
                y = Rose []

                x != y
                "#
        ),
        false,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
#[ignore]
fn rosetree_with_tag() {
    // currently stack overflows in type checking

    assert_evals_to!(
        indoc!(
            r#"
                Rose a : [ Rose (Result (List (Rose a)) I64) ]

                x : Rose I64 
                x = (Rose (Ok []))

                y : Rose I64 
                y = (Rose (Ok []))

                x == y
                "#
        ),
        true,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_eq_empty() {
    assert_evals_to!("[] == []", true, bool);
    assert_evals_to!("[] != []", false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_eq_by_length() {
    assert_evals_to!("[1] == []", false, bool);
    assert_evals_to!("[] == [1]", false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_eq_compare_pointwise() {
    assert_evals_to!("[1] == [1]", true, bool);
    assert_evals_to!("[2] == [1]", false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_eq_nested() {
    assert_evals_to!("[[1]] == [[1]]", true, bool);
    assert_evals_to!("[[2]] == [[1]]", false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_neq_compare_pointwise() {
    assert_evals_to!("[1] != [1]", false, bool);
    assert_evals_to!("[2] != [1]", true, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn list_neq_nested() {
    assert_evals_to!("[[1]] != [[1]]", false, bool);
    assert_evals_to!("[[2]] != [[1]]", true, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn compare_union_same_content() {
    assert_evals_to!(
        indoc!(
            r#"
            Foo : [ A I64, B I64 ]

            a : Foo
            a = A 42

            b : Foo
            b = B 42

            a == b
            "#
        ),
        false,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn compare_recursive_union_same_content() {
    assert_evals_to!(
        indoc!(
            r#"
                Expr : [ Add Expr Expr, Mul Expr Expr, Val1 I64, Val2 I64 ]

                v1 : Expr 
                v1 = Val1 42

                v2 : Expr 
                v2 = Val2 42

                v1 == v2
            "#
        ),
        false,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn compare_nullable_recursive_union_same_content() {
    assert_evals_to!(
        indoc!(
            r#"
                Expr : [ Add Expr Expr, Mul Expr Expr, Val1 I64, Val2 I64, Empty ]

                v1 : Expr 
                v1 = Val1 42

                v2 : Expr 
                v2 = Val2 42

                v1 == v2
            "#
        ),
        false,
        bool
    );
}
