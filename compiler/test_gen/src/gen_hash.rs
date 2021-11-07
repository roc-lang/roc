#![cfg(test)]

use crate::assert_evals_to;
// use crate::assert_wasm_evals_to as assert_evals_to;
use indoc::indoc;

#[test]
fn basic_hash() {
    assert_evals_to!(
        indoc!(
            r#"
                Dict.#hashTestOnly 0 0
            "#
        ),
        9718519427346233646,
        u64
    );
}

#[test]
fn hash_str_with_seed() {
    assert_evals_to!("Dict.#hashTestOnly 1 \"a\"", 0xbed235177f41d328, u64);
    assert_evals_to!("Dict.#hashTestOnly 2 \"abc\"", 0xbe348debe59b27c3, u64);
}

#[test]
fn hash_record() {
    assert_evals_to!(
        "Dict.#hashTestOnly 1 { x: \"a\" } ",
        0xbed235177f41d328,
        u64
    );
    assert_evals_to!(
        "Dict.#hashTestOnly 1 { x: 42, y: 3.14 } ",
        5348189196103430707,
        u64
    );
}

#[test]
fn hash_result() {
    assert_evals_to!(
        "Dict.#hashTestOnly 0 (List.get [ 0x1 ] 0) ",
        2878521786781103245,
        u64
    );
}

#[test]
fn hash_linked_list() {
    assert_evals_to!(
        indoc!(
            r#"
                LinkedList a : [ Nil, Cons a (LinkedList a) ]

                input : LinkedList I64
                input = Nil

                Dict.#hashTestOnly 0 input 
            "#
        ),
        0,
        u64
    );

    assert_evals_to!(
        indoc!(
            r#"
                LinkedList a : [ Nil, Cons a (LinkedList a) ]

                input : LinkedList I64
                input = Cons 4 (Cons 3 Nil)

                Dict.#hashTestOnly 0 input 
            "#
        ),
        8287696503006938486,
        u64
    );
}

#[test]
fn hash_expr() {
    assert_evals_to!(
        indoc!(
            r#"
                Expr : [ Add Expr Expr, Mul Expr Expr, Val I64, Var I64 ]

                x : Expr 
                x = Val 1

                add : Expr
                add = Add x x

                Dict.#hashTestOnly 0 add
            "#
        ),
        10825806964604997723,
        u64
    );
}

#[test]
fn hash_nullable_expr() {
    assert_evals_to!(
        indoc!(
            r#"
                Expr : [ Add Expr Expr, Mul Expr Expr, Val I64, Empty ]

                x : Expr 
                x = Val 1

                add : Expr
                add = Add x x

                Dict.#hashTestOnly 0 add
            "#
        ),
        1907558799788307114,
        u64
    );
}

#[test]
fn hash_rosetree() {
    assert_evals_to!(
        indoc!(
            r#"
            Rose a : [ Rose (List (Rose a)) ]

            x : Rose I64 
            x = Rose [] 

            Dict.#hashTestOnly 0 x
            "#
        ),
        0,
        u64
    );
}

#[test]
fn hash_union_same_content() {
    assert_evals_to!(
        indoc!(
            r#"
            Foo : [ A I64, B I64 ]

            a : Foo
            a = A 42

            b : Foo
            b = B 42

            { a: Dict.#hashTestOnly 0 a, b : Dict.#hashTestOnly 0 b }
            "#
        ),
        true,
        (i64, i64),
        |(a, b)| a != b
    );
}

#[test]
fn hash_recursive_union_same_content() {
    assert_evals_to!(
        indoc!(
            r#"
                Expr : [ Add Expr Expr, Mul Expr Expr, Val1 I64, Val2 I64 ]

                v1 : Expr 
                v1 = Val1 42

                v2 : Expr 
                v2 = Val2 42

                { a: Dict.#hashTestOnly 0 v1, b : Dict.#hashTestOnly 0 v2 }
            "#
        ),
        true,
        (i64, i64),
        |(a, b)| a != b
    );
}

#[test]
fn hash_nullable_recursive_union_same_content() {
    assert_evals_to!(
        indoc!(
            r#"
                Expr : [ Add Expr Expr, Mul Expr Expr, Val1 I64, Val2 I64, Empty ]

                v1 : Expr 
                v1 = Val1 42

                v2 : Expr 
                v2 = Val2 42

                { a: Dict.#hashTestOnly 0 v1, b : Dict.#hashTestOnly 0 v2 }
            "#
        ),
        true,
        (i64, i64),
        |(a, b)| a != b
    );
}

#[test]
fn hash_list() {
    assert_evals_to!(
        indoc!(
            r#"
            x : List Str 
            x = [ "foo", "bar", "baz" ] 

            Dict.#hashTestOnly 0 x
            "#
        ),
        10731521034618280801,
        u64
    );
}
