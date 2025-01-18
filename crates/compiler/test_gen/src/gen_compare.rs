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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn eq_bool_tag() {
    assert_evals_to!(
        indoc!(
            r#"
                true : Bool
                true = Bool.true

                true == Bool.true
                "#
        ),
        true,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn neq_bool_tag() {
    assert_evals_to!(
        indoc!(
            r#"
                true : Bool
                true = Bool.true

                true == Bool.false
                "#
        ),
        false,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn bool_logic() {
    assert_evals_to!(
        indoc!(
            r#"
                bool1 = Bool.true
                bool2 = Bool.false
                bool3 = !bool1

                (bool1 and bool2) or bool2 and bool3
                "#
        ),
        false,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn and_bool() {
    assert_evals_to!("Bool.true and Bool.true", true, bool);
    assert_evals_to!("Bool.true and Bool.false", false, bool);
    assert_evals_to!("Bool.false and Bool.true", false, bool);
    assert_evals_to!("Bool.false and Bool.false", false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn or_bool() {
    assert_evals_to!("Bool.true or Bool.true", true, bool);
    assert_evals_to!("Bool.true or Bool.false", true, bool);
    assert_evals_to!("Bool.false or Bool.true", true, bool);
    assert_evals_to!("Bool.false or Bool.false", false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn not_bool() {
    assert_evals_to!("!Bool.true", false, bool);
    assert_evals_to!("!Bool.false", true, bool);

    assert_evals_to!("!(!Bool.true)", true, bool);
    assert_evals_to!("!(!Bool.false)", false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn empty_record() {
    assert_evals_to!("{} == {}", true, bool);
    assert_evals_to!("{} != {}", false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn record() {
    assert_evals_to!(
        "{ x: 123, y: \"Hello\", z: 3.14 } == { x: 123, y: \"Hello\", z: 3.14 }",
        true,
        bool
    );

    assert_evals_to!(
        "{ x: 234, y: \"Hello\", z: 3.14 } == { x: 123, y: \"Hello\", z: 3.14 }",
        false,
        bool
    );
    assert_evals_to!(
        "{ x: 123, y: \"World\", z: 3.14 } == { x: 123, y: \"Hello\", z: 3.14 }",
        false,
        bool
    );
    assert_evals_to!(
        "{ x: 123, y: \"Hello\", z: 1.11 } == { x: 123, y: \"Hello\", z: 3.14 }",
        false,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn unit() {
    assert_evals_to!("Unit == Unit", true, bool);
    assert_evals_to!("Unit != Unit", false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn newtype() {
    assert_evals_to!("Identity 42 == Identity 42", true, bool);
    assert_evals_to!("Identity 42 != Identity 42", false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn small_str() {
    assert_evals_to!("\"aaa\" == \"aaa\"", true, bool);
    assert_evals_to!("\"aaa\" == \"bbb\"", false, bool);
    assert_evals_to!("\"aaa\" != \"aaa\"", false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn eq_expr() {
    assert_evals_to!(
        indoc!(
            r#"
                Expr : [Add Expr Expr, Mul Expr Expr, Val I64, Var I64]

                x : Expr
                x = Val(0)

                y : Expr
                y = Val(0)

                x == y
                "#
        ),
        true,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn eq_linked_list() {
    assert_evals_to!(
        indoc!(
            r#"
                LinkedList a : [Nil, Cons a (LinkedList a)]

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
                LinkedList a : [Nil, Cons a (LinkedList a)]

                x : LinkedList I64
                x = Cons(1, Nil)

                y : LinkedList I64
                y = Cons(1, Nil)

                x == y
                "#
        ),
        true,
        bool
    );

    assert_evals_to!(
        indoc!(
            r#"
                LinkedList a : [Nil, Cons a (LinkedList a)]

                x : LinkedList I64
                x = Cons(1, Cons(2, Nil))

                y : LinkedList I64
                y = Cons(1, Cons(2, Nil))

                x == y
                "#
        ),
        true,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn eq_linked_list_false() {
    assert_evals_to!(
        indoc!(
            r#"
                LinkedList a : [Nil, Cons a (LinkedList a)]

                x : LinkedList I64
                x = Cons(1, Nil)

                y : LinkedList I64
                y = Cons(1, Cons(2, Nil))

                y == x
                "#
        ),
        false,
        bool
    );
}

#[test]
#[ignore] // breaks for LLVM (no tail recursion), takes a long time for Wasm
fn eq_linked_list_long() {
    assert_evals_to!(
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
                    n = 100_000 # be careful, can make a noticeble difference to test_gen total time!

                    x : LinkedList I64
                    x = prepend_ones n (Cons 999 Nil)

                    y : LinkedList I64
                    y = prepend_ones n (Cons 123 Nil)

                    y == x
                "#
        ),
        false,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn eq_nullable_expr() {
    assert_evals_to!(
        indoc!(
            r#"
                Expr : [Add Expr Expr, Mul Expr Expr, Val I64, Empty]

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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]

fn eq_rosetree() {
    assert_evals_to!(
        indoc!(
            r#"
                Rose a : [Rose a (List (Rose a))]

                x : Rose I64
                x = Rose 0 []

                y : Rose I64
                y = Rose 0 []

                x == y
                "#
        ),
        true,
        bool
    );

    assert_evals_to!(
        indoc!(
            r#"
                Rose a : [Rose a (List (Rose a))]

                x : Rose I64
                x = Rose 0 []

                y : Rose I64
                y = Rose 0 []

                x != y
                "#
        ),
        false,
        bool
    );

    assert_evals_to!(
        indoc!(
            r#"
                Rose a : [Rose a (List (Rose a))]

                a1 : Rose I64
                a1 = Rose 999 []

                a2 : Rose I64
                a2 = Rose 0 [a1, a1]

                a3 : Rose I64
                a3 = Rose 0 [a2, a2, a2]

                b1 : Rose I64
                b1 = Rose 111 []

                b2 : Rose I64
                b2 = Rose 0 [a1, b1]

                b3 : Rose I64
                b3 = Rose 0 [a2, a2, b2]

                a3 == b3
                "#
        ),
        false,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn eq_different_rosetrees() {
    // Requires two different equality procedures for `List (Rose I64)` and `List (Rose Str)`
    // even though both appear in the mono Layout as `List(RecursivePointer)`
    assert_evals_to!(
        indoc!(
            r#"
                Rose a : [Rose a (List (Rose a))]

                a1 : Rose I64
                a1 = Rose 999 []
                a2 : Rose I64
                a2 = Rose 0 [a1]

                b1 : Rose I64
                b1 = Rose 999 []
                b2 : Rose I64
                b2 = Rose 0 [b1]

                ab = a2 == b2

                c1 : Rose Str
                c1 = Rose "hello" []
                c2 : Rose Str
                c2 = Rose "" [c1]

                d1 : Rose Str
                d1 = Rose "hello" []
                d2 : Rose Str
                d2 = Rose "" [d1]

                cd = c2 == d2

                ab and cd
        "#
        ),
        true,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn rosetree_with_tag() {
    assert_evals_to!(
        indoc!(
            r#"
                Rose a : [Rose (Result (List (Rose a)) I64)]

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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_eq_empty() {
    assert_evals_to!("[] == []", true, bool);
    assert_evals_to!("[] != []", false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_eq_by_length() {
    assert_evals_to!("[1] == []", false, bool);
    assert_evals_to!("[] == [1]", false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_eq_compare_pointwise() {
    assert_evals_to!("[1] == [1]", true, bool);
    assert_evals_to!("[2] == [1]", false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_eq_nested() {
    assert_evals_to!("[[1]] == [[1]]", true, bool);
    assert_evals_to!("[[2]] == [[1]]", false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_neq_compare_pointwise() {
    assert_evals_to!("[1] != [1]", false, bool);
    assert_evals_to!("[2] != [1]", true, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn list_neq_nested() {
    assert_evals_to!("[[1]] != [[1]]", false, bool);
    assert_evals_to!("[[2]] != [[1]]", true, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn compare_union_same_content() {
    assert_evals_to!(
        indoc!(
            r#"
            Foo : [A I64, B I64]

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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn compare_recursive_union_same_content() {
    assert_evals_to!(
        indoc!(
            r#"
                Expr : [Add Expr Expr, Mul Expr Expr, Val1 I64, Val2 I64]

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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn compare_nullable_recursive_union_same_content() {
    assert_evals_to!(
        indoc!(
            r#"
                Expr : [Add Expr Expr, Mul Expr Expr, Val1 I64, Val2 I64, Empty]

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
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn boxed_eq_int() {
    assert_evals_to!("Box.box 1i64 == Box.box 1", true, bool);
    assert_evals_to!("Box.box 2i64 == Box.box 1", false, bool);
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn boxed_eq_str() {
    assert_evals_to!(
        "Box.box \"Hello, world\" == Box.box \"Hello, world\"",
        true,
        bool
    );
    assert_evals_to!(
        "Box.box \"Hello, world\" == Box.box \"Hello, stranger\"",
        false,
        bool
    );
}
