#[macro_use]
extern crate pretty_assertions;
#[macro_use]
extern crate indoc;

extern crate bumpalo;
extern crate inkwell;
extern crate libc;
extern crate roc_gen;

#[macro_use]
mod helpers;

#[cfg(test)]
mod gen_hash {

    #[test]
    fn basic_hash() {
        assert_evals_to!(
            indoc!(
                r#"
                    Dict.hashTestOnly 0 0
                "#
            ),
            9718519427346233646,
            u64
        );
    }

    #[test]
    fn hash_str_with_seed() {
        assert_evals_to!("Dict.hashTestOnly 1 \"a\"", 0xbed235177f41d328, u64);
        assert_evals_to!("Dict.hashTestOnly 2 \"abc\"", 0xbe348debe59b27c3, u64);
    }

    #[test]
    fn hash_record() {
        assert_evals_to!("Dict.hashTestOnly 1 { x: \"a\" } ", 0xbed235177f41d328, u64);
        assert_evals_to!(
            "Dict.hashTestOnly 1 { x: 42, y: 3.14 } ",
            9902514118285102975,
            u64
        );
    }

    #[test]
    fn hash_result() {
        assert_evals_to!(
            "Dict.hashTestOnly 0 (List.get [ 0x1 ] 0) ",
            11861702399108768034,
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

                    Dict.hashTestOnly 0 input 
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

                    Dict.hashTestOnly 0 input 
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

                    Dict.hashTestOnly 0 (Add x x) 
                "#
            ),
            17097760081222710062,
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

                    Dict.hashTestOnly 0 (Add x x) 
                "#
            ),
            14822035946487008182,
            u64
        );
    }
}
