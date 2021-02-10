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
mod gen_num {

    #[test]
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
    fn empty_record() {
        assert_evals_to!("{} == {}", true, bool);
        assert_evals_to!("{} != {}", false, bool);
    }

    #[test]
    fn unit() {
        assert_evals_to!("Unit == Unit", true, bool);
        assert_evals_to!("Unit != Unit", false, bool);
    }

    #[test]
    fn newtype() {
        assert_evals_to!("Identity 42 == Identity 42", true, bool);
        assert_evals_to!("Identity 42 != Identity 42", false, bool);
    }

    #[test]
    fn small_str() {
        assert_evals_to!("\"aaa\" == \"aaa\"", true, bool);
        assert_evals_to!("\"aaa\" == \"bbb\"", false, bool);
        assert_evals_to!("\"aaa\" != \"aaa\"", false, bool);
    }

    #[test]
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
}
