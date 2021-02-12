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
mod gen_dict {
    #[test]
    fn dict_empty_len() {
        assert_evals_to!(
            indoc!(
                r#"
                    Dict.len Dict.empty
                "#
            ),
            0,
            usize
        );
    }

    #[test]
    fn dict_insert_empty() {
        assert_evals_to!(
            indoc!(
                r#"
                Dict.insert Dict.empty 42 32
                    |> Dict.len
                "#
            ),
            1,
            usize
        );
    }

    #[test]
    fn dict_empty_contains() {
        assert_evals_to!(
            indoc!(
                r#"
                empty : Dict I64 F64
                empty = Dict.empty

                Dict.contains empty 42
                "#
            ),
            false,
            bool
        );
    }

    #[test]
    fn dict_nonempty_contains() {
        assert_evals_to!(
            indoc!(
                r#"
                empty : Dict I64 F64
                empty = Dict.insert Dict.empty 42 3.14

                Dict.contains empty 42
                "#
            ),
            true,
            bool
        );
    }

    #[test]
    fn dict_empty_remove() {
        assert_evals_to!(
            indoc!(
                r#"
                empty : Dict I64 F64
                empty = Dict.empty

                empty
                    |> Dict.remove 42
                    |> Dict.len
                "#
            ),
            0,
            i64
        );
    }

    #[test]
    fn dict_nonempty_remove() {
        assert_evals_to!(
            indoc!(
                r#"
                empty : Dict I64 F64
                empty = Dict.insert Dict.empty 42 3.14

                empty
                    |> Dict.remove 42
                    |> Dict.len
                "#
            ),
            0,
            i64
        );
    }
}
