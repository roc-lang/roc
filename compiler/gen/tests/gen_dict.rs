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
}
