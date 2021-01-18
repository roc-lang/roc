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
    fn dict_empty_len() {
        assert_evals_to!(
            indoc!(
                r#"
                    Dict.size Dict.empty
                "#
            ),
            1,
            usize
        );
    }
}
