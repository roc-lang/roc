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
            1,
            u64
        );
    }
}
