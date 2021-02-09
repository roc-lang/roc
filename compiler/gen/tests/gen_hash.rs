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
}
