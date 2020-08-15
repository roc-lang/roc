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
mod gen_str {
    #[test]
    fn str_concat() {
        assert_evals_to!("Str.concat \"a\" \"b\"", "abc", &'static str);
    }
}
