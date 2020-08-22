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
        assert_evals_to!("Str.concat \"a\" \"b\"", "ab", &'static str);
        assert_evals_to!("Str.concat \"\" \"second str\"", "second str", &'static str);
        assert_evals_to!("Str.concat \"first str\" \"\"", "first str", &'static str);
        assert_evals_to!("Str.concat \"\" \"\"", "", &'static str);
        assert_evals_to!(
            indoc!(
                r#"
                    Str.concat
                        "First string that is fairly long. Longer strings make for different errors. "
                        "Second string that is also fairly long. Two long strings test things that might not appear with short strings."
                "#
            ),
            "First string that is fairly long. Longer strings make for different errors. Second string that is also fairly long. Two long strings test things that might not appear with short strings.",
            &'static str
        );
    }
}
