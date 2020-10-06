#[macro_use]
extern crate pretty_assertions;

#[cfg(test)]
mod bitcode {
    use roc_builtins_bitcode::count_delimiters_;

    #[test]
    fn count_delimiters() {
        assert_eq!(
            count_delimiters_((&"hello there").as_bytes(), (&"hello").as_bytes()),
            1
        );
        assert_eq!(
            count_delimiters_((&"a\nb\nc").as_bytes(), (&"\n").as_bytes()),
            2
        );
        assert_eq!(
            count_delimiters_((&"str").as_bytes(), (&"delimiter").as_bytes()),
            0
        );
    }
}
