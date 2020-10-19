#[macro_use]
extern crate pretty_assertions;

#[cfg(test)]
mod bitcode {
    use roc_builtins_bitcode::{count_delimiters_, measure_next_split_segment_length_};

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

    #[test]
    fn measure_next_split_segment() {
        assert_eq!(
            measure_next_split_segment_length_(
                0,
                (&"de!!!de!!!de").as_bytes(),
                (&"!!!").as_bytes()
            ),
            2
        );

        assert_eq!(
            measure_next_split_segment_length_(
                5,
                (&"de!!!abcde!!!de").as_bytes(),
                (&"!!!").as_bytes()
            ),
            5
        );

        assert_eq!(
            measure_next_split_segment_length_(
                13,
                (&"de!!!abcde!!!de").as_bytes(),
                (&"!!!").as_bytes()
            ),
            2
        );

        assert_eq!(
            measure_next_split_segment_length_(0, (&"!!!").as_bytes(), (&"!!!").as_bytes()),
            0
        );

        assert_eq!(
            measure_next_split_segment_length_(0, (&"a!!b!!!").as_bytes(), (&"!!!").as_bytes()),
            4
        );

        assert_eq!(
            measure_next_split_segment_length_(0, (&"abcde!!!").as_bytes(), (&"!!!").as_bytes()),
            5
        );
    }
}
