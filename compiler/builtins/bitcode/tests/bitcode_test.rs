#[macro_use]
extern crate pretty_assertions;

#[cfg(test)]
mod bitcode {
    use roc_builtins_bitcode::{count_segments_, str_split_};

    #[test]
    fn count_segments() {
        assert_eq!(
            count_segments_((&"hello there").as_bytes(), (&"hello").as_bytes()),
            2
        );
        assert_eq!(
            count_segments_((&"a\nb\nc").as_bytes(), (&"\n").as_bytes()),
            3
        );
        assert_eq!(
            count_segments_((&"str").as_bytes(), (&"delimiter").as_bytes()),
            1
        );
    }

    #[test]
    fn str_split() {
        fn splits_to(string: &str, delimiter: &str, expectation: &[&[u8]]) {
            assert_eq!(
                str_split_(
                    &mut [(&"").as_bytes()].repeat(expectation.len()),
                    &string.as_bytes(),
                    &delimiter.as_bytes()
                ),
                expectation
            );
        }

        splits_to(
            "a!b!c",
            "!",
            &[(&"a").as_bytes(), (&"b").as_bytes(), (&"c").as_bytes()],
        );

        splits_to(
            "a!?b!?c!?",
            "!?",
            &[
                (&"a").as_bytes(),
                (&"b").as_bytes(),
                (&"c").as_bytes(),
                (&"").as_bytes(),
            ],
        );

        splits_to("abc", "!", &[(&"abc").as_bytes()]);

        splits_to(
            "tttttghittttt",
            "ttttt",
            &[(&"").as_bytes(), (&"ghi").as_bytes(), (&"").as_bytes()],
        );

        splits_to("def", "!!!!!!", &[(&"def").as_bytes()]);
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
