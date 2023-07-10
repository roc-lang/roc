#[derive(Debug, Eq, PartialEq, Default)]
struct Test {
    description: &'static str,
    input: &'static [&'static str],
    strings: &'static [(usize, usize)],  // (begin_index, length)
    comments: &'static [(usize, usize)], // (begin_index, length)
}

macro_rules! gen_test {
    (
        $desc:ident {
            input: $input:expr,
            strings: $expected_strings:expr,
            comments: $expected_comments:expr
        }
    ) => {
        #[test]
        fn $desc() {
            let input = $input.first().unwrap().replace("¶", "\n");
            let output: Test = process_chunk(chunk_from_str(&input));
            let expected_strings: &'static [(usize, usize)] = $expected_strings;
            let expected_comments: &'static [(usize, usize)] = $expected_comments;

            assert_eq!(expected_strings, output.strings);
            assert_eq!(expected_comments, output.comments);
        }
    };
}

gen_test!(all_spaces {
    input: &[r#"                                                                "#],
    strings: &[],
    comments: &[]
});

gen_test!(one_string_only {
    input: &[r#"   "blah"                                                        "#],
    strings: &[],
    comments: &[(3, 4)]
});

gen_test!(one_comment_only {
    input: &[r#"                                                         #blah   "#],
    strings: &[],
    comments: &[(56, 7)]
});

gen_test!(one_escaped_quote_no_strings {
    input: &[r#" \"                                                              "#],
    strings: &[],
    comments: &[]
});

gen_test!(string_with_one_escaped_quote {
    input: &[r#"  "  \"                "                                         "#],
    strings: &[(2, 20)],
    comments: &[]
});

#[cfg(test)]
type Chunk = [u8; 64];

#[cfg(test)]
fn chunk_from_str(str: &str) -> Chunk {
    if str.len() != 64 {
        panic!(
            "Tried to convert this slice of length {} to a chunk of length 64: {:?}",
            str.len(),
            str
        )
    }

    let mut chunk = [0; 64];

    for (chunk_elem, slice_elem) in chunk.iter_mut().zip(str.as_bytes().iter()) {
        *chunk_elem = *slice_elem;
    }

    chunk
}

#[cfg(test)]
fn process_chunk(_chunk: Chunk) -> Test {
    Test::default()
}
