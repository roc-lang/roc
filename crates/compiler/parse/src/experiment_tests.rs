#[cfg(test)]
use crate::experiment::{Env, Listener};
use crate::token::Token;

#[cfg(test)]
#[derive(Debug, Eq, PartialEq, Default)]
struct TestOutput {
    single_line_strings: Vec<(u32, u32)>,
    multi_line_strings: Vec<(u32, u32)>,
    single_quote_chars: Vec<(u32, u32)>,
    comments: Vec<(u32, u32)>,
}

#[cfg(test)]
impl Listener for TestOutput {
    fn emit(&mut self, token: Token, start_offset: u32, end_offset: u32) {
        match token {
            Token::SingleLineStr => self.single_line_strings.push((start_offset, end_offset)),
            Token::MultiLineStr => self.multi_line_strings.push((start_offset, end_offset)),
            Token::SingleQuoteChar => self.single_quote_chars.push((start_offset, end_offset)),
            Token::Comment => self.comments.push((start_offset, end_offset)),
            Token::Lambda => todo!(),
            Token::Indent => todo!(),
            Token::Outdent => todo!(),
            Token::InterpolationStart => todo!(),
            Token::InterpolationEnd => todo!(),
        }
    }
}

macro_rules! gen_test {
    (
        $desc:ident {
            input: $input:expr,
            single_line_strings: $expected_single_line_strings:expr,
            multi_line_strings: $expected_multi_line_strings:expr,
            single_quote_chars: $expected_single_quote_chars:expr,
            comments: $expected_comments:expr
        }
    ) => {
        #[test]
        fn $desc() {
            let input = $input.first().unwrap().replace('¶', "\n");
            let expected = TestOutput {
                single_line_strings: $expected_single_line_strings,
                multi_line_strings: $expected_multi_line_strings,
                single_quote_chars: $expected_single_quote_chars,
                comments: $expected_comments,
            };

            let actual = process_chunk(chunk_from_str(&input));

            debug_assert_eq!(expected, actual);
        }
    };
}

gen_test!(all_spaces {
    input: &[r#"                                                                "#],
    single_line_strings: vec![],
    multi_line_strings: vec![],
    single_quote_chars: vec![],
    comments: vec![]
});

gen_test!(one_string_only {
    input: &[r#"   "blah"                                                       "#],
    single_line_strings: vec![(3, 8)],
    multi_line_strings: vec![],
    single_quote_chars: vec![],
    comments: vec![]
});

gen_test!(two_strings {
    input: &[r#"   "blah"                                               ""      "#],
    single_line_strings: vec![(3, 8), (56, 57)],
    multi_line_strings: vec![],
    single_quote_chars: vec![],
    comments: vec![]
});

gen_test!(one_comment_only {
    input: &[r#"                                                       #blah  ¶ "#],
    single_line_strings: vec![],
    multi_line_strings: vec![],
    single_quote_chars: vec![],
    comments: vec![(55, 62)]
});

gen_test!(two_comments {
    input: &[r#"   #foo¶                                               #blah  ¶ "#],
    single_line_strings: vec![],
    multi_line_strings: vec![],
    single_quote_chars: vec![],
    comments: vec![(3, 7), (55, 62)]
});

gen_test!(one_escaped_quote_no_strings {
    input: &[r#" \"                                                             "#],
    single_line_strings: vec![],
    multi_line_strings: vec![],
    single_quote_chars: vec![],
    comments: vec![]
});

gen_test!(string_with_one_escaped_quote {
    input: &[r#"  "  \"                "                                        "#],
    single_line_strings: vec![(2, 23)],
    multi_line_strings: vec![],
    single_quote_chars: vec![],
    comments: vec![]
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
fn process_chunk(chunk: Chunk) -> TestOutput {
    let mut env = Env::default();
    let mut output = TestOutput::default();

    env.handle_chunk(chunk, &mut output);

    output
}
