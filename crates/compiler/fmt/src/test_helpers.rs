use bumpalo::Bump;
use roc_test_utils::assert_multiline_str_eq;

use crate::{
    annotation::{Formattable, Newlines, Parens},
    Buf,
};

/// Parse and re-format the given input, and pass the output to `check_formatting`
/// for verification.  The expectation is that `check_formatting` assert the result matches
/// expectations (or, overwrite the expectation based on a command-line flag)
/// Optionally, based on the value of `check_idempotency`, also verify that the formatting
/// is idempotent - that if we reformat the output, we get the same result.
pub fn expr_formats(input: &str, check_formatting: impl Fn(&str), check_idempotency: bool) {
    let arena = Bump::new();
    let input = input.trim();

    match roc_parse::test_helpers::parse_expr_with(&arena, input) {
        Ok(actual) => {
            use crate::spaces::RemoveSpaces;

            let mut buf = Buf::new_in(&arena);

            actual.format_with_options(&mut buf, Parens::NotNeeded, Newlines::Yes, 0);

            let output = buf.as_str();

            check_formatting(output);

            let reparsed_ast = roc_parse::test_helpers::parse_expr_with(&arena, output).unwrap_or_else(|err| {
                panic!(
                    "After formatting, the source code no longer parsed!\n\n\
                    Parse error was: {:?}\n\n\
                    The code that failed to parse:\n\n{}\n\n\
                    The original ast was:\n\n{:#?}\n\n",
                    err, output, actual
                );
            });

            let ast_normalized = actual.remove_spaces(&arena);
            let reparsed_ast_normalized = reparsed_ast.remove_spaces(&arena);

            // HACK!
            // We compare the debug format strings of the ASTs, because I'm finding in practice that _somewhere_ deep inside the ast,
            // the PartialEq implementation is returning `false` even when the Debug-formatted impl is exactly the same.
            // I don't have the patience to debug this right now, so let's leave it for another day...
            // TODO: fix PartialEq impl on ast types
            if format!("{:?}", ast_normalized) != format!("{:?}", reparsed_ast_normalized) {
                panic!(
                    "Formatting bug; formatting didn't reparse to the same AST (after removing spaces)\n\n\
                    * * * Source code before formatting:\n{}\n\n\
                    * * * Source code after formatting:\n{}\n\n\
                    * * * AST before formatting:\n{:#?}\n\n\
                    * * * AST after formatting:\n{:#?}\n\n",
                    input,
                    output,
                    ast_normalized,
                    reparsed_ast_normalized
                );
            }

            // Now verify that the resultant formatting is _idempotent_ - i.e. that it doesn't change again if re-formatted
            if check_idempotency {
                let mut reformatted_buf = Buf::new_in(&arena);
                reparsed_ast.format_with_options(&mut reformatted_buf, Parens::NotNeeded, Newlines::Yes, 0);

                if output != reformatted_buf.as_str() {
                    eprintln!("Formatting bug; formatting is not stable.\nOriginal code:\n{}\n\nFormatted code:\n{}\n\n", input, output);
                    eprintln!("Reformatting the formatted code changed it again, as follows:\n\n");

                    assert_multiline_str_eq!(output, reformatted_buf.as_str());
                }
            }
        }
        Err(error) => panic!("Unexpected parse failure when parsing this for formatting:\n\n{}\n\nParse error was:\n\n{:?}\n\n", input, error)
    };
}
