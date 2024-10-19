/// The purpose of this function is to let us run tests like this:
///
///     run_some_test(r#"
///         x = 1
///
///         x
///     ")
///
/// ...without needing to call a macro like `indoc!` to deal with the fact that
/// multiline Rust string literals preserve all the indented spaces. This takes out
/// the indentation as well as the leading newline in examples like the above, and it's
/// a no-op on single-line strings.
pub fn without_indent(input: &str) -> &str {
    // Ignore any leading newlines, which we expect because the opening line will be `(r#"`
    let input = input.trim_start_matches('\n');
    let leading_spaces = input.chars().take_while(|&ch| ch == ' ').count();

    input
        .lines()
        .map(|line| {
            if line.starts_with(" ") {
                line.get(leading_spaces..).unwrap_or("")
            } else {
                line
            }
        })
        .collect::<Vec<&str>>()
        .join("\n")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_single_line_input() {
        let input = "single line";
        assert_eq!(without_indent(input), "single line");
    }

    #[test]
    fn test_multiline_with_indentation() {
        let input = r#"
        x = 1

        x
    "#;
        let expected = "x = 1\n\nx";
        assert_eq!(without_indent(input), expected);
    }

    #[test]
    fn test_multiline_with_varying_indentation() {
        let input = r#"
        x = 1
          y = 2
        z = 3
    "#;
        let expected = "x = 1\n  y = 2\nz = 3";
        assert_eq!(without_indent(input), expected);
    }

    #[test]
    fn test_multiline_with_empty_lines() {
        let input = r#"
        x = 1

        y = 2

        z = 3
    "#;
        let expected = "x = 1\n\ny = 2\n\nz = 3";
        assert_eq!(without_indent(input), expected);
    }

    #[test]
    fn test_input_without_leading_newline() {
        let input = "    x = 1\n    y = 2";
        let expected = "x = 1\ny = 2";
        assert_eq!(without_indent(input), expected);
    }

    #[test]
    fn test_input_with_multiple_leading_newlines() {
        let input = "\n\n\n    x = 1\n    y = 2";
        let expected = "x = 1\ny = 2";
        assert_eq!(without_indent(input), expected);
    }

    #[test]
    fn test_input_with_mixed_indentation() {
        let input = r#"
        x = 1
    y = 2
            z = 3
    "#;
        let expected = "x = 1\ny = 2\n    z = 3";
        assert_eq!(without_indent(input), expected);
    }
}
