use bumpalo::Bump;

/// The purpose of this function is to let us run tests like this:
///
/// ```rust,ignore
/// run_some_test(r#"
///     x = 1
///
///     x
/// "#)
///
/// ...without needing to call a macro like `indoc!` to deal with the fact that
/// multiline Rust string literals preserve all the indented spaces.
///
/// This function removes the indentation by removing leading newlines (e.g. after
/// the `(r#"` opening) and then counting how many spaces precede the first line
/// (e.g. `"        x = 1"` here) and trimming that many spaces from the beginning
/// of each subsequent line. The end of the string is then trimmed normally, and
/// any remaining empty lines are left empty.
pub fn trim_and_deindent<'a>(arena: &'a Bump, input: &'a str) -> &'a str {
    let newline_count = input.chars().filter(|&ch| ch == '\n').count();

    // If it's a single-line string, return it without allocating anything.
    if newline_count == 0 {
        return input.trim(); // Trim to remove spaces
    }

    // Trim leading blank lines - we expect at least one, because the opening line will be `(r#"`
    // (Also, there may be stray blank lines at the start, which this will trim off too.)
    let mut lines = bumpalo::collections::Vec::with_capacity_in(newline_count + 1, arena);

    for line in input
        .lines()
        // Keep skipping until we hit a line that is neither empty nor all spaces.
        .skip_while(|line| line.chars().all(|ch| ch == ' '))
    {
        lines.push(line);
    }

    // Drop trailing blank lines
    while lines
        .last()
        .map_or(false, |line| line.chars().all(|ch| ch == ' '))
    {
        lines.pop();
    }

    // Now that we've trimmed leading and trailing blank lines,
    // Find the smallest indent of the remaining lines. That's our indentation amount.
    let smallest_indent = lines
        .iter()
        .filter(|line| !line.trim().is_empty())
        .map(|line| line.chars().take_while(|&ch| ch == ' ').count())
        .min()
        .unwrap_or(0);

    // Remove this amount of indentation from each line.
    let mut final_str_len = 0;

    lines.iter_mut().for_each(|line| {
        if line.starts_with(' ') {
            *line = line.get(smallest_indent..).unwrap_or("");
        }
        final_str_len += line.len() + 1; // +1 for the newline that will be added to the end of this line.
    });

    // Convert lines into a bumpalo::String
    let mut answer = bumpalo::collections::String::with_capacity_in(final_str_len, arena);

    // Unconditionally push a newline after each line we add. We'll trim off the last one before we return.
    for line in lines {
        answer.push_str(line);
        answer.push('\n');
    }

    // Trim off the extra newline we added at the end. (Saturate to 0 if we ended up with no lines.)
    &answer.into_bump_str()[..final_str_len.saturating_sub(1)]
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_single_line_input() {
        let input = "single line";
        assert_eq!(trim_and_deindent(&Bump::new(), input), "single line");
    }

    #[test]
    fn test_multiline_with_indentation() {
        let input = r#"
        x = 1

        x
    "#;
        let expected = "x = 1\n\nx";
        assert_eq!(trim_and_deindent(&Bump::new(), input), expected);
    }

    #[test]
    fn test_multiline_with_varying_indentation() {
        let input = r#"
        x = 1
          y = 2
        z = 3
    "#;
        let expected = "x = 1\n  y = 2\nz = 3";
        assert_eq!(trim_and_deindent(&Bump::new(), input), expected);
    }

    #[test]
    fn test_multiline_with_empty_lines() {
        let input = r#"
        x = 1

        y = 2

        z = 3
    "#;
        let expected = "x = 1\n\ny = 2\n\nz = 3";
        assert_eq!(trim_and_deindent(&Bump::new(), input), expected);
    }

    #[test]
    fn test_input_without_leading_newline() {
        let input = "    x = 1\n    y = 2";
        let expected = "x = 1\ny = 2";
        assert_eq!(trim_and_deindent(&Bump::new(), input), expected);
    }

    #[test]
    fn test_input_with_multiple_leading_newlines() {
        let input = "\n\n\n    x = 1\n    y = 2";
        let expected = "x = 1\ny = 2";
        assert_eq!(trim_and_deindent(&Bump::new(), input), expected);
    }

    #[test]
    fn test_input_with_mixed_indentation() {
        let input = r#"
        x = 1
    y = 2
            z = 3
    "#;
        let expected = "    x = 1\ny = 2\n        z = 3";
        assert_eq!(trim_and_deindent(&Bump::new(), input), expected);
    }

    #[test]
    fn test_input_with_only_spaces() {
        let input = "    ";
        let expected = "";
        assert_eq!(trim_and_deindent(&Bump::new(), input), expected);
    }

    #[test]
    fn test_input_with_only_newlines() {
        let input = "\n\n\n";
        let expected = "";
        assert_eq!(trim_and_deindent(&Bump::new(), input), expected);
    }

    #[test]
    fn test_input_with_tabs() {
        let input = "\t\tx = 1\n\t\ty = 2";
        let expected = "\t\tx = 1\n\t\ty = 2";
        assert_eq!(trim_and_deindent(&Bump::new(), input), expected);
    }

    #[test]
    fn test_input_with_mixed_spaces_and_tabs() {
        let input = "    \tx = 1\n    \ty = 2";
        let expected = "\tx = 1\n\ty = 2";
        assert_eq!(trim_and_deindent(&Bump::new(), input), expected);
    }

    #[test]
    fn test_input_with_trailing_spaces() {
        let input = "    x = 1    \n    y = 2    ";
        let expected = "x = 1    \ny = 2    ";
        assert_eq!(trim_and_deindent(&Bump::new(), input), expected);
    }

    #[test]
    fn test_input_with_empty_lines_and_spaces() {
        let input = "    x = 1\n    \n    y = 2";
        let expected = "x = 1\n\ny = 2";
        assert_eq!(trim_and_deindent(&Bump::new(), input), expected);
    }

    #[test]
    fn test_input_with_different_indentation_levels() {
        let input = "    x = 1\n        y = 2\n  z = 3";
        let expected = "  x = 1\n      y = 2\nz = 3";
        assert_eq!(trim_and_deindent(&Bump::new(), input), expected);
    }

    #[test]
    fn test_input_with_non_space_characters_at_start() {
        let input = "x = 1\n    y = 2\n        z = 3";
        let expected = "x = 1\n    y = 2\n        z = 3";
        assert_eq!(trim_and_deindent(&Bump::new(), input), expected);
    }

    #[test]
    fn test_empty_input() {
        let input = "";
        let expected = "";
        assert_eq!(trim_and_deindent(&Bump::new(), input), expected);
    }

    #[test]
    fn test_input_with_only_one_indented_line() {
        let input = "    x = 1";
        let expected = "x = 1";
        assert_eq!(trim_and_deindent(&Bump::new(), input), expected);
    }
}
