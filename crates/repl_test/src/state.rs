use roc_repl_cli::repl_state::{is_incomplete, ReplState, TIPS};

// These are tests of the REPL state machine. They work without actually
// running the CLI, and without using rustyline, and instead verify
// the expected outputs for various sequences of user input strings.

#[test]
fn one_plus_one() {
    complete("1 + 1", &mut ReplState::new(), Ok(("2 : Num *", "val1")));
}

#[test]
fn tips() {
    assert!(!is_incomplete(""));
    assert_eq!(ReplState::new().step(""), Ok(format!("\n{TIPS}\n")));
}

#[test]
fn standalone_annotation() {
    let mut state = ReplState::new();
    let mut input = "x : Str".to_string();

    assert_eq!(&state.with_past_defs("test"), "test");

    incomplete(&mut input);
    assert!(!is_incomplete(&input));
    assert_eq!(state.step(&input), Ok(String::new()));

    assert_eq!(&state.with_past_defs("test"), "x : Str\n\ntest");
}

#[test]
fn multiline_def() {
    todo!("x =\n1");
}

/// validate and step the given input, then check the Result vs the input
/// with ANSI escape codes stripped.
fn complete(input: &str, state: &mut ReplState, expected_step_result: Result<(&str, &str), i32>) {
    assert!(!is_incomplete(input));

    match state.step(input) {
        Ok(string) => {
            let escaped = std::string::String::from_utf8(
                strip_ansi_escapes::strip(string.trim()).unwrap().into(),
            )
            .unwrap();

            let comment_index = escaped.rfind('#').unwrap_or_else(|| escaped.len());

            assert_eq!(
                expected_step_result.map(|(starts_with, _)| starts_with),
                Ok(*&escaped[0..comment_index].trim())
            );

            assert_eq!(
                expected_step_result.map(|(_, ends_with)| ends_with),
                // +1 because we want to skip over the '#' itself
                Ok(*&escaped[comment_index + 1..].trim())
            );
        }
        Err(err) => {
            assert_eq!(expected_step_result, Err(err));
        }
    }
}

fn incomplete(input: &mut String) {
    assert!(is_incomplete(input));

    // Since this was incomplete, rustyline won't step the state. Instead, it will
    // remember the input (with a newline appended) for next time.
    input.push('\n');
}
