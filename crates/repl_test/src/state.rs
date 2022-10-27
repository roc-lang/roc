use roc_repl_cli::repl_state::{is_incomplete, ReplState, TIPS};

#[test]
fn one_plus_one() {
    complete("1 + 1", &mut ReplState::new(), Ok("2 : Num *   # TODOval1"));
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

    incomplete(&mut input);
    complete(&input, &mut state, Ok(""));
}

/// validate and step the given input, then check the Result vs the input
/// with ANSI escape codes stripped.
fn complete(input: &str, state: &mut ReplState, expected_step_result: Result<&str, i32>) {
    assert!(!is_incomplete(input));

    match state.step(input) {
        Ok(string) => {
            let escaped = std::string::String::from_utf8(
                strip_ansi_escapes::strip(string.trim()).unwrap().into(),
            )
            .unwrap();

            assert_eq!(expected_step_result.map(str::to_string), Ok(escaped));
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
