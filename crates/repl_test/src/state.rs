use roc_repl_cli::repl_state::{validate, ReplState};
use rustyline::validate::ValidationResult;

#[test]
fn one_plus_one() {
    let mut state = ReplState::new();
    let input = "1 + 1";

    assert_validates(input);
    assert_step(input, &mut state, Ok("2 : Num *   # TODOval1"));
    assert_done(&state);
}

#[test]
fn incomplete_annotation() {
    let mut state = ReplState::new();
    let input = "x : Str";

    assert_incomplete(input);
    assert_step(input, &mut state, Ok(""));
    assert_done(&state);
}

fn assert_validates(input: &str) {
    assert!(matches!(validate(input), Ok(ValidationResult::Valid(None))));
}

fn assert_incomplete(input: &str) {
    assert!(matches!(validate(input), Ok(ValidationResult::Incomplete)));
}

/// step the given input, then check the Result vs the trimmed input with ANSI escape codes stripped.
fn assert_step(input: &str, state: &mut ReplState, expected_step_result: Result<&str, i32>) {
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

fn assert_done(state: &ReplState) {
    assert_eq!(
        state.pending_src,
        String::new(),
        "pending_src was not empty; it was {:?}",
        state.pending_src
    );
    assert!(!state.prev_line_blank, "prev_line_blank was true");
}
