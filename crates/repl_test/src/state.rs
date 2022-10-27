use roc_repl_cli::repl_state::{validate, ReplState};
use rustyline::validate::ValidationResult;

#[test]
fn one_plus_one() {
    let mut state = ReplState::new();
    let input = "1 + 1";

    assert_step(input, &mut state, Ok("2 : Num *   # TODOval1"));
    assert!(matches!(validate(input), Ok(ValidationResult::Valid(None))));
}

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
