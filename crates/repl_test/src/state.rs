use indoc::indoc;
use roc_repl_cli::repl_state::{is_incomplete, ReplState, TIPS};

// These are tests of the REPL state machine. They work without actually
// running the CLI, and without using rustyline, and instead verify
// the expected outputs for various sequences of user input strings.

#[test]
fn one_plus_one() {
    complete("1 + 1", &mut ReplState::new(), Ok(("2 : Num *", "val1")));
}

#[test]
fn generated_expr_names() {
    let mut state = ReplState::new();

    complete("2 * 3", &mut state, Ok(("6 : Num *", "val1")));
    complete("4 - 1", &mut state, Ok(("3 : Num *", "val2")));
    complete("val1 + val2", &mut state, Ok(("9 : Num *", "val3")));
    complete("1 + (val2 * val3)", &mut state, Ok(("28 : Num *", "val4")));
}

#[test]
fn persisted_defs() {
    let mut state = ReplState::new();

    complete("x = 5", &mut state, Ok(("5 : Num *", "x")));
    complete("7 - 3", &mut state, Ok(("4 : Num *", "val1")));
    complete("y = 6", &mut state, Ok(("6 : Num *", "y")));
    complete("val1 + x + y", &mut state, Ok(("15 : Num *", "val2")));
}

#[test]
fn annotated_body() {
    let mut input = "t : [A, B, C]".to_string();

    incomplete(&mut input);

    input.push_str("t = A");

    complete(&input, &mut ReplState::new(), Ok(("A : [A, B, C]", "t")));
}

#[test]
fn exhaustiveness_problem() {
    let mut state = ReplState::new();

    // Enter an annotated tag union to make it exhaustive
    {
        let mut input = "t : [A, B, C]".to_string();

        incomplete(&mut input);

        input.push_str("t = A");

        complete(&input, &mut state, Ok(("A : [A, B, C]", "t")));
    }

    // Run a `when` on it that isn't exhaustive
    {
        let mut input = "when t is".to_string();
        incomplete(&mut input);

        input.push_str("    A -> 1");
        incomplete(&mut input);

        const EXPECTED_ERROR: &str = indoc!(
            r#"
            ── UNSAFE PATTERN ──────────────────────────────────────────────────────────────

            This when does not cover all the possibilities:

            6│>      when t is
            7│>          A -> 1

            Other possibilities include:

                B
                C

            I would have to crash if I saw one of those! Add branches for them!"#
        );

        error(&input, &mut state, EXPECTED_ERROR.to_string());
    }
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

/// validate and step the given input, then check the Result vs the output
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

/// validate and step the given input, then check the given string vs the output
/// with ANSI escape codes stripped.
fn error(input: &str, state: &mut ReplState, expected_step_result: String) {
    assert!(!is_incomplete(input));

    let escaped = state.step(input).map(|string| {
        std::string::String::from_utf8(strip_ansi_escapes::strip(string.trim()).unwrap().into())
            .unwrap()
    });

    assert_eq!(Ok(expected_step_result), escaped);
}
