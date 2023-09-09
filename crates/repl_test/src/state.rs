use bumpalo::Bump;
use indoc::indoc;
use roc_repl_cli::{evaluate, ReplHelper};
use roc_repl_ui::is_incomplete;
use roc_repl_ui::repl_state::{ReplAction, ReplState};
use roc_reporting::report::DEFAULT_PALETTE;
use roc_target::TargetInfo;
use rustyline::Editor;
use target_lexicon::Triple;

// These are tests of the REPL state machine. They work without actually
// running the CLI, and without using rustyline, and instead verify
// the expected outputs for various sequences of user input strings.

#[test]
fn one_plus_one() {
    complete("1 + 1", &mut ReplState::new(), "2 : Num *", "val1");
}

#[test]
fn generated_expr_names() {
    let mut state = ReplState::new();

    complete("2 * 3", &mut state, "6 : Num *", "val1");
    complete("4 - 1", &mut state, "3 : Num *", "val2");
    complete("val1 + val2", &mut state, "9 : Num *", "val3");
    complete("1 + (val2 * val3)", &mut state, "28 : Num *", "val4");
}

#[test]
fn persisted_defs() {
    let mut state = ReplState::new();

    complete("x = 5", &mut state, "5 : Num *", "x");
    complete("7 - 3", &mut state, "4 : Num *", "val1");
    complete("y = 6", &mut state, "6 : Num *", "y");
    complete("val1 + x + y", &mut state, "15 : Num *", "val2");
}

#[test]
fn annotated_body() {
    let mut input = "t : [A, B, C]".to_string();

    incomplete(&mut input);

    input.push_str("t = A");

    complete(&input, &mut ReplState::new(), "A : [A, B, C]", "t");
}

#[test]
fn exhaustiveness_problem() {
    let mut state = ReplState::new();

    // Enter an annotated tag union to make it exhaustive
    {
        let mut input = "t : [A, B, C]".to_string();

        incomplete(&mut input);

        input.push_str("t = A");

        complete(&input, &mut state, "A : [A, B, C]", "t");
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

            7│>      when t is
            8│>          A -> 1

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
    let arena = Bump::new();
    let target = Triple::host();
    let target_info = TargetInfo::from(&target);
    let action = ReplState::default().step(&arena, "", target_info, DEFAULT_PALETTE);
    assert!(matches!(action, ReplAction::Help));
}

#[test]
fn standalone_annotation() {
    let mut state = ReplState::new();
    let mut input = "x : Str".to_string();

    incomplete(&mut input);
    assert!(!is_incomplete(&input));
    let arena = Bump::new();
    let target = Triple::host();
    let target_info = TargetInfo::from(&target);
    let action = state.step(&arena, &input, target_info, DEFAULT_PALETTE);
    assert!(matches!(action, ReplAction::Nothing));
}

/// validate and step the given input, then check the Result vs the output
/// with ANSI escape codes stripped.
fn complete(input: &str, state: &mut ReplState, expected_start: &str, expected_end: &str) {
    assert!(!is_incomplete(input));
    let arena = Bump::new();
    let target = Triple::host();
    let target_info = TargetInfo::from(&target);
    let action = state.step(&arena, input, target_info, DEFAULT_PALETTE);
    let repl_helper = ReplHelper::default();
    let mut editor = Editor::<ReplHelper>::new();
    editor.set_helper(Some(repl_helper));
    let dimensions = editor.dimensions();

    match action {
        ReplAction::Eval {
            opt_mono,
            problems,
            opt_var_name,
        } => {
            let string = evaluate(opt_mono, problems, opt_var_name, &target, dimensions);
            let escaped =
                std::string::String::from_utf8(strip_ansi_escapes::strip(string.trim()).unwrap())
                    .unwrap();

            let comment_index = escaped.rfind('#').unwrap_or(escaped.len());

            assert_eq!(expected_start, (escaped[0..comment_index].trim()));

            assert_eq!(
                expected_end,
                // +1 because we want to skip over the '#' itself
                (escaped[comment_index + 1..].trim())
            );
        }
        _ => {
            assert!(false, "Unexpected action: {:?}", action);
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
    let arena = Bump::new();
    let target = Triple::host();
    let target_info = TargetInfo::from(&target);
    let action = state.step(&arena, input, target_info, DEFAULT_PALETTE);
    let repl_helper = ReplHelper::default();
    let mut editor = Editor::<ReplHelper>::new();
    editor.set_helper(Some(repl_helper));
    let dimensions = editor.dimensions();

    match action {
        ReplAction::Eval {
            opt_mono,
            problems,
            opt_var_name,
        } => {
            let string = evaluate(opt_mono, problems, opt_var_name, &target, dimensions);
            let escaped =
                std::string::String::from_utf8(strip_ansi_escapes::strip(string.trim()).unwrap())
                    .unwrap();
            assert_eq!(expected_step_result, escaped);
        }
        _ => {
            assert!(false, "Unexpected action: {:?}", action);
        }
    }
}
