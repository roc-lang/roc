use bumpalo::Bump;
use indoc::indoc;
use roc_repl_cli::{evaluate, ReplHelper};
use roc_repl_ui::is_incomplete;
use roc_repl_ui::repl_state::{ReplAction, ReplState};
use roc_reporting::report::DEFAULT_PALETTE;
use rustyline::Editor;
use target_lexicon::Triple;

// These are tests of the REPL state machine. They work without actually
// running the CLI, and without using rustyline, and instead verify
// the expected outputs for various sequences of user input strings.

#[test]
fn one_plus_one() {
    complete("1 + 1", &mut ReplState::new(), "2 : Num *");
}

#[test]
fn persisted_defs() {
    let mut state = ReplState::new();

    complete("x = 5", &mut state, "5 : Num *");
    complete("7 - 3", &mut state, "4 : Num *");
    complete("y = 6", &mut state, "6 : Num *");
}

#[test]
fn annotated_body() {
    let mut input = "t : [A, B, C]".to_string();

    incomplete(&mut input);

    input.push_str("t = A");

    complete(&input, &mut ReplState::new(), "A : [A, B, C]");
}

#[test]
fn exhaustiveness_problem() {
    let mut state = ReplState::new();

    // Enter an annotated tag union to make it exhaustive
    {
        let mut input = "t : [A, B, C]".to_string();

        incomplete(&mut input);

        input.push_str("t = A");

        complete(&input, &mut state, "A : [A, B, C]");
    }

    // Run a `when` on it that isn't exhaustive
    {
        let mut input = "when t is".to_string();
        incomplete(&mut input);

        input.push_str("    A -> 1");
        incomplete(&mut input);

        let expected_error: &str = indoc!(
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

        error(&input, &mut state, expected_error.to_string());
    }
}

#[test]
fn partial_record_definition() {
    // Partially define a record successfully
    {
        let mut state = ReplState::new();
        let mut input = "successfulRecord = {".to_string();
        incomplete(&mut input);

        input.push_str("field: \"field\",");
        incomplete(&mut input);

        input.push('}');
        complete(&input, &mut state, "{ field: \"field\" } : { field : Str }");
    }

    // Partially define a record incompletely
    {
        let mut state = ReplState::new();
        let mut input = "failed_record = {".to_string();
        incomplete(&mut input);

        input.push_str("field: \"field\",");
        incomplete(&mut input);

        input.push('\n');
        let expected_error: &str = indoc!(
            r#"
            ── RECORD PARSE PROBLEM ────────────────────────────────────────────────────────

            I am partway through parsing a record, but I got stuck here:

            1│  app "app" provides [repl_output] to "./platform"
            2│
            3│  repl_output =
            4│      failed_record = {
                                    ^

            TODO provide more context."#
        );
        error(&input, &mut state, expected_error.to_string());
    }
}

#[test]
fn tips() {
    assert!(!is_incomplete(""));
    let arena = Bump::new();
    let target = Triple::host().into();
    let action = ReplState::default().step(&arena, "", target, DEFAULT_PALETTE);
    assert!(matches!(action, ReplAction::Help));
}

#[test]
fn standalone_annotation() {
    let mut state = ReplState::new();
    let mut input = "x : Str".to_string();

    incomplete(&mut input);
    assert!(!is_incomplete(&input));
    let arena = Bump::new();
    let target = Triple::host().into();
    let action = state.step(&arena, &input, target, DEFAULT_PALETTE);
    assert!(matches!(action, ReplAction::Nothing));
}

/// validate and step the given input, then check the Result vs the output
/// with ANSI escape codes stripped.
fn complete(input: &str, state: &mut ReplState, expected_start: &str) {
    assert!(!is_incomplete(input));
    let arena = Bump::new();
    let target = Triple::host().into();
    let action = state.step(&arena, input, target, DEFAULT_PALETTE);
    let repl_helper = ReplHelper::default();
    let mut editor = Editor::<ReplHelper>::new();
    editor.set_helper(Some(repl_helper));

    match action {
        ReplAction::Eval { opt_mono, problems } => {
            let string = evaluate(opt_mono, problems, target);
            let escaped =
                std::string::String::from_utf8(strip_ansi_escapes::strip(string.trim()).unwrap())
                    .unwrap();

            let comment_index = escaped.rfind('#').unwrap_or(escaped.len());

            assert_eq!(expected_start, (escaped[0..comment_index].trim()));
        }
        _ => {
            panic!("Unexpected action: {:?}", action);
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
    let target = Triple::host().into();
    let action = state.step(&arena, input, target, DEFAULT_PALETTE);
    let repl_helper = ReplHelper::default();
    let mut editor = Editor::<ReplHelper>::new();
    editor.set_helper(Some(repl_helper));

    match action {
        ReplAction::Eval { opt_mono, problems } => {
            let string = evaluate(opt_mono, problems, target);
            let escaped =
                std::string::String::from_utf8(strip_ansi_escapes::strip(string.trim()).unwrap())
                    .unwrap();
            assert_eq!(expected_step_result, escaped);
        }
        _ => {
            panic!("Unexpected action: {:?}", action);
        }
    }
}
