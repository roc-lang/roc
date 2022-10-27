use crate::cli_gen::gen_and_eval_llvm;
use crate::colors::{BLUE, END_COL, GREEN, PINK};
use bumpalo::Bump;
use const_format::concatcp;
use roc_mono::ir::OptLevel;
use roc_parse::ast::{Expr, TypeDef, ValueDef};
use roc_parse::expr::{parse_single_def, ExprParseOptions, SingleDef};
use roc_parse::parser::Either;
use roc_parse::parser::{EClosure, EExpr};
use roc_parse::state::State;
use roc_repl_eval::gen::ReplOutput;
use rustyline::highlight::{Highlighter, PromptInfo};
use rustyline::validate::{self, ValidationContext, ValidationResult, Validator};
use rustyline_derive::{Completer, Helper, Hinter};
use std::borrow::Cow;
use std::collections::LinkedList;
use target_lexicon::Triple;

pub const PROMPT: &str = concatcp!("\n", BLUE, "»", END_COL, " ");
pub const CONT_PROMPT: &str = concatcp!(BLUE, "…", END_COL, " ");

// TODO add link to repl tutorial(does not yet exist).
pub const TIPS: &str = concatcp!(
    BLUE,
    "  - ",
    END_COL,
    PINK,
    "ctrl-v",
    END_COL,
    " + ",
    PINK,
    "ctrl-j",
    END_COL,
    " makes a newline\n\n",
    BLUE,
    "  - ",
    END_COL,
    ":q to quit\n\n",
    BLUE,
    "  - ",
    END_COL,
    ":help\n"
);

struct PastDef {
    _ident: String,
    _src: String,
}

#[derive(Completer, Helper, Hinter)]
pub struct ReplState {
    pub prev_line_blank: bool,
    pub pending_src: String,
    validator: InputValidator,
    _past_defs: LinkedList<PastDef>,
}

impl ReplState {
    pub fn new() -> Self {
        Self {
            validator: InputValidator::new(),
            prev_line_blank: false,
            pending_src: String::new(),
            _past_defs: Default::default(),
        }
    }

    pub fn step(&mut self, trim_line: &str) -> Result<String, i32> {
        let arena = Bump::new();

        match parse_src(&arena, trim_line) {
            ParseOutcome::Empty => {
                if self.pending_src.is_empty() {
                    self.prev_line_blank = false;

                    return Ok(format!("\n{}\n", TIPS));
                } else if self.prev_line_blank {
                    // After two blank lines in a row, give up and try parsing it
                    // even though it's going to fail. This way you don't get stuck.
                    let src = self.pending_src.clone();

                    self.pending_src.clear();

                    Ok(self.eval_and_format(&src))
                } else {
                    // The previous line wasn't blank, but there's some pending source.
                    // This could mean that, for example, you're writing a multiline `when`
                    // and want to add a blank line. No problem! Print a blank line and
                    // continue waiting for input.
                    //
                    // If the user presses enter again, next time prev_line_blank will be true
                    //  and we'll try parsing the source as-is.
                    self.prev_line_blank = true;

                    Ok("\n".to_string())
                }
            }
            ParseOutcome::Expr(_)
            | ParseOutcome::ValueDef(_)
            | ParseOutcome::TypeDef(_)
            | ParseOutcome::Incomplete => Ok(self.eval_and_format(trim_line)),
            ParseOutcome::Help => {
                // TODO add link to repl tutorial(does not yet exist).
                Ok(format!("\n{}\n", TIPS))
            }
            ParseOutcome::Exit => Err(0),
        }
    }

    pub fn eval_and_format<'a>(&mut self, src: &str) -> String {
        let src = if self.pending_src.is_empty() {
            src
        } else {
            self.pending_src.push('\n');
            self.pending_src.push_str(src);

            self.pending_src.as_str()
        };

        let arena = Bump::new();

        let src = match parse_src(&arena, src) {
            ParseOutcome::Expr(_) => src,
            ParseOutcome::ValueDef(value_def) => {
                match value_def {
                    ValueDef::Annotation(_, _) => {
                        // Needed to avoid a borrow error.
                        let src = src.to_string();

                        dbg!(&src);

                        // We received a type annotation, like `x : Str`
                        //
                        // This might be the beginning of an AnnotatedBody, or it might be
                        // a standalone annotation. To find out, we need more input from the user.
                        self.pending_src.push_str(src.as_str());
                        self.pending_src.push('\n');

                        // Return without running eval or clearing pending_src.
                        return String::new();
                    }
                    ValueDef::Body(_loc_pattern, _loc_expr)
                    | ValueDef::AnnotatedBody {
                        body_pattern: _loc_pattern,
                        body_expr: _loc_expr,
                        ..
                    } => todo!("handle receiving a toplevel def of a value/function"),
                    ValueDef::Expect { .. } => {
                        todo!("handle receiving an `expect` - what should the repl do for that?")
                    }
                    ValueDef::ExpectFx { .. } => {
                        todo!("handle receiving an `expect-fx` - what should the repl do for that?")
                    }
                }
            }
            ParseOutcome::TypeDef(_) => {
                // Alias, Opaque, or Ability
                todo!("handle Alias, Opaque, or Ability")
            }
            ParseOutcome::Incomplete => todo!("handle Incomplete parse"),
            ParseOutcome::Empty | ParseOutcome::Help | ParseOutcome::Exit => unreachable!(),
        };

        let output = format_output(gen_and_eval_llvm(
            src,
            Triple::host(),
            OptLevel::Normal,
            "TODOval1".to_string(),
        ));

        self.pending_src.clear();

        output
    }

    /// Wrap the given expresssion in the appropriate past defs
    fn _wrapped_expr_src(&self, src: &str) -> String {
        let mut buf = String::new();

        for past_def in self._past_defs.iter() {
            buf.push_str(past_def._src.as_str());
            buf.push('\n');
        }

        buf.push_str(src);

        buf
    }
}

enum ParseOutcome<'a> {
    ValueDef(ValueDef<'a>),
    TypeDef(TypeDef<'a>),
    Expr(Expr<'a>),
    Incomplete,
    Empty,
    Help,
    Exit,
}

fn parse_src<'a>(arena: &'a Bump, line: &'a str) -> ParseOutcome<'a> {
    let trim_line = line.trim();

    match trim_line.to_lowercase().as_str() {
        "" => ParseOutcome::Empty,
        ":help" => ParseOutcome::Help,
        ":exit" | ":quit" | ":q" => ParseOutcome::Exit,
        _ => {
            let src_bytes = trim_line.as_bytes();

            match roc_parse::expr::parse_loc_expr(0, &arena, State::new(src_bytes)) {
                Ok((_, loc_expr, _)) => ParseOutcome::Expr(loc_expr.value),
                // Special case some syntax errors to allow for multi-line inputs
                Err((_, EExpr::Closure(EClosure::Body(_, _), _), _)) => ParseOutcome::Incomplete,
                Err((_, EExpr::DefMissingFinalExpr(_), _))
                | Err((_, EExpr::DefMissingFinalExpr2(_, _), _)) => {
                    // This indicates that we had an attempted def; re-parse it as a single-line def.
                    match parse_single_def(
                        ExprParseOptions {
                            accept_multi_backpassing: true,
                            check_for_arrow: true,
                        },
                        0,
                        &arena,
                        State::new(src_bytes),
                    ) {
                        Ok((
                            _,
                            Some(SingleDef {
                                type_or_value: Either::First(type_def),
                                ..
                            }),
                            _,
                        )) => ParseOutcome::TypeDef(type_def),
                        Ok((
                            _,
                            Some(SingleDef {
                                type_or_value: Either::Second(value_def),
                                ..
                            }),
                            _,
                        )) => ParseOutcome::ValueDef(value_def),
                        Ok((_, None, _)) => {
                            todo!("TODO determine appropriate ParseOutcome for Ok(None)")
                        }
                        Err(_) => ParseOutcome::Incomplete,
                    }
                }
                Err(_) => ParseOutcome::Incomplete,
            }
        }
    }
}

struct InputValidator {}

impl InputValidator {
    pub fn new() -> InputValidator {
        InputValidator {}
    }
}

impl Validator for InputValidator {
    fn validate(&self, ctx: &mut ValidationContext) -> rustyline::Result<ValidationResult> {
        validate(ctx.input())
    }
}

pub fn validate(input: &str) -> rustyline::Result<ValidationResult> {
    let arena = Bump::new();

    match parse_src(&arena, input) {
        // Standalone annotations are default incomplete, because we can't know
        // whether they're about to annotate a body on the next line
        // (or if not, meaning they stay standalone) until you press Enter again!
        ParseOutcome::ValueDef(ValueDef::Annotation(_, _))
        | ParseOutcome::Empty
        | ParseOutcome::Incomplete => Ok(ValidationResult::Incomplete),
        ParseOutcome::Help
        | ParseOutcome::Exit
        | ParseOutcome::ValueDef(_)
        | ParseOutcome::TypeDef(_)
        | ParseOutcome::Expr(_) => Ok(ValidationResult::Valid(None)),
    }
}

impl Highlighter for ReplState {
    fn has_continuation_prompt(&self) -> bool {
        true
    }

    fn highlight_prompt<'b, 's: 'b, 'p: 'b>(
        &'s self,
        prompt: &'p str,
        info: PromptInfo<'_>,
    ) -> Cow<'b, str> {
        if info.line_no() > 0 {
            CONT_PROMPT.into()
        } else {
            prompt.into()
        }
    }
}

impl Validator for ReplState {
    fn validate(
        &self,
        ctx: &mut validate::ValidationContext,
    ) -> rustyline::Result<validate::ValidationResult> {
        self.validator.validate(ctx)
    }

    fn validate_while_typing(&self) -> bool {
        self.validator.validate_while_typing()
    }
}

fn format_output(output: ReplOutput) -> String {
    match output {
        ReplOutput::NoProblems {
            expr,
            expr_type,
            val_name,
        } => {
            if expr.is_empty() {
                // This means it was a type annotation or ability declaration;
                // don't print anything!
                String::new()
            } else {
                format!("\n{expr} {PINK}:{END_COL} {expr_type}  {GREEN} # {val_name}")
            }
        }
        ReplOutput::Problems(lines) => format!("\n{}\n", lines.join("\n\n")),
    }
}
