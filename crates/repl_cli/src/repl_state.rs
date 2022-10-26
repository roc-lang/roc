use crate::colors::{BLUE, END_COL, PINK};
use bumpalo::Bump;
use const_format::concatcp;
use roc_parse::ast::ValueDef;
use roc_parse::expr::{parse_single_def, ExprParseOptions};
use roc_parse::parser::{EClosure, EExpr, Parser};
use roc_parse::parser::{Either, SyntaxError};
use roc_parse::state::State;
use roc_repl_eval::eval::jit_to_ast;
use roc_repl_eval::gen::{compile_to_mono, format_answer, ReplOutput};
use roc_reporting::report::DEFAULT_PALETTE;
use roc_types::pretty_print::{name_and_print_var, DebugPrint};
use rustyline::highlight::{Highlighter, PromptInfo};
use rustyline::validate::{self, ValidationContext, ValidationResult, Validator};
use rustyline_derive::{Completer, Helper, Hinter};
use std::borrow::Cow;
use std::collections::LinkedList;

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

pub struct PastDef {
    ident: String,
    src: String,
}

#[derive(Completer, Helper, Hinter)]
pub(crate) struct ReplState {
    validator: InputValidator,
    prev_line_blank: bool,
    pending_src: String,
    past_defs: LinkedList<PastDef>,
}

impl ReplState {
    pub fn new() -> Self {
        Self {
            validator: InputValidator::new(),
            prev_line_blank: false,
            pending_src: String::new(),
            past_defs: Default::default(),
        }
    }

    fn step(&mut self, trim_line: &str) -> Result<String, i32> {
        match trim_line.to_lowercase().as_str() {
            "" => {
                if self.pending_src.is_empty() {
                    self.prev_line_blank = false;

                    return Ok(format!("\n{}\n", TIPS));
                } else if self.prev_line_blank {
                    // After two blank lines in a row, give up and try parsing it
                    // even though it's going to fail. This way you don't get stuck.
                    let src = self.pending_src.clone();

                    self.pending_src.clear();

                    self.eval_and_format(&src).map_err(|_| {
                        // This seems to be unreachable in practice.
                        unreachable!();
                    })
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
            ":help" => {
                // TODO add link to repl tutorial(does not yet exist).
                Ok(format!("\n{}\n", TIPS))
            }
            ":exit" | ":quit" | ":q" => Err(0),
            _ => self.eval_and_format(trim_line).map_err(|fail| {
                todo!("gracefully report parse error in repl: {:?}", fail);
            }),
        }
    }

    pub fn eval_and_format<'a>(&mut self, src: &str) -> Result<String, SyntaxError<'a>> {
        let src = if self.pending_src.is_empty() {
            src
        } else {
            self.pending_src.push('\n');
            self.pending_src.push_str(src);

            self.pending_src.as_str()
        };

        dbg!(&src);

        // First, try to parse it as a Def. If that succeeds, record it in self and continue.
        let src = match parse_single_def(
            ExprParseOptions {
                accept_multi_backpassing: true,
                check_for_arrow: true,
            },
            0,
            &Bump::new(),
            State::new(src.as_bytes()),
        ) {
            Ok((_, Some(single_def), _)) => match single_def.type_or_value {
                Either::First(type_def) => {
                    // Alias, Opaque, or Ability
                    todo!("handle Alias, Opaque, or Ability")
                }
                Either::Second(value_def) => match value_def {
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

                        // Return without clearing pending_src.
                        return Ok(String::new());
                    }
                    ValueDef::Body(loc_pattern, loc_expr)
                    | ValueDef::AnnotatedBody {
                        body_pattern: loc_pattern,
                        body_expr: loc_expr,
                        ..
                    } => todo!("handle receiving a toplevel def of a value/function"),
                    ValueDef::Expect { .. } => {
                        todo!("handle receiving an `expect` - what should the repl do for that?")
                    }
                    ValueDef::ExpectFx { .. } => {
                        todo!("handle receiving an `expect-fx` - what should the repl do for that?")
                    }
                },
            },
            Ok(_) => src,
            Err((_, eexpr, _)) => {
                return Err(fail);
            }
        };

        let answer = gen_and_eval_llvm(
            src,
            Triple::host(),
            OptLevel::Normal,
            "TODOval1".to_string(),
        )
        .map(format_output);

        self.pending_src.clear();

        answer
    }

    /// Wrap the given expresssion in the appropriate past defs
    fn wrapped_expr_src(&self, src: &str) -> String {
        let mut buf = String::new();

        for past_def in self.past_defs.iter() {
            buf.push_str(past_def.src.as_str());
            buf.push('\n');
        }

        buf.push_str(src);

        buf
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
        if ctx.input().is_empty() {
            Ok(ValidationResult::Incomplete)
        } else {
            let arena = bumpalo::Bump::new();
            let state = roc_parse::state::State::new(ctx.input().trim().as_bytes());
            let answer = match roc_parse::expr::toplevel_defs(0).parse(&arena, state) {
                // Special case some syntax errors to allow for multi-line inputs
                Err((_, EExpr::DefMissingFinalExpr(_), _))
                | Err((_, EExpr::DefMissingFinalExpr2(_, _), _))
                | Err((_, EExpr::Closure(EClosure::Body(_, _), _), _)) => {
                    Ok(ValidationResult::Incomplete)
                }
                Err((_, _, state)) => {
                    // It wasn't a valid top-level decl, so continue parsing it as an expr.
                    match roc_parse::expr::parse_loc_expr(0, &arena, state) {
                        // Special case some syntax errors to allow for multi-line inputs
                        Err((_, EExpr::DefMissingFinalExpr(_), _))
                        | Err((_, EExpr::DefMissingFinalExpr2(_, _), _))
                        | Err((_, EExpr::Closure(EClosure::Body(_, _), _), _)) => {
                            Ok(ValidationResult::Incomplete)
                        }
                        _ => Ok(ValidationResult::Valid(None)),
                    }
                }
                Ok(_) => Ok(ValidationResult::Valid(None)),
            };

            // This is necessary to extend the lifetime of `arena`; without it,
            // we get a borrow checker error!
            answer
        }
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

fn gen_and_eval_llvm<'a>(
    src: &str,
    target: Triple,
    opt_level: OptLevel,
    val_name: String,
) -> Result<ReplOutput, SyntaxError<'a>> {
    let arena = Bump::new();
    let target_info = TargetInfo::from(&target);

    let mut loaded = match compile_to_mono(&arena, src, target_info, DEFAULT_PALETTE) {
        Ok(x) => x,
        Err(prob_strings) => {
            return Ok(ReplOutput::Problems(prob_strings));
        }
    };

    debug_assert_eq!(loaded.exposed_to_host.values.len(), 1);
    let (main_fn_symbol, main_fn_var) = loaded.exposed_to_host.values.iter().next().unwrap();
    let main_fn_symbol = *main_fn_symbol;
    let main_fn_var = *main_fn_var;

    // pretty-print the expr type string for later.
    let expr_type_str = name_and_print_var(
        main_fn_var,
        &mut loaded.subs,
        loaded.module_id,
        &loaded.interns,
        DebugPrint::NOTHING,
    );

    let (_, main_fn_layout) = match loaded.procedures.keys().find(|(s, _)| *s == main_fn_symbol) {
        Some(layout) => *layout,
        None => {
            return Ok(ReplOutput::NoProblems {
                expr: "<function>".to_string(),
                expr_type: expr_type_str,
                val_name,
            });
        }
    };

    let interns = loaded.interns.clone();

    let (lib, main_fn_name, subs, layout_interner) =
        mono_module_to_dylib(&arena, target, loaded, opt_level).expect("we produce a valid Dylib");

    let mut app = CliApp { lib };

    let res_answer = jit_to_ast(
        &arena,
        &mut app,
        main_fn_name,
        main_fn_layout,
        main_fn_var,
        &subs,
        &interns,
        layout_interner.into_global().fork(),
        target_info,
    );

    Ok(format_answer(&arena, res_answer, expr_type_str, val_name))
}
