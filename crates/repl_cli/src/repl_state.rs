use crate::cli_gen::gen_and_eval_llvm;
use crate::colors::{BLUE, END_COL, GREEN, PINK};
use bumpalo::Bump;
use const_format::concatcp;
use roc_collections::MutSet;
use roc_mono::ir::OptLevel;
use roc_parse::ast::{Expr, Pattern, TypeDef, TypeHeader, ValueDef};
use roc_parse::expr::{parse_single_def, ExprParseOptions, SingleDef};
use roc_parse::parser::Parser;
use roc_parse::parser::{EClosure, EExpr, EPattern};
use roc_parse::parser::{EWhen, Either};
use roc_parse::state::State;
use roc_parse::{join_alias_to_body, join_ann_to_body};
use roc_region::all::Loc;
use roc_repl_eval::gen::{Problems, ReplOutput};
use rustyline::highlight::{Highlighter, PromptInfo};
use rustyline::validate::{self, ValidationContext, ValidationResult, Validator};
use rustyline_derive::{Completer, Helper, Hinter};
use std::borrow::Cow;
use target_lexicon::Triple;

pub const PROMPT: &str = concatcp!(BLUE, "»", END_COL, " ");
pub const CONT_PROMPT: &str = concatcp!(BLUE, "…", END_COL, " ");

/// The prefix we use for the automatic variable names we assign to each expr,
/// e.g. if the prefix is "val" then the first expr you enter will be named "val1"
pub const AUTO_VAR_PREFIX: &str = "val";

// TODO add link to repl tutorial(does not yet exist).
pub const TIPS: &str = concatcp!(
    "\nEnter an expression to evaluate, or a definition (like ",
    BLUE,
    "x = 1",
    END_COL,
    ") to use in future expressions.\n\nUnless there was a compile-time error, expressions get automatically named so you can refer to them later.\nFor example, if you see ",
    GREEN,
    "# val1",
    END_COL,
    " after an output, you can now refer to that expression as ",
    BLUE,
    "val1",
    END_COL,
    " in future expressions.\n\nTips:\n\n",
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
    ":help"
);

#[derive(Debug, Clone, PartialEq)]
struct PastDef {
    ident: String,
    src: String,
}

#[derive(Completer, Helper, Hinter)]
pub struct ReplState {
    validator: InputValidator,
    past_defs: Vec<PastDef>,
    past_def_idents: MutSet<String>,
    last_auto_ident: u64,
}

impl Default for ReplState {
    fn default() -> Self {
        Self::new()
    }
}

impl ReplState {
    pub fn new() -> Self {
        Self {
            validator: InputValidator::new(),
            past_defs: Default::default(),
            past_def_idents: Default::default(),
            last_auto_ident: 0,
        }
    }

    pub fn step(&mut self, line: &str, dimensions: Option<(usize, usize)>) -> Result<String, i32> {
        let arena = Bump::new();

        match parse_src(&arena, line) {
            ParseOutcome::Empty => {
                if line.is_empty() {
                    Ok(TIPS.to_string())
                } else if line.ends_with('\n') {
                    // After two blank lines in a row, give up and try parsing it
                    // even though it's going to fail. This way you don't get stuck
                    // in a perpetual Incomplete state due to a syntax error.
                    Ok(self.eval_and_format(line, dimensions))
                } else {
                    // The previous line wasn't blank, but the line isn't empty either.
                    // This could mean that, for example, you're writing a multiline `when`
                    // and want to add a blank line. No problem! Print a blank line and
                    // continue waiting for input.
                    //
                    // If the user presses enter again, next time prev_line_blank() will be true
                    //  and we'll try parsing the source as-is.
                    Ok("\n".to_string())
                }
            }
            ParseOutcome::Expr(_)
            | ParseOutcome::ValueDef(_)
            | ParseOutcome::TypeDef(_)
            | ParseOutcome::SyntaxErr
            | ParseOutcome::Incomplete => Ok(self.eval_and_format(line, dimensions)),
            ParseOutcome::Help => {
                // TODO add link to repl tutorial(does not yet exist).
                Ok(TIPS.to_string())
            }
            ParseOutcome::Exit => Err(0),
        }
    }

    pub fn eval_and_format(&mut self, src: &str, dimensions: Option<(usize, usize)>) -> String {
        let arena = Bump::new();
        let pending_past_def;
        let mut opt_var_name;
        let src = match parse_src(&arena, src) {
            ParseOutcome::Expr(_) | ParseOutcome::Incomplete | ParseOutcome::SyntaxErr => {
                pending_past_def = None;
                // If it's a SyntaxErr (or Incomplete at this point, meaning it will
                // become a SyntaxErr as soon as we evaluate it),
                // proceed as normal and let the error reporting happen during eval.
                opt_var_name = None;

                src
            }
            ParseOutcome::ValueDef(value_def) => {
                match value_def {
                    ValueDef::Annotation(
                        Loc {
                            value: Pattern::Identifier(ident),
                            ..
                        },
                        _,
                    ) => {
                        // Record the standalone type annotation for future use.
                        self.add_past_def(ident.trim_end().to_string(), src.to_string());

                        // Return early without running eval, since standalone annotations
                        // cannnot be evaluated as expressions.
                        return String::new();
                    }
                    ValueDef::Body(
                        Loc {
                            value: Pattern::Identifier(ident),
                            ..
                        },
                        _,
                    )
                    | ValueDef::AnnotatedBody {
                        body_pattern:
                            Loc {
                                value: Pattern::Identifier(ident),
                                ..
                            },
                        ..
                    } => {
                        pending_past_def = Some((ident.to_string(), src.to_string()));
                        opt_var_name = Some(ident.to_string());

                        // Recreate the body of the def and then evaluate it as a lookup.
                        // We do this so that any errors will get reported as part of this expr;
                        // if we just did a lookup on the past def, then errors wouldn't get
                        // reported because we filter out errors whose regions are in past defs.
                        let mut buf = bumpalo::collections::string::String::with_capacity_in(
                            ident.len() + src.len() + 1,
                            &arena,
                        );

                        buf.push_str(src);
                        buf.push('\n');
                        buf.push_str(ident);

                        buf.into_bump_str()
                    }
                    ValueDef::Annotation(_, _)
                    | ValueDef::Body(_, _)
                    | ValueDef::AnnotatedBody { .. } => {
                        todo!("handle pattern other than identifier (which repl doesn't support)")
                    }
                    ValueDef::Dbg { .. } => {
                        todo!("handle receiving a `dbg` - what should the repl do for that?")
                    }
                    ValueDef::Expect { .. } => {
                        todo!("handle receiving an `expect` - what should the repl do for that?")
                    }
                    ValueDef::ExpectFx { .. } => {
                        todo!("handle receiving an `expect-fx` - what should the repl do for that?")
                    }
                }
            }
            ParseOutcome::TypeDef(TypeDef::Alias {
                header:
                    TypeHeader {
                        name: Loc { value: ident, .. },
                        ..
                    },
                ..
            })
            | ParseOutcome::TypeDef(TypeDef::Opaque {
                header:
                    TypeHeader {
                        name: Loc { value: ident, .. },
                        ..
                    },
                ..
            })
            | ParseOutcome::TypeDef(TypeDef::Ability {
                header:
                    TypeHeader {
                        name: Loc { value: ident, .. },
                        ..
                    },
                ..
            }) => {
                // Record the type for future use.
                self.add_past_def(ident.trim_end().to_string(), src.to_string());

                // Return early without running eval, since none of these
                // can be evaluated as expressions.
                return String::new();
            }
            ParseOutcome::Empty | ParseOutcome::Help | ParseOutcome::Exit => unreachable!(),
        };

        // Record e.g. "val1" as a past def, unless our input was exactly the name of
        // an existing identifer (e.g. I just typed "val1" into the prompt - there's no
        // need to reassign "val1" to "val2" just because I wanted to see what its value was!)
        let (output, problems) =
            match opt_var_name.or_else(|| self.past_def_idents.get(src.trim()).cloned()) {
                Some(existing_ident) => {
                    opt_var_name = Some(existing_ident);

                    gen_and_eval_llvm(
                        self.past_defs.iter().map(|def| def.src.as_str()),
                        src,
                        Triple::host(),
                        OptLevel::Normal,
                    )
                }
                None => {
                    let (output, problems) = gen_and_eval_llvm(
                        self.past_defs.iter().map(|def| def.src.as_str()),
                        src,
                        Triple::host(),
                        OptLevel::Normal,
                    );

                    // Don't persist defs that have compile errors
                    if problems.errors.is_empty() {
                        let var_name = format!("{AUTO_VAR_PREFIX}{}", self.next_auto_ident());
                        let src = format!("{var_name} = {}", src.trim_end());

                        opt_var_name = Some(var_name.clone());

                        self.add_past_def(var_name, src);
                    } else {
                        opt_var_name = None;
                    }

                    (output, problems)
                }
            };

        if let Some((ident, src)) = pending_past_def {
            self.add_past_def(ident, src);
        }

        format_output(output, problems, opt_var_name, dimensions)
    }

    fn next_auto_ident(&mut self) -> u64 {
        self.last_auto_ident += 1;
        self.last_auto_ident
    }

    fn add_past_def(&mut self, ident: String, src: String) {
        let existing_idents = &mut self.past_def_idents;

        existing_idents.insert(ident.clone());

        self.past_defs.push(PastDef { ident, src });
    }
}

#[derive(Debug, PartialEq)]
enum ParseOutcome<'a> {
    ValueDef(ValueDef<'a>),
    TypeDef(TypeDef<'a>),
    Expr(Expr<'a>),
    Incomplete,
    SyntaxErr,
    Empty,
    Help,
    Exit,
}

fn parse_src<'a>(arena: &'a Bump, line: &'a str) -> ParseOutcome<'a> {
    match line.trim().to_lowercase().as_str() {
        "" => ParseOutcome::Empty,
        ":help" => ParseOutcome::Help,
        ":exit" | ":quit" | ":q" => ParseOutcome::Exit,
        _ => {
            let src_bytes = line.as_bytes();

            match roc_parse::expr::loc_expr().parse(arena, State::new(src_bytes), 0) {
                Ok((_, loc_expr, _)) => ParseOutcome::Expr(loc_expr.value),
                // Special case some syntax errors to allow for multi-line inputs
                Err((_, EExpr::Closure(EClosure::Body(_, _), _)))
                | Err((_, EExpr::When(EWhen::Pattern(EPattern::Start(_), _), _)))
                | Err((_, EExpr::Start(_)))
                | Err((_, EExpr::IndentStart(_))) => ParseOutcome::Incomplete,
                Err((_, EExpr::DefMissingFinalExpr(_)))
                | Err((_, EExpr::DefMissingFinalExpr2(_, _))) => {
                    // This indicates that we had an attempted def; re-parse it as a single-line def.
                    match parse_single_def(
                        ExprParseOptions {
                            accept_multi_backpassing: true,
                            check_for_arrow: true,
                        },
                        0,
                        arena,
                        State::new(src_bytes),
                    ) {
                        Ok((
                            _,
                            Some(SingleDef {
                                type_or_value: Either::First(TypeDef::Alias { header, ann }),
                                ..
                            }),
                            state,
                        )) => {
                            // This *could* be an AnnotatedBody, e.g. in a case like this:
                            //
                            //   UserId x : [UserId Int]
                            //   UserId x = UserId 42
                            //
                            // We optimistically parsed the first line as an alias; we might now
                            // turn it into an annotation.
                            match parse_single_def(
                                ExprParseOptions {
                                    accept_multi_backpassing: true,
                                    check_for_arrow: true,
                                },
                                0,
                                arena,
                                state,
                            ) {
                                Ok((
                                    _,
                                    Some(SingleDef {
                                        type_or_value:
                                            Either::Second(ValueDef::Body(loc_pattern, loc_def_expr)),
                                        region,
                                        spaces_before,
                                    }),
                                    _,
                                )) if spaces_before.len() <= 1 => {
                                    // This was, in fact, an AnnotatedBody! Build and return it.
                                    let (value_def, _) = join_alias_to_body!(
                                        arena,
                                        loc_pattern,
                                        loc_def_expr,
                                        header,
                                        &ann,
                                        spaces_before,
                                        region
                                    );

                                    ParseOutcome::ValueDef(value_def)
                                }
                                _ => {
                                    // This was not an AnnotatedBody, so return the alias.
                                    ParseOutcome::TypeDef(TypeDef::Alias { header, ann })
                                }
                            }
                        }
                        Ok((
                            _,
                            Some(SingleDef {
                                type_or_value:
                                    Either::Second(ValueDef::Annotation(ann_pattern, ann_type)),
                                ..
                            }),
                            state,
                        )) => {
                            // This *could* be an AnnotatedBody, if the next line is a body.
                            match parse_single_def(
                                ExprParseOptions {
                                    accept_multi_backpassing: true,
                                    check_for_arrow: true,
                                },
                                0,
                                arena,
                                state,
                            ) {
                                Ok((
                                    _,
                                    Some(SingleDef {
                                        type_or_value:
                                            Either::Second(ValueDef::Body(loc_pattern, loc_def_expr)),
                                        region,
                                        spaces_before,
                                    }),
                                    _,
                                )) if spaces_before.len() <= 1 => {
                                    // Inlining this borrow makes clippy unhappy for some reason.
                                    let ann_pattern = &ann_pattern;

                                    // This was, in fact, an AnnotatedBody! Build and return it.
                                    let (value_def, _) = join_ann_to_body!(
                                        arena,
                                        loc_pattern,
                                        loc_def_expr,
                                        ann_pattern,
                                        &ann_type,
                                        spaces_before,
                                        region
                                    );

                                    ParseOutcome::ValueDef(value_def)
                                }
                                _ => {
                                    // This was not an AnnotatedBody, so return the standalone annotation.
                                    ParseOutcome::ValueDef(ValueDef::Annotation(
                                        ann_pattern,
                                        ann_type,
                                    ))
                                }
                            }
                        }
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
                        Err(_) => ParseOutcome::SyntaxErr,
                    }
                }
                Err(_) => ParseOutcome::SyntaxErr,
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
        if is_incomplete(ctx.input()) {
            Ok(ValidationResult::Incomplete)
        } else {
            Ok(ValidationResult::Valid(None))
        }
    }
}

pub fn is_incomplete(input: &str) -> bool {
    let arena = Bump::new();

    match parse_src(&arena, input) {
        ParseOutcome::Incomplete => !input.ends_with('\n'),
        // Standalone annotations are default incomplete, because we can't know
        // whether they're about to annotate a body on the next line
        // (or if not, meaning they stay standalone) until you press Enter again!
        //
        // So it's Incomplete until you've pressed Enter again (causing the input to end in "\n")
        ParseOutcome::ValueDef(ValueDef::Annotation(_, _)) if !input.ends_with('\n') => true,
        ParseOutcome::Expr(Expr::When(_, _)) => {
            // There might be lots of `when` branches, so don't assume the user is done entering
            // them until they enter a blank line!
            !input.ends_with('\n')
        }
        ParseOutcome::Empty
        | ParseOutcome::Help
        | ParseOutcome::Exit
        | ParseOutcome::ValueDef(_)
        | ParseOutcome::TypeDef(_)
        | ParseOutcome::SyntaxErr
        | ParseOutcome::Expr(_) => false,
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

fn format_output(
    opt_output: Option<ReplOutput>,
    problems: Problems,
    opt_var_name: Option<String>,
    dimensions: Option<(usize, usize)>,
) -> String {
    let mut buf = String::new();

    for message in problems.errors.iter().chain(problems.warnings.iter()) {
        if !buf.is_empty() {
            buf.push_str("\n\n");
        }

        buf.push('\n');
        buf.push_str(message);
        buf.push('\n');
    }

    if let Some(ReplOutput { expr, expr_type }) = opt_output {
        // If expr was empty, it was a type annotation or ability declaration;
        // don't print anything!
        //
        // Also, for now we also don't print anything if there was a compile-time error.
        // In the future, it would be great to run anyway and print useful output here!
        if !expr.is_empty() && problems.errors.is_empty() {
            const EXPR_TYPE_SEPARATOR: &str = " : "; // e.g. in "5 : Num *"

            // Print the expr and its type
            {
                buf.push('\n');
                buf.push_str(&expr);
                buf.push_str(PINK); // Color for the type separator
                buf.push_str(EXPR_TYPE_SEPARATOR);
                buf.push_str(END_COL);
                buf.push_str(&expr_type);
            }

            // Print var_name right-aligned on the last line of output.
            if let Some(var_name) = opt_var_name {
                use unicode_segmentation::UnicodeSegmentation;

                const VAR_NAME_PREFIX: &str = " # "; // e.g. in " # val1"
                const VAR_NAME_COLUMN_MAX: usize = 32; // Right-align the var_name at this column

                let term_width = match dimensions {
                    Some((width, _)) => width.min(VAR_NAME_COLUMN_MAX),
                    None => VAR_NAME_COLUMN_MAX as usize,
                };

                let expr_with_type = format!("{expr}{EXPR_TYPE_SEPARATOR}{expr_type}");

                // Count graphemes because we care about what's *rendered* in the terminal
                let last_line_len = expr_with_type
                    .split('\n')
                    .last()
                    .unwrap_or_default()
                    .graphemes(true)
                    .count();
                let var_name_len =
                    var_name.graphemes(true).count() + VAR_NAME_PREFIX.graphemes(true).count();
                let spaces_needed = if last_line_len + var_name_len > term_width {
                    buf.push('\n');
                    term_width - var_name_len
                } else {
                    term_width - last_line_len - var_name_len
                };

                for _ in 0..spaces_needed {
                    buf.push(' ');
                }

                buf.push_str(GREEN);
                buf.push_str(VAR_NAME_PREFIX);
                buf.push_str(&var_name);
                buf.push_str(END_COL);
                buf.push('\n');
            }
        }
    }

    buf
}
