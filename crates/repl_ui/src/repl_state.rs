use bumpalo::Bump;
use roc_collections::MutSet;
use roc_load::MonomorphizedModule;
use roc_parse::ast::{Expr, Pattern, TypeDef, TypeHeader, ValueDef};
use roc_parse::expr::{parse_single_def, ExprParseOptions, SingleDef};
use roc_parse::parser::Parser;
use roc_parse::parser::{EClosure, EExpr, EPattern};
use roc_parse::parser::{EWhen, Either};
use roc_parse::state::State;
use roc_parse::{join_alias_to_body, join_ann_to_body};
use roc_region::all::Loc;
use roc_repl_eval::gen::{compile_to_mono, Problems};
use roc_reporting::report::Palette;
use roc_target::TargetInfo;

/// The prefix we use for the automatic variable names we assign to each expr,
/// e.g. if the prefix is "val" then the first expr you enter will be named "val1"
pub const AUTO_VAR_PREFIX: &str = "val";
#[derive(Debug, Clone, PartialEq)]
struct PastDef {
    ident: String,
    src: String,
}

pub struct ReplState {
    past_defs: Vec<PastDef>,
    past_def_idents: MutSet<String>,
    last_auto_ident: u64,
}

impl Default for ReplState {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
#[allow(clippy::large_enum_variant)]
pub enum ReplAction<'a> {
    Eval {
        opt_mono: Option<MonomorphizedModule<'a>>,
        problems: Problems,
        opt_var_name: Option<String>,
    },
    Exit,
    Help,
    Nothing,
}

impl ReplState {
    pub fn new() -> Self {
        Self {
            past_defs: Default::default(),
            past_def_idents: Default::default(),
            last_auto_ident: 0,
        }
    }

    pub fn step<'a>(
        &mut self,
        arena: &'a Bump,
        line: &str,
        target_info: TargetInfo,
        palette: Palette,
    ) -> ReplAction<'a> {
        match parse_src(arena, line) {
            ParseOutcome::Empty | ParseOutcome::Help => ReplAction::Help,
            ParseOutcome::Expr(_)
            | ParseOutcome::ValueDef(_)
            | ParseOutcome::TypeDef(_)
            | ParseOutcome::SyntaxErr
            | ParseOutcome::Incomplete => self.next_action(arena, line, target_info, palette),
            ParseOutcome::Exit => ReplAction::Exit,
        }
    }

    fn next_action<'a>(
        &mut self,
        arena: &'a Bump,
        src: &str,
        target_info: TargetInfo,
        palette: Palette,
    ) -> ReplAction<'a> {
        let pending_past_def;
        let mut opt_var_name;
        let src = match parse_src(arena, src) {
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
                        // cannot be evaluated as expressions.
                        return ReplAction::Nothing;
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
                            arena,
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
                return ReplAction::Nothing;
            }
            ParseOutcome::Empty | ParseOutcome::Help | ParseOutcome::Exit => unreachable!(),
        };

        // Record e.g. "val1" as a past def, unless our input was exactly the name of
        // an existing identifer (e.g. I just typed "val1" into the prompt - there's no
        // need to reassign "val1" to "val2" just because I wanted to see what its value was!)
        let (opt_mono, problems) =
            match opt_var_name.or_else(|| self.past_def_idents.get(src.trim()).cloned()) {
                Some(existing_ident) => {
                    opt_var_name = Some(existing_ident);

                    compile_to_mono(
                        arena,
                        self.past_defs.iter().map(|def| def.src.as_str()),
                        src,
                        target_info,
                        palette,
                    )
                }
                None => {
                    let (output, problems) = compile_to_mono(
                        arena,
                        self.past_defs.iter().map(|def| def.src.as_str()),
                        src,
                        target_info,
                        palette,
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

        ReplAction::Eval {
            opt_mono,
            problems,
            opt_var_name,
        }
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
pub enum ParseOutcome<'a> {
    ValueDef(ValueDef<'a>),
    TypeDef(TypeDef<'a>),
    Expr(Expr<'a>),
    Incomplete,
    SyntaxErr,
    Empty,
    Help,
    Exit,
}

pub fn parse_src<'a>(arena: &'a Bump, line: &'a str) -> ParseOutcome<'a> {
    match line.trim().to_lowercase().as_str() {
        "" => ParseOutcome::Empty,
        ":help" => ParseOutcome::Help,
        ":exit" | ":quit" | ":q" => ParseOutcome::Exit,
        _ => {
            let src_bytes = line.as_bytes();

            match roc_parse::expr::loc_expr(true).parse(arena, State::new(src_bytes), 0) {
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
