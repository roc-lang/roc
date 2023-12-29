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

#[derive(Debug, Clone, PartialEq)]
struct PastDef {
    ident: String,
    src: String,
}

pub struct ReplState {
    past_defs: Vec<PastDef>,
    past_def_idents: MutSet<String>,
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
        }
    }

    pub fn step<'a>(
        &mut self,
        arena: &'a Bump,
        line: &str,
        target_info: TargetInfo,
        palette: Palette,
    ) -> ReplAction<'a> {
        let pending_past_def;
        let src: &str = match parse_src(arena, line) {
            ParseOutcome::Empty | ParseOutcome::Help => return ReplAction::Help,
            ParseOutcome::Exit => return ReplAction::Exit,
            ParseOutcome::Expr(_) | ParseOutcome::Incomplete | ParseOutcome::SyntaxErr => {
                pending_past_def = None;

                // If it's a SyntaxErr (or Incomplete at this point, meaning it will
                // become a SyntaxErr as soon as we evaluate it),
                // proceed as normal and let the error reporting happen during eval.
                line
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
                        self.add_past_def(ident.trim_end().to_string(), line.to_string());

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
                        pending_past_def = Some((ident.to_string(), line.to_string()));

                        // Recreate the body of the def and then evaluate it as a lookup.
                        // We do this so that any errors will get reported as part of this expr;
                        // if we just did a lookup on the past def, then errors wouldn't get
                        // reported because we filter out errors whose regions are in past defs.
                        let mut buf = bumpalo::collections::string::String::with_capacity_in(
                            ident.len() + line.len() + 1,
                            arena,
                        );

                        buf.push_str(line);
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
                    ValueDef::ModuleImport(_) => {
                        todo!("handle importing a module from the REPL")
                    }
                    ValueDef::IngestedFileImport(_) => {
                        todo!("handle ingesting a file from the REPL")
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
                self.add_past_def(ident.trim_end().to_string(), line.to_string());

                // Return early without running eval, since none of these
                // can be evaluated as expressions.
                return ReplAction::Nothing;
            }
        };

        let (opt_mono, problems) = compile_to_mono(
            arena,
            self.past_defs.iter().map(|def| def.src.as_str()),
            src,
            target_info,
            palette,
        );

        if let Some((ident, src)) = pending_past_def {
            self.add_past_def(ident, src);
        }

        ReplAction::Eval { opt_mono, problems }
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
        // These are all common things beginners try.
        // Let people exit the repl easily!
        // If you really need to evaluate `exit` for some reason,
        // you can do `foo = exit` and then evaluate `foo` instead.
        ":exit" | ":quit" | ":q" | "exit" | "quit" | "exit()" | "quit()" => ParseOutcome::Exit,
        _ => {
            let src_bytes = line.as_bytes();

            match roc_parse::expr::loc_expr(true).parse(arena, State::new(src_bytes), 0) {
                Ok((_, loc_expr, _)) => ParseOutcome::Expr(loc_expr.value),
                // Special case some syntax errors to allow for multi-line inputs
                Err((_, EExpr::Closure(EClosure::Body(_, _), _)))
                | Err((_, EExpr::When(EWhen::Pattern(EPattern::Start(_), _), _)))
                | Err((_, EExpr::Record(_, _)))
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
