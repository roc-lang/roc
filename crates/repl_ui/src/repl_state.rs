use std::path::PathBuf;
use std::{fs, io};

use bumpalo::Bump;
use roc_collections::MutSet;
use roc_load::MonomorphizedModule;
use roc_parse::ast::{Defs, Expr, Pattern, StrLiteral, TypeDef, TypeHeader, ValueDef};
use roc_parse::expr::parse_repl_defs_and_optional_expr;
use roc_parse::parser::EWhen;
use roc_parse::parser::{EClosure, EExpr, EPattern};
use roc_parse::state::State;
use roc_region::all::Loc;
use roc_repl_eval::gen::{compile_to_mono, Problems};
use roc_reporting::report::Palette;
use roc_target::Target;

#[derive(Debug, Clone, PartialEq)]
enum PastDef {
    Def { ident: String, src: String },
    Import(String),
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
    FileProblem {
        filename: PathBuf,
        error: io::ErrorKind,
    },
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
        target: Target,
        palette: Palette,
    ) -> ReplAction<'a> {
        let mut pending_past_def = None;
        let src: &str = match parse_src(arena, line) {
            ParseOutcome::Empty | ParseOutcome::Help => return ReplAction::Help,
            ParseOutcome::Exit => return ReplAction::Exit,
            ParseOutcome::Incomplete | ParseOutcome::SyntaxErr => {
                pending_past_def = None;

                // If it's a SyntaxErr (or Incomplete at this point, meaning it will
                // become a SyntaxErr as soon as we evaluate it),
                // proceed as normal and let the error reporting happen during eval.
                line
            }
            ParseOutcome::DefsAndExpr(_defs, Some(_expr)) => {
                // For now if we have a expr, we bundle everything together into a single
                // Defs expr, matching the older behavior of the parser. If we instead
                // use the branch below, it would trigger a bug further in the compiler.

                pending_past_def = None;
                line
            }
            ParseOutcome::DefsAndExpr(defs, None) => {
                let mut last_src = None;

                for def in defs.loc_defs() {
                    match def {
                        Ok(td) => {
                            match td.value {
                                TypeDef::Alias {
                                    header:
                                        TypeHeader {
                                            name: Loc { value: ident, .. },
                                            ..
                                        },
                                    ..
                                }
                                | TypeDef::Opaque {
                                    header:
                                        TypeHeader {
                                            name: Loc { value: ident, .. },
                                            ..
                                        },
                                    ..
                                }
                                | TypeDef::Ability {
                                    header:
                                        TypeHeader {
                                            name: Loc { value: ident, .. },
                                            ..
                                        },
                                    ..
                                } => {
                                    // Record the type for future use.
                                    self.add_past_def(
                                        ident.trim_end().to_string(),
                                        line[td.byte_range()].to_string(),
                                    );

                                    // Return early without running eval, since none of these
                                    // can be evaluated as expressions.
                                    return ReplAction::Nothing;
                                }
                            }
                        }
                        Err(vd) => {
                            match vd.value {
                                ValueDef::Annotation(
                                    Loc {
                                        // TODO is this right for suffixed
                                        value: Pattern::Identifier { ident },
                                        ..
                                    },
                                    _,
                                ) => {
                                    // Record the standalone type annotation for future use.
                                    self.add_past_def(
                                        ident.trim_end().to_string(),
                                        line[vd.byte_range()].to_string(),
                                    );

                                    // Return early without running eval, since standalone annotations
                                    // cannot be evaluated as expressions.
                                    return ReplAction::Nothing;
                                }
                                ValueDef::Body(
                                    Loc {
                                        // TODO is this right for suffixed
                                        value: Pattern::Identifier { ident },
                                        ..
                                    },
                                    _,
                                )
                                | ValueDef::AnnotatedBody {
                                    body_pattern:
                                        Loc {
                                            // TODO is this right for suffixed
                                            value: Pattern::Identifier { ident },
                                            ..
                                        },
                                    ..
                                } => {
                                    pending_past_def = Some((
                                        ident.to_string(),
                                        line[vd.byte_range()].to_string(),
                                    ));

                                    // Recreate the body of the def and then evaluate it as a lookup.
                                    // We do this so that any errors will get reported as part of this expr;
                                    // if we just did a lookup on the past def, then errors wouldn't get
                                    // reported because we filter out errors whose regions are in past defs.
                                    let mut buf =
                                        bumpalo::collections::string::String::with_capacity_in(
                                            ident.len() + line.len() + 1,
                                            arena,
                                        );

                                    buf.push_str(line);
                                    buf.push('\n');
                                    buf.push_str(ident);

                                    last_src = Some(buf.into_bump_str());
                                }
                                ValueDef::Annotation(_, _)
                                | ValueDef::Body(_, _)
                                | ValueDef::AnnotatedBody { .. } => {
                                    todo!("handle pattern other than identifier (which repl doesn't support).\
                                            \nTip: this error can be triggered when trying to define a variable with a character that is not allowed, \
                                            like starting with an uppercase character or using underdash (_).")
                                }
                                ValueDef::Dbg { .. } => {
                                    todo!("handle receiving a `dbg` - what should the repl do for that?")
                                }
                                ValueDef::Expect { .. } => {
                                    todo!("handle receiving an `expect` - what should the repl do for that?")
                                }
                                ValueDef::ModuleImport(import) => match import.name.value.package {
                                    Some(_) => {
                                        todo!("handle importing a module from a package")
                                    }
                                    None => {
                                        let mut filename = PathBuf::new();

                                        for part in import.name.value.name.parts() {
                                            filename.push(part);
                                        }

                                        filename.set_extension("roc");

                                        // Check we can read the file before we add it to past defs.
                                        // If we didn't do this, the bad import would remain in past_defs
                                        // and we'd report it on every subsequent evaluation.
                                        if let Err(err) = fs::metadata(&filename) {
                                            return ReplAction::FileProblem {
                                                filename,
                                                error: err.kind(),
                                            };
                                        }

                                        self.past_defs.push(PastDef::Import(
                                            line[vd.byte_range()].to_string(),
                                        ));

                                        return ReplAction::Nothing;
                                    }
                                },
                                ValueDef::IngestedFileImport(file) => {
                                    if let StrLiteral::PlainLine(path) = file.path.value {
                                        let filename = PathBuf::from(path);
                                        if let Err(err) = fs::metadata(&filename) {
                                            return ReplAction::FileProblem {
                                                filename,
                                                error: err.kind(),
                                            };
                                        }
                                    }

                                    self.past_defs
                                        .push(PastDef::Import(line[vd.byte_range()].to_string()));

                                    return ReplAction::Nothing;
                                }
                                ValueDef::Stmt(_) => todo!(),
                                ValueDef::StmtAfterExpr => todo!("effects in repl"),
                            }
                        }
                    }
                }

                if let Some(src) = last_src {
                    src
                } else {
                    return ReplAction::Nothing;
                }
            }
        };

        let (opt_mono, problems) = compile_to_mono(
            arena,
            self.past_defs.iter().map(|past_def| match past_def {
                PastDef::Def { ident: _, src } => src.as_str(),
                PastDef::Import(src) => src.as_str(),
            }),
            src,
            target,
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

        self.past_defs.push(PastDef::Def { ident, src });
    }
}

#[derive(Debug, PartialEq)]
pub enum ParseOutcome<'a> {
    DefsAndExpr(Defs<'a>, Option<Loc<Expr<'a>>>),
    Incomplete,
    SyntaxErr,
    Empty,
    Help,
    Exit,
}

/// Special case some syntax errors to allow for multi-line inputs
fn parse_outcome_for_error(e: EExpr<'_>) -> ParseOutcome<'_> {
    match e {
        EExpr::Closure(EClosure::Body(_, _), _)
        | EExpr::When(EWhen::Pattern(EPattern::Start(_), _), _)
        | EExpr::Record(_, _)
        | EExpr::Start(_)
        | EExpr::IndentStart(_) => ParseOutcome::Incomplete,
        _ => ParseOutcome::SyntaxErr,
    }
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

            match parse_repl_defs_and_optional_expr(arena, State::new(src_bytes)) {
                Err((_, e)) => parse_outcome_for_error(e),
                Ok((_, (defs, opt_last_expr), _state)) => {
                    if defs.is_empty() && opt_last_expr.is_none() {
                        ParseOutcome::Empty
                    } else {
                        ParseOutcome::DefsAndExpr(defs, opt_last_expr)
                    }
                }
            }
        }
    }
}
