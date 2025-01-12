//! Pretty-prints the canonical AST back to check our work - do things look reasonable?

use crate::def::{Def, DefKind};
use crate::expr::Expr::{self, *};
use crate::expr::{
    ClosureData, DeclarationTag, Declarations, FunctionDef, OpaqueWrapFunctionData,
    StructAccessorData, WhenBranch,
};
use crate::pattern::{Pattern, RecordDestruct, TupleDestruct};

use roc_module::symbol::{Interns, ModuleId, Symbol};

use roc_types::types::IndexOrField;
use ven_pretty::{text, Arena, DocAllocator, DocBuilder};

pub struct Ctx<'a> {
    pub home: ModuleId,
    pub interns: &'a Interns,
    pub print_lambda_names: bool,
}

pub fn pretty_write_declarations(
    writer: &mut impl std::io::Write,
    c: &Ctx,
    declarations: &Declarations,
) -> std::io::Result<()> {
    let f = Arena::new();
    print_declarations_help(c, &f, declarations)
        .1
        .render(80, writer)
}

pub fn pretty_print_def(c: &Ctx, d: &Def) -> String {
    let f = Arena::new();
    def(c, &f, d).append(f.hardline()).1.pretty(80).to_string()
}

fn print_declarations_help<'a>(
    c: &Ctx,
    f: &'a Arena<'a>,
    declarations: &'a Declarations,
) -> DocBuilder<'a, Arena<'a>> {
    let mut defs = Vec::with_capacity(declarations.len());
    for (index, tag) in declarations.iter_bottom_up() {
        let symbol = declarations.symbols[index].value;
        let body = &declarations.expressions[index];

        let def = match tag {
            DeclarationTag::Value => def_symbol_help(c, f, symbol, &body.value),
            DeclarationTag::Function(f_index)
            | DeclarationTag::Recursive(f_index)
            | DeclarationTag::TailRecursive(f_index) => {
                let function_def = &declarations.function_bodies[f_index.index()].value;
                toplevel_function(c, f, symbol, function_def, &body.value)
            }
            DeclarationTag::Expectation => todo!(),
            DeclarationTag::Destructure(_) => todo!(),
            DeclarationTag::MutualRecursion { .. } => {
                // the defs will be printed next
                continue;
            }
        };

        defs.push(def);
    }

    f.intersperse(defs, f.hardline().append(f.hardline()))
}

fn always_true() -> bool {
    true
}

macro_rules! maybe_paren {
    ($paren_if_above:expr, $my_prec:expr, $doc:expr) => {
        maybe_paren!($paren_if_above, $my_prec, always_true, $doc)
    };
    ($paren_if_above:expr, $my_prec:expr, $extra_cond:expr, $doc:expr) => {
        'blk: {
            if $my_prec > $paren_if_above {
                #[allow(clippy::redundant_closure_call)]
                let extra_cond = $extra_cond();

                if extra_cond {
                    break 'blk $doc.parens().group();
                }
            }

            $doc
        }
    };
}

fn def<'a>(c: &Ctx, f: &'a Arena<'a>, d: &'a Def) -> DocBuilder<'a, Arena<'a>> {
    let Def {
        loc_pattern,
        loc_expr,
        expr_var: _,
        pattern_vars: _,
        annotation: _,
        kind,
    } = d;

    match kind {
        DefKind::Let => def_help(c, f, &loc_pattern.value, &loc_expr.value),
        DefKind::Ignored(_) => def_help(c, f, &loc_pattern.value, &loc_expr.value),
        DefKind::Stmt(_) => expr(c, EPrec::Free, f, &loc_expr.value),
    }
}

fn def_symbol_help<'a>(
    c: &Ctx,
    f: &'a Arena<'a>,
    sym: Symbol,
    body: &'a Expr,
) -> DocBuilder<'a, Arena<'a>> {
    pp_sym(c, f, sym)
        .append(f.text(" ="))
        .append(f.line())
        .append(expr(c, EPrec::Free, f, body))
        .nest(2)
        .group()
}

fn def_help<'a>(
    c: &Ctx,
    f: &'a Arena<'a>,
    pat: &'a Pattern,
    body: &'a Expr,
) -> DocBuilder<'a, Arena<'a>> {
    pattern(c, PPrec::Free, f, pat)
        .append(f.text(" ="))
        .append(f.line())
        .append(expr(c, EPrec::Free, f, body))
        .nest(2)
        .group()
}

fn toplevel_function<'a>(
    c: &Ctx,
    f: &'a Arena<'a>,
    sym: Symbol,
    function_def: &'a FunctionDef,
    body: &'a Expr,
) -> DocBuilder<'a, Arena<'a>> {
    let FunctionDef { arguments, .. } = function_def;

    let args = arguments
        .iter()
        .map(|arg| pattern(c, PPrec::Free, f, &arg.2.value));

    pp_sym(c, f, sym)
        .append(f.text(" ="))
        .append(f.line())
        .append(f.text("\\"))
        .append(f.intersperse(args, f.text(", ")))
        .append(f.text(" ->"))
        .group()
        .append(f.line())
        .append(expr(c, EPrec::Free, f, body).group())
        .nest(2)
        .group()
}

#[derive(PartialEq, PartialOrd)]
enum EPrec {
    Free,
    AppArg,
}

fn expr<'a>(c: &Ctx, p: EPrec, f: &'a Arena<'a>, e: &'a Expr) -> DocBuilder<'a, Arena<'a>> {
    use EPrec::*;
    match e {
        Num(_, n, _, _) | Int(_, _, n, _, _) | Float(_, _, n, _, _) => f.text(&**n),
        Str(s) => text!(f, r#""{}""#, s),
        SingleQuote(_, _, c, _) => text!(f, "'{}'", c),
        IngestedFile(file_path, bytes, _) => {
            text!(f, "<ingested {:?}, {} bytes>", file_path, bytes.len())
        }
        List {
            elem_var: _,
            loc_elems,
        } => f
            .reflow("[")
            .append(
                f.concat(loc_elems.iter().map(|le| {
                    f.hardline()
                        .append(expr(c, Free, f, &le.value))
                        .append(f.text(","))
                }))
                .group()
                .nest(2),
            )
            .append(f.line_())
            .append("]")
            .group()
            .flat_alt(
                f.reflow("[")
                    .append(f.intersperse(
                        loc_elems.iter().map(|le| expr(c, Free, f, &le.value)),
                        f.reflow(", "),
                    ))
                    .append(f.line_())
                    .append("]")
                    .group(),
            ),
        Var(sym, _) | ParamsVar { symbol: sym, .. } | AbilityMember(sym, _, _) => {
            pp_sym(c, f, *sym)
        }
        ImportParams(_, _, Some((_, params_expr))) => expr(c, p, f, params_expr),
        ImportParams(module_id, _, None) => {
            text!(f, "<no params for {:?}>", module_id)
        }
        When {
            loc_cond, branches, ..
        } => maybe_paren!(
            Free,
            p,
            f.reflow("when ")
                .append(expr(c, Free, f, &loc_cond.value))
                .append(f.text(" is"))
                .append(
                    f.concat(
                        branches
                            .iter()
                            .map(|b| f.hardline().append(branch(c, f, b)))
                    )
                    .group(),
                )
                .nest(2)
                .group()
        ),
        If {
            branches,
            final_else,
            ..
        } => f
            .concat(branches.iter().enumerate().map(|(i, (cond, body))| {
                let head = if i == 0 { "if " } else { "else if " };
                (f.reflow(head)
                    .append(expr(c, Free, f, &cond.value))
                    .group()
                    .nest(2))
                .append(f.line())
                .append(
                    f.reflow("then")
                        .append(f.softline().append(expr(c, Free, f, &body.value)))
                        .group()
                        .nest(2),
                )
                .append(f.line())
            }))
            .append(
                f.reflow("else ")
                    .append(expr(c, Free, f, &final_else.value))
                    .group()
                    .nest(2),
            )
            .group(),
        LetRec(_, _, _) => todo!(),
        LetNonRec(loc_def, body) => def(c, f, loc_def)
            .append(f.hardline())
            .append(expr(c, Free, f, &body.value))
            .group(),
        Call(fun, args, _) => {
            let (_, fun, _, _, _) = &**fun;
            maybe_paren!(
                Free,
                p,
                expr(c, AppArg, f, &fun.value)
                    .append(
                        f.concat(args.iter().map(|le| f.line().append(expr(
                            c,
                            AppArg,
                            f,
                            &le.1.value
                        ))))
                        .group()
                    )
                    .group()
                    .nest(2)
            )
        }
        RunLowLevel { args, .. } => {
            let op = "LowLevel";

            maybe_paren!(
                Free,
                p,
                f.reflow(op)
                    .append(
                        f.concat(
                            args.iter()
                                .map(|le| f.line().append(expr(c, AppArg, f, &le.1)))
                        )
                        .group()
                    )
                    .group()
                    .nest(2)
            )
        }
        ForeignCall { .. } => todo!(),
        Closure(ClosureData {
            arguments,
            loc_body,
            name,
            ..
        }) => f
            .text("\\")
            .append(
                f.intersperse(
                    arguments
                        .iter()
                        .map(|(_, _, arg)| pattern(c, PPrec::Free, f, &arg.value)),
                    f.text(", "),
                ),
            )
            .append(if c.print_lambda_names {
                f.text(" -[")
                    .append(pp_sym(c, f, *name))
                    .append(f.text("]->"))
            } else {
                f.text(" ->")
            })
            .append(f.line())
            .append(expr(c, Free, f, &loc_body.value))
            .nest(2)
            .group(),
        Record { fields, .. } => f
            .reflow("{")
            .append(
                f.intersperse(
                    fields.iter().map(|(name, field)| {
                        let field = f
                            .text(name.as_str())
                            .append(f.reflow(": "))
                            .append(expr(c, Free, f, &field.loc_expr.value))
                            .nest(2)
                            .group();
                        f.line().append(field)
                    }),
                    f.reflow(","),
                )
                .nest(2)
                .group(),
            )
            .append(f.line())
            .append(f.text("}"))
            .group(),
        Tuple { elems, .. } => f
            .reflow("(")
            .append(
                f.intersperse(
                    elems.iter().map(|(_var, elem)| {
                        f.line()
                            .append(expr(c, Free, f, &elem.value))
                            .nest(2)
                            .group()
                    }),
                    f.reflow(","),
                )
                .nest(2)
                .group(),
            )
            .append(f.line())
            .append(f.text(")"))
            .group(),
        EmptyRecord => f.text("{}"),
        EmptyTuple => f.text("()"),
        RecordAccess {
            loc_expr, field, ..
        } => expr(c, AppArg, f, &loc_expr.value)
            .append(text!(f, ".{}", field.as_str()))
            .group(),
        TupleAccess {
            loc_expr, index, ..
        } => expr(c, AppArg, f, &loc_expr.value)
            .append(text!(f, ".{index}"))
            .group(),
        OpaqueWrapFunction(OpaqueWrapFunctionData { opaque_name, .. }) => {
            text!(f, "@{}", opaque_name.as_str(c.interns))
        }
        RecordAccessor(StructAccessorData { field, .. }) => match field {
            IndexOrField::Index(index) => text!(f, ".{}", index),
            IndexOrField::Field(name) => text!(f, ".{}", name),
        },
        RecordUpdate {
            symbol, updates, ..
        } => f
            .reflow("{")
            .append(f.line())
            .append(f.text(symbol.as_str(c.interns).to_string()))
            .append(f.reflow(" &"))
            .append(
                f.intersperse(
                    updates.iter().map(|(name, field)| {
                        let field = f
                            .text(name.as_str())
                            .append(f.reflow(": "))
                            .append(expr(c, Free, f, &field.loc_expr.value))
                            .nest(2)
                            .group();
                        f.line().append(field)
                    }),
                    f.reflow(","),
                )
                .nest(2)
                .group(),
            )
            .append(f.line())
            .append(f.text("}"))
            .group(),
        Tag {
            name, arguments, ..
        } => maybe_paren!(
            Free,
            p,
            || !arguments.is_empty(),
            f.text(name.0.as_str())
                .append(if arguments.is_empty() {
                    f.nil()
                } else {
                    f.space()
                })
                .append(
                    f.intersperse(
                        arguments
                            .iter()
                            .map(|(_, le)| expr(c, AppArg, f, &le.value)),
                        f.space(),
                    )
                )
                .group()
        ),
        Crash { .. } => todo!(),
        ZeroArgumentTag { .. } => todo!(),
        OpaqueRef { name, argument, .. } => maybe_paren!(
            Free,
            p,
            || true,
            pp_sym(c, f, *name)
                .append(f.space())
                .append(expr(c, AppArg, f, &argument.1.value))
                .group()
        ),
        Dbg { .. } => todo!(),
        Expect { .. } => todo!(),
        Try { .. } => todo!(),
        Return { .. } => todo!(),
        RuntimeError(_) => todo!(),
    }
}

fn pp_sym<'a>(c: &Ctx, f: &'a Arena<'a>, sym: Symbol) -> DocBuilder<'a, Arena<'a>> {
    if sym.module_id() == c.home {
        f.text(sym.as_str(c.interns).to_owned())
    } else {
        text!(
            f,
            "{}.{}",
            sym.module_string(c.interns),
            sym.as_str(c.interns),
        )
    }
}

fn branch<'a>(c: &Ctx, f: &'a Arena<'a>, b: &'a WhenBranch) -> DocBuilder<'a, Arena<'a>> {
    let WhenBranch {
        patterns,
        value,
        guard,
        redundant: _,
    } = b;

    f.intersperse(
        patterns
            .iter()
            .map(|lp| pattern(c, PPrec::Free, f, &lp.pattern.value)),
        f.text(" | "),
    )
    .append(match guard {
        Some(e) => f.text("if ").append(expr(c, EPrec::Free, f, &e.value)),
        None => f.nil(),
    })
    .append(f.text(" ->"))
    .append(f.line())
    .append(expr(c, EPrec::Free, f, &value.value))
    .nest(2)
    .group()
}

#[derive(PartialEq, PartialOrd)]
enum PPrec {
    Free,
    AppArg,
}

fn pattern<'a>(
    c: &Ctx,
    prec: PPrec,
    f: &'a Arena<'a>,
    p: &'a Pattern,
) -> DocBuilder<'a, Arena<'a>> {
    use PPrec::*;
    use Pattern::*;
    match p {
        Identifier(sym)
        | AbilityMemberSpecialization {
            specializes: sym, ..
        } => pp_sym(c, f, *sym),
        As(subpattern, symbol) => pattern(c, prec, f, &subpattern.value)
            .append(f.text(" as "))
            .append(pp_sym(c, f, *symbol)),
        AppliedTag {
            tag_name,
            arguments,
            ..
        } => maybe_paren!(
            Free,
            prec,
            f.text(tag_name.0.as_str())
                .append(if arguments.is_empty() {
                    f.nil()
                } else {
                    f.space()
                })
                .append(
                    f.intersperse(
                        arguments
                            .iter()
                            .map(|(_, lp)| pattern(c, AppArg, f, &lp.value)),
                        f.space(),
                    )
                )
                .group()
        ),
        UnwrappedOpaque {
            opaque, argument, ..
        } => text!(f, "@{} ", opaque.module_string(c.interns))
            .append(pattern(c, Free, f, &argument.1.value))
            .group(),
        RecordDestructure { destructs, .. } => f
            .text("{")
            .append(
                f.intersperse(
                    destructs.iter().map(|l| &l.value).map(
                        |RecordDestruct { label, typ, .. }| match typ {
                            crate::pattern::DestructType::Required => f.text(label.as_str()),
                            crate::pattern::DestructType::Optional(_, e) => f
                                .text(label.as_str())
                                .append(f.text(" ? "))
                                .append(expr(c, EPrec::Free, f, &e.value)),
                            crate::pattern::DestructType::Guard(_, p) => f
                                .text(label.as_str())
                                .append(f.text(": "))
                                .append(pattern(c, Free, f, &p.value)),
                        },
                    ),
                    f.text(", "),
                ),
            )
            .append(f.text("}"))
            .group(),
        TupleDestructure { destructs, .. } => f
            .text("(")
            .append(
                f.intersperse(
                    destructs
                        .iter()
                        .map(|l| &l.value)
                        .map(|TupleDestruct { typ: (_, p), .. }| pattern(c, Free, f, &p.value)),
                    f.text(", "),
                ),
            )
            .append(f.text(")"))
            .group(),
        List { .. } => todo!(),
        NumLiteral(_, n, _, _) | IntLiteral(_, _, n, _, _) | FloatLiteral(_, _, n, _, _) => {
            f.text(&**n)
        }
        StrLiteral(s) => text!(f, r#""{}""#, s),
        SingleQuote(_, _, c, _) => text!(f, "'{}'", c),
        Underscore => f.text("_"),

        Shadowed(_, _, _) => todo!(),
        OpaqueNotInScope(_) => todo!(),
        UnsupportedPattern(_) => todo!(),
        MalformedPattern(_, _) => todo!(),
    }
}
