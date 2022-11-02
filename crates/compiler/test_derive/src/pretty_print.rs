//! Pretty-prints the canonical AST back to check our work - do things look reasonable?

use roc_can::def::Def;
use roc_can::expr::Expr::{self, *};
use roc_can::expr::{ClosureData, OpaqueWrapFunctionData, WhenBranch};
use roc_can::pattern::{Pattern, RecordDestruct};

use roc_module::symbol::Interns;

use ven_pretty::{Arena, DocAllocator, DocBuilder};

pub struct Ctx<'a> {
    pub interns: &'a Interns,
}

pub fn pretty_print_def(c: &Ctx, d: &Def) -> String {
    let f = Arena::new();
    def(c, &f, d).append(f.hardline()).1.pretty(80).to_string()
}

macro_rules! maybe_paren {
    ($paren_if_above:expr, $my_prec:expr, $doc:expr) => {
        maybe_paren!($paren_if_above, $my_prec, || true, $doc)
    };
    ($paren_if_above:expr, $my_prec:expr, $extra_cond:expr, $doc:expr) => {
        if $my_prec > $paren_if_above && $extra_cond() {
            $doc.parens().group()
        } else {
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
    } = d;

    pattern(c, PPrec::Free, f, &loc_pattern.value)
        .append(f.text(" ="))
        .append(f.line())
        .append(expr(c, EPrec::Free, f, &loc_expr.value))
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
        Str(s) => f.text(format!(r#""{}""#, s)),
        SingleQuote(_, _, c, _) => f.text(format!("'{}'", c)),
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
        Var(sym, _) | AbilityMember(sym, _, _) => f.text(format!(
            "{}.{}",
            sym.module_string(c.interns),
            sym.as_str(c.interns),
        )),
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
            let (_, fun, _, _) = &**fun;
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
            .append(f.text(" ->"))
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
        EmptyRecord => f.text("{}"),
        Access {
            loc_expr, field, ..
        } => expr(c, AppArg, f, &loc_expr.value)
            .append(f.text(format!(".{}", field.as_str())))
            .group(),
        OpaqueWrapFunction(OpaqueWrapFunctionData { opaque_name, .. }) => {
            f.text(format!("@{}", opaque_name.as_str(c.interns)))
        }
        Accessor(_) => todo!(),
        Update {
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
        OpaqueRef { .. } => todo!(),
        Dbg { .. } => todo!(),
        Expect { .. } => todo!(),
        ExpectFx { .. } => todo!(),
        TypedHole(_) => todo!(),
        RuntimeError(_) => todo!(),
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
        } => f.text(format!(
            "{}.{}",
            sym.module_string(c.interns),
            sym.as_str(c.interns),
        )),
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
        } => f
            .text(format!("@{} ", opaque.module_string(c.interns)))
            .append(pattern(c, Free, f, &argument.1.value))
            .group(),
        RecordDestructure { destructs, .. } => f
            .text("{")
            .append(
                f.intersperse(
                    destructs.iter().map(|l| &l.value).map(
                        |RecordDestruct { label, typ, .. }| match typ {
                            roc_can::pattern::DestructType::Required => f.text(label.as_str()),
                            roc_can::pattern::DestructType::Optional(_, e) => f
                                .text(label.as_str())
                                .append(f.text(" ? "))
                                .append(expr(c, EPrec::Free, f, &e.value)),
                            roc_can::pattern::DestructType::Guard(_, p) => f
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
        List { .. } => todo!(),
        NumLiteral(_, n, _, _) | IntLiteral(_, _, n, _, _) | FloatLiteral(_, _, n, _, _) => {
            f.text(&**n)
        }
        StrLiteral(s) => f.text(format!(r#""{}""#, s)),
        SingleQuote(_, _, c, _) => f.text(format!("'{}'", c)),
        Underscore => f.text("_"),

        Shadowed(_, _, _) => todo!(),
        OpaqueNotInScope(_) => todo!(),
        UnsupportedPattern(_) => todo!(),
        MalformedPattern(_, _) => todo!(),
    }
}
