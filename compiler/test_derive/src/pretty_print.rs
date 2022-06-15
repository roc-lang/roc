//! Pretty-prints the canonical AST back to check our work - do things look reasonable?

use roc_can::expr::Expr::{self, *};
use roc_can::expr::{ClosureData, WhenBranch};
use roc_can::pattern::{Pattern, RecordDestruct};

use roc_module::symbol::Interns;
use ven_pretty::{Arena, DocAllocator, DocBuilder};

pub struct Ctx<'a> {
    pub interns: &'a Interns,
}

pub fn pretty_print(c: &Ctx, e: &Expr) -> String {
    let f = Arena::new();
    expr(c, &f, e).append(f.hardline()).1.pretty(80).to_string()
}

fn expr<'a>(c: &Ctx, f: &'a Arena<'a>, e: &'a Expr) -> DocBuilder<'a, Arena<'a>> {
    match e {
        Num(_, n, _, _) | Int(_, _, n, _, _) | Float(_, _, n, _, _) => f.text(&**n),
        Str(s) => f.text(format!(r#""{}""#, s)),
        SingleQuote(c) => f.text(format!("'{}'", c)),
        List {
            elem_var: _,
            loc_elems,
        } => f
            .reflow("[")
            .append(
                f.concat(loc_elems.iter().map(|le| {
                    let elem = expr(c, f, &le.value);
                    f.line().append(elem).append(",")
                }))
                .group()
                .nest(2),
            )
            .append(f.line())
            .append("]")
            .group(),
        Var(sym) | AbilityMember(sym, _, _) => f.text(format!(
            "{}.{}",
            sym.module_string(c.interns),
            sym.as_str(c.interns),
        )),
        When {
            loc_cond, branches, ..
        } => f
            .reflow("when ")
            .append(expr(c, f, &loc_cond.value))
            .append(f.text(" is"))
            .append(
                f.concat(branches.iter().map(|b| f.line().append(branch(c, f, b))))
                    .nest(2)
                    .group(),
            )
            .nest(2)
            .group(),
        If {
            branches,
            final_else,
            ..
        } => f
            .concat(branches.iter().enumerate().map(|(i, (cond, body))| {
                let head = if i == 0 { "if " } else { "else if " };
                (f.reflow(head)
                    .append(expr(c, f, &cond.value))
                    .group()
                    .nest(2))
                .append(f.line())
                .append(
                    f.reflow("then")
                        .append(f.softline().append(expr(c, f, &body.value)))
                        .group()
                        .nest(2),
                )
                .append(f.line())
            }))
            .append(
                f.reflow("else ")
                    .append(expr(c, f, &final_else.value))
                    .group()
                    .nest(2),
            )
            .group(),
        LetRec(_, _, _) => todo!(),
        LetNonRec(_, _) => todo!(),
        Call(fun, args, _) => {
            let (_, fun, _, _) = &**fun;
            f.text("(")
                .append(expr(c, f, &fun.value))
                .append(f.softline())
                .append(f.intersperse(args.iter().map(|le| expr(c, f, &le.1.value)), f.softline()))
                .append(f.text(")"))
                .group()
        }
        RunLowLevel { .. } => todo!(),
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
                        .map(|(_, _, arg)| pattern(c, f, &arg.value)),
                    f.text(", "),
                ),
            )
            .append(f.text(" ->"))
            .append(f.line())
            .append(expr(c, f, &loc_body.value))
            .nest(2)
            .group(),
        Record { fields, .. } => f
            .reflow("{")
            .append(
                f.concat(fields.iter().map(|(name, field)| {
                    let field = f
                        .text(name.as_str())
                        .append(f.reflow(": "))
                        .append(expr(c, f, &field.loc_expr.value))
                        .nest(2)
                        .group();
                    f.line().append(field).append(",")
                }))
                .nest(2)
                .group(),
            )
            .append(f.line())
            .append(f.text("}"))
            .group(),
        EmptyRecord => f.text("{}"),
        Access {
            loc_expr, field, ..
        } => f
            .text("(")
            .append(expr(c, f, &loc_expr.value))
            .append(f.text(")"))
            .append(f.text(format!(".{}", field.as_str())))
            .group(),
        Accessor(_) => todo!(),
        Update { .. } => todo!(),
        Tag { .. } => todo!(),
        ZeroArgumentTag { .. } => todo!(),
        OpaqueRef { .. } => todo!(),
        Expect { .. } => todo!(),
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
        patterns.iter().map(|lp| pattern(c, f, &lp.value)),
        f.text(" | "),
    )
    .append(match guard {
        Some(e) => f.text("if ").append(expr(c, f, &e.value)),
        None => f.nil(),
    })
    .append(f.text(" ->"))
    .append(f.softline())
    .append(expr(c, f, &value.value))
    .nest(2)
    .group()
}

fn pattern<'a>(c: &Ctx, f: &'a Arena<'a>, p: &'a Pattern) -> DocBuilder<'a, Arena<'a>> {
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
        } => f
            .text(format!("({}", tag_name.0.as_str()))
            .append(f.intersperse(
                arguments.iter().map(|(_, lp)| pattern(c, f, &lp.value)),
                f.space(),
            ))
            .append(")")
            .group(),
        UnwrappedOpaque {
            opaque, argument, ..
        } => f
            .text(format!("@{} ", opaque.module_string(c.interns)))
            .append(pattern(c, f, &argument.1.value))
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
                                .append(expr(c, f, &e.value)),
                            roc_can::pattern::DestructType::Guard(_, p) => f
                                .text(label.as_str())
                                .append(f.text(": "))
                                .append(pattern(c, f, &p.value)),
                        },
                    ),
                    f.text(", "),
                ),
            )
            .append(f.text("}"))
            .group(),
        NumLiteral(_, n, _, _) | IntLiteral(_, _, n, _, _) | FloatLiteral(_, _, n, _, _) => {
            f.text(&**n)
        }
        StrLiteral(s) => f.text(format!(r#""{}""#, s)),
        SingleQuote(c) => f.text(format!("'{}'", c)),
        Underscore => f.text("_"),

        Shadowed(_, _, _) => todo!(),
        OpaqueNotInScope(_) => todo!(),
        UnsupportedPattern(_) => todo!(),
        MalformedPattern(_, _) => todo!(),
    }
}
