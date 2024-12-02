use crate::annotation::{Formattable, Newlines, Parens};
use crate::expr::{
    expr_is_multiline, expr_lift_spaces_after, fmt_str_literal, format_sq_literal, is_str_multiline,
};
use crate::spaces::{fmt_comments_only, fmt_spaces, NewlineAt, INDENT};
use crate::Buf;
use bumpalo::Bump;
use roc_parse::ast::{
    Base, CommentOrNewline, Pattern, PatternAs, Spaceable, Spaces, SpacesAfter, SpacesBefore,
};
use roc_parse::expr::merge_spaces;
use roc_region::all::Loc;

pub fn fmt_pattern<'a>(buf: &mut Buf, pattern: &'a Pattern<'a>, indent: u16, parens: Parens) {
    pattern.format_with_options(buf, parens, Newlines::No, indent);
}

impl<'a> Formattable for PatternAs<'a> {
    fn is_multiline(&self) -> bool {
        self.spaces_before.iter().any(|s| s.is_comment())
    }

    fn format_with_options(
        &self,
        buf: &mut Buf,
        _parens: Parens,
        _newlines: Newlines,
        indent: u16,
    ) {
        buf.indent(indent);

        if !buf.ends_with_space() {
            buf.spaces(1);
        }

        buf.push_str("as");
        buf.spaces(1);

        // these spaces "belong" to the identifier, which can never be multiline
        fmt_comments_only(buf, self.spaces_before.iter(), NewlineAt::Bottom, indent);

        buf.indent(indent);
        buf.push_str(self.identifier.value);
    }
}

impl<'a> Formattable for Pattern<'a> {
    fn is_multiline(&self) -> bool {
        // Theory: a pattern should only be multiline when it contains a comment
        match self {
            Pattern::SpaceBefore(pattern, spaces) | Pattern::SpaceAfter(pattern, spaces) => {
                debug_assert!(
                    !spaces.is_empty(),
                    "spaces is empty in pattern {:#?}",
                    pattern
                );

                spaces.iter().any(|s| s.is_comment()) || pattern.is_multiline()
            }

            Pattern::RecordDestructure(fields) => fields.iter().any(|f| f.is_multiline()),
            Pattern::RequiredField(_, subpattern) => subpattern.is_multiline(),

            Pattern::OptionalField(_, expr) => expr_is_multiline(&expr.value, true),

            Pattern::As(pattern, pattern_as) => pattern.is_multiline() || pattern_as.is_multiline(),
            Pattern::ListRest(opt_pattern_as) => match opt_pattern_as {
                None => false,
                Some((list_rest_spaces, pattern_as)) => {
                    list_rest_spaces.iter().any(|s| s.is_comment()) || pattern_as.is_multiline()
                }
            },
            Pattern::StrLiteral(literal) => is_str_multiline(literal),
            Pattern::Apply(pat, args) => {
                pat.is_multiline() || args.iter().any(|a| a.is_multiline())
            }

            Pattern::Identifier { .. }
            | Pattern::Tag(_)
            | Pattern::OpaqueRef(_)
            | Pattern::NumLiteral(..)
            | Pattern::NonBase10Literal { .. }
            | Pattern::FloatLiteral(..)
            | Pattern::SingleQuote(_)
            | Pattern::Underscore(_)
            | Pattern::Malformed(_)
            | Pattern::MalformedIdent(_, _)
            | Pattern::QualifiedIdentifier { .. } => false,

            Pattern::Tuple(patterns) | Pattern::List(patterns) => {
                patterns.iter().any(|p| p.is_multiline())
            }
        }
    }

    fn format_with_options(&self, buf: &mut Buf, parens: Parens, _newlines: Newlines, indent: u16) {
        fmt_pattern_inner(self, buf, parens, indent, self.is_multiline());
    }
}

fn fmt_pattern_inner(
    pat: &Pattern<'_>,
    buf: &mut Buf,
    parens: Parens,
    indent: u16,
    outer_is_multiline: bool,
) {
    use self::Pattern::*;

    let me = pattern_lift_spaces(buf.text.bump(), pat);

    if !me.before.is_empty() {
        if !outer_is_multiline {
            fmt_comments_only(buf, me.before.iter(), NewlineAt::Bottom, indent)
        } else {
            fmt_spaces(buf, me.before.iter(), indent);
        }
    }

    let is_multiline = me.item.is_multiline();

    match me.item {
        Identifier { ident: string } => {
            buf.indent(indent);
            buf.push_str(string);
        }
        Tag(name) | OpaqueRef(name) => {
            buf.indent(indent);
            buf.push_str(name);
        }
        Apply(loc_pattern, loc_arg_patterns) => {
            buf.indent(indent);
            // Sometimes, an Apply pattern needs parens around it.
            // In particular when an Apply's argument is itself an Apply (> 0) arguments
            let parens = !loc_arg_patterns.is_empty() && (parens == Parens::InApply);

            let indent_more = if is_multiline {
                indent + INDENT
            } else {
                indent
            };

            if parens {
                buf.push('(');
            }

            let pat = pattern_lift_spaces(buf.text.bump(), &loc_pattern.value);

            if !pat.before.is_empty() {
                if !is_multiline {
                    fmt_comments_only(buf, pat.before.iter(), NewlineAt::Bottom, indent)
                } else {
                    fmt_spaces(buf, pat.before.iter(), indent);
                }
            }

            fmt_pattern_inner(&pat.item, buf, Parens::InApply, indent, is_multiline);

            if !pat.after.is_empty() {
                if !is_multiline {
                    fmt_comments_only(buf, pat.after.iter(), NewlineAt::Bottom, indent_more)
                } else {
                    fmt_spaces(buf, pat.after.iter(), indent_more);
                }
            }

            for loc_arg in loc_arg_patterns.iter() {
                buf.spaces(1);
                fmt_pattern_inner(
                    &loc_arg.value,
                    buf,
                    Parens::InApply,
                    indent_more,
                    is_multiline,
                );
            }

            if parens {
                buf.push(')');
            }
        }
        RecordDestructure(loc_patterns) => {
            buf.indent(indent);
            buf.push_str("{");

            if !loc_patterns.is_empty() {
                buf.spaces(1);
                let mut it = loc_patterns.iter().peekable();
                while let Some(loc_pattern) = it.next() {
                    let item = pattern_lift_spaces(buf.text.bump(), &loc_pattern.value);

                    if !item.before.is_empty() {
                        if !is_multiline {
                            fmt_comments_only(buf, item.before.iter(), NewlineAt::Bottom, indent)
                        } else {
                            fmt_spaces(buf, item.before.iter(), indent);
                        }
                    }

                    fmt_pattern_inner(&item.item, buf, Parens::NotNeeded, indent, is_multiline);

                    let is_multiline = item.item.is_multiline();

                    if it.peek().is_some() {
                        buf.push_str(",");
                        buf.spaces(1);
                    }

                    if !item.after.is_empty() {
                        if starts_with_inline_comment(item.after.iter()) {
                            buf.spaces(1);
                        }

                        if !is_multiline {
                            fmt_comments_only(buf, item.after.iter(), NewlineAt::Bottom, indent)
                        } else {
                            fmt_spaces(buf, item.after.iter(), indent);
                        }
                    }
                }
                buf.spaces(1);
            }

            buf.indent(indent);
            buf.push_str("}");
        }

        RequiredField(name, loc_pattern) => {
            buf.indent(indent);
            buf.push_str(name);
            buf.push_str(":");
            buf.spaces(1);
            fmt_pattern_inner(
                &loc_pattern.value,
                buf,
                Parens::NotNeeded,
                indent,
                is_multiline,
            );
        }

        OptionalField(name, loc_pattern) => {
            buf.indent(indent);
            buf.push_str(name);
            buf.push_str(" ?");
            buf.spaces(1);
            loc_pattern.format(buf, indent);
        }

        NumLiteral(string) => {
            buf.indent(indent);
            buf.push_str(string);
        }
        NonBase10Literal {
            base,
            string,
            is_negative,
        } => {
            buf.indent(indent);
            if is_negative {
                buf.push('-');
            }

            match base {
                Base::Hex => buf.push_str("0x"),
                Base::Octal => buf.push_str("0o"),
                Base::Binary => buf.push_str("0b"),
                Base::Decimal => { /* nothing */ }
            }

            buf.push_str(string);
        }
        FloatLiteral(string) => {
            buf.indent(indent);
            buf.push_str(string);
        }
        StrLiteral(literal) => fmt_str_literal(buf, literal, indent),
        SingleQuote(string) => {
            buf.indent(indent);
            format_sq_literal(buf, string);
        }
        Underscore(name) => {
            buf.indent(indent);
            buf.push('_');
            buf.push_str(name);
        }
        Tuple(loc_patterns) => {
            buf.indent(indent);
            buf.push_str("(");

            let mut it = loc_patterns.iter().peekable();
            while let Some(loc_pattern) = it.next() {
                fmt_pattern_inner(
                    &loc_pattern.value,
                    buf,
                    Parens::NotNeeded,
                    indent,
                    is_multiline,
                );

                if it.peek().is_some() {
                    buf.indent(indent);
                    buf.push_str(",");
                    buf.spaces(1);
                }
            }

            buf.indent(indent);
            buf.push_str(")");
        }
        List(loc_patterns) => {
            buf.indent(indent);
            buf.push_str("[");

            let mut it = loc_patterns.iter().peekable();
            while let Some(loc_pattern) = it.next() {
                fmt_pattern_inner(
                    &loc_pattern.value,
                    buf,
                    Parens::NotNeeded,
                    indent,
                    is_multiline,
                );

                if it.peek().is_some() {
                    buf.indent(indent);
                    buf.push_str(",");
                    buf.spaces(1);
                }
            }

            buf.indent(indent);
            buf.push_str("]");
        }
        ListRest(opt_pattern_as) => {
            buf.indent(indent);
            buf.push_str("..");

            if let Some((list_rest_spaces, pattern_as)) = opt_pattern_as {
                // these spaces "belong" to the `..`, which can never be multiline
                fmt_comments_only(buf, list_rest_spaces.iter(), NewlineAt::Bottom, indent);

                pattern_as.format(buf, indent + INDENT);
            }
        }

        As(pattern, pattern_as) => {
            let needs_parens = parens == Parens::InAsPattern;

            if needs_parens {
                buf.indent(indent);
                buf.push('(');
            }

            fmt_pattern(buf, &pattern.value, indent, parens);

            pattern_as.format(buf, indent + INDENT);

            if needs_parens {
                buf.indent(indent);
                buf.push(')');
            }
        }

        SpaceBefore(..) | SpaceAfter(..) => unreachable!("handled by lift_spaces"),

        // Malformed
        Malformed(string) | MalformedIdent(string, _) => {
            buf.indent(indent);
            buf.push_str(string);
        }
        QualifiedIdentifier { module_name, ident } => {
            buf.indent(indent);
            if !module_name.is_empty() {
                buf.push_str(module_name);
                buf.push('.');
            }

            buf.push_str(ident);
        }
    }

    if !me.after.is_empty() {
        if starts_with_inline_comment(me.after.iter()) {
            buf.spaces(1);
        }

        if !outer_is_multiline {
            fmt_comments_only(buf, me.after.iter(), NewlineAt::Bottom, indent)
        } else {
            fmt_spaces(buf, me.after.iter(), indent);
        }
    }
}

pub fn starts_with_inline_comment<'a, I: IntoIterator<Item = &'a CommentOrNewline<'a>>>(
    spaces: I,
) -> bool {
    matches!(
        spaces.into_iter().next(),
        Some(CommentOrNewline::LineComment(_))
    )
}

pub fn pattern_lift_spaces<'a, 'b: 'a>(
    arena: &'a Bump,
    pat: &Pattern<'b>,
) -> Spaces<'a, Pattern<'a>> {
    match pat {
        Pattern::Apply(func, args) => {
            let func_lifted = pattern_lift_spaces(arena, &func.value);
            let args = arena.alloc_slice_copy(args);
            let (before, func, after) = if let Some(last) = args.last_mut() {
                let last_lifted = pattern_lift_spaces(arena, &last.value);
                if last_lifted.before.is_empty() {
                    *last = Loc::at(last.region, last_lifted.item)
                } else {
                    *last = Loc::at(
                        last.region,
                        Pattern::SpaceBefore(arena.alloc(last_lifted.item), last_lifted.before),
                    );
                }

                let f = if func_lifted.after.is_empty() {
                    func_lifted.item
                } else {
                    Pattern::SpaceAfter(arena.alloc(func_lifted.item), func_lifted.after)
                };

                (
                    func_lifted.before,
                    Loc::at(func.region, f),
                    last_lifted.after,
                )
            } else {
                (
                    func_lifted.before,
                    Loc::at(func.region, func_lifted.item),
                    func_lifted.after,
                )
            };
            Spaces {
                before,
                item: Pattern::Apply(arena.alloc(func), args),
                after,
            }
        }
        Pattern::OptionalField(name, expr) => {
            let lifted = expr_lift_spaces_after(Parens::NotNeeded, arena, &expr.value);
            Spaces {
                before: &[],
                item: Pattern::OptionalField(name, arena.alloc(Loc::at(expr.region, lifted.item))),
                after: lifted.after,
            }
        }
        Pattern::RequiredField(name, pat) => {
            let lifted = pattern_lift_spaces_after(arena, &pat.value);
            Spaces {
                before: &[],
                item: Pattern::RequiredField(name, arena.alloc(Loc::at(pat.region, lifted.item))),
                after: lifted.after,
            }
        }
        Pattern::SpaceBefore(expr, spaces) => {
            let mut inner = pattern_lift_spaces(arena, expr);
            inner.before = merge_spaces(arena, spaces, inner.before);
            inner
        }
        Pattern::SpaceAfter(expr, spaces) => {
            let mut inner = pattern_lift_spaces(arena, expr);
            inner.after = merge_spaces(arena, inner.after, spaces);
            inner
        }
        _ => Spaces {
            before: &[],
            item: *pat,
            after: &[],
        },
    }
}

pub fn pattern_lift_spaces_before<'a, 'b: 'a>(
    arena: &'a Bump,
    pat: &Pattern<'b>,
) -> SpacesBefore<'a, Pattern<'a>> {
    let lifted = pattern_lift_spaces(arena, pat);
    SpacesBefore {
        before: lifted.before,
        item: lifted.item.maybe_after(arena, lifted.after),
    }
}

pub fn pattern_lift_spaces_after<'a, 'b: 'a>(
    arena: &'a Bump,
    pat: &Pattern<'b>,
) -> SpacesAfter<'a, Pattern<'a>> {
    let lifted = pattern_lift_spaces(arena, pat);
    SpacesAfter {
        item: lifted.item.maybe_before(arena, lifted.before),
        after: lifted.after,
    }
}
