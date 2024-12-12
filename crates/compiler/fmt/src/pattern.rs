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
        fmt_pattern_inner(self, buf, parens, indent, self.is_multiline(), false);
    }
}

fn fmt_pattern_inner(
    pat: &Pattern<'_>,
    buf: &mut Buf,
    parens: Parens,
    indent: u16,
    outer_is_multiline: bool,
    force_newline_at_start: bool,
) -> bool {
    let me = pattern_lift_spaces(buf.text.bump(), pat);

    let mut was_multiline = me.item.is_multiline();

    if !me.before.is_empty() {
        if !outer_is_multiline {
            was_multiline |= me.before.iter().any(|s| s.is_comment());
            fmt_comments_only(buf, me.before.iter(), NewlineAt::Bottom, indent)
        } else {
            was_multiline |= true;
            fmt_spaces(buf, me.before.iter(), indent);
        }
    }

    if force_newline_at_start {
        buf.ensure_ends_with_newline();
    }

    let is_multiline = me.item.is_multiline();

    fmt_pattern_only(me, buf, indent, parens, is_multiline);

    if !me.after.is_empty() {
        if starts_with_inline_comment(me.after.iter()) {
            buf.spaces(1);
        }

        if !outer_is_multiline {
            was_multiline |= me.before.iter().any(|s| s.is_comment());
            fmt_comments_only(buf, me.after.iter(), NewlineAt::Bottom, indent)
        } else {
            was_multiline |= true;
            fmt_spaces(buf, me.after.iter(), indent);
        }
    }

    was_multiline
}

fn fmt_pattern_only(
    me: Spaces<'_, Pattern<'_>>,
    buf: &mut Buf<'_>,
    indent: u16,
    parens: Parens,
    is_multiline: bool,
) {
    match me.item {
        Pattern::Identifier { ident: string } => {
            buf.indent(indent);
            snakify_camel_ident(buf, string);
        }
        Pattern::Tag(name) | Pattern::OpaqueRef(name) => {
            buf.indent(indent);
            buf.push_str(name);
        }
        Pattern::Apply(loc_pattern, loc_arg_patterns) => {
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

            fmt_pattern_inner(&pat.item, buf, Parens::InApply, indent, is_multiline, false);

            if !pat.after.is_empty() {
                if !is_multiline {
                    fmt_comments_only(buf, pat.after.iter(), NewlineAt::Bottom, indent_more)
                } else {
                    fmt_spaces(buf, pat.after.iter(), indent_more);
                }
            }

            let mut add_newlines = false;

            for loc_arg in loc_arg_patterns.iter() {
                buf.spaces(1);
                let was_multiline = fmt_pattern_inner(
                    &loc_arg.value,
                    buf,
                    Parens::InApply,
                    indent_more,
                    is_multiline,
                    add_newlines,
                );
                add_newlines |= was_multiline;
            }

            if parens {
                buf.push(')');
            }
        }
        Pattern::RecordDestructure(loc_patterns) => {
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

                    fmt_pattern_inner(
                        &item.item,
                        buf,
                        Parens::NotNeeded,
                        indent,
                        is_multiline,
                        false,
                    );

                    let is_multiline = item.item.is_multiline();

                    if it.peek().is_some() {
                        buf.push_str(",");
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
                    if it.peek().is_some() {
                        buf.ensure_ends_with_whitespace();
                    }
                }
                buf.spaces(1);
            }

            buf.indent(indent);
            buf.push_str("}");
        }

        Pattern::RequiredField(name, loc_pattern) => {
            buf.indent(indent);
            snakify_camel_ident(buf, name);
            buf.push_str(":");
            buf.spaces(1);
            fmt_pattern_inner(
                &loc_pattern.value,
                buf,
                Parens::NotNeeded,
                indent,
                is_multiline,
                false,
            );
        }

        Pattern::OptionalField(name, loc_pattern) => {
            buf.indent(indent);
            snakify_camel_ident(buf, name);
            buf.push_str(" ?");
            buf.spaces(1);
            loc_pattern.format(buf, indent);
        }

        Pattern::NumLiteral(string) => {
            buf.indent(indent);
            buf.push_str(string);
        }
        Pattern::NonBase10Literal {
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
        Pattern::FloatLiteral(string) => {
            buf.indent(indent);
            buf.push_str(string);
        }
        Pattern::StrLiteral(literal) => fmt_str_literal(buf, literal, indent),
        Pattern::SingleQuote(string) => {
            buf.indent(indent);
            format_sq_literal(buf, string);
        }
        Pattern::Underscore(name) => {
            buf.indent(indent);
            buf.push('_');
            buf.push_str(name);
        }
        Pattern::Tuple(loc_patterns) => {
            buf.indent(indent);
            buf.push_str("(");

            let mut add_newlines = false;

            let mut it = loc_patterns.iter().peekable();
            while let Some(loc_pattern) = it.next() {
                add_newlines |= fmt_pattern_inner(
                    &loc_pattern.value,
                    buf,
                    Parens::NotNeeded,
                    indent,
                    is_multiline,
                    add_newlines,
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
        Pattern::List(loc_patterns) => {
            buf.indent(indent);
            buf.push_str("[");

            let mut add_newlines = false;

            let mut it = loc_patterns.iter().peekable();
            while let Some(loc_pattern) = it.next() {
                add_newlines |= fmt_pattern_inner(
                    &loc_pattern.value,
                    buf,
                    Parens::NotNeeded,
                    indent,
                    is_multiline,
                    add_newlines,
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
        Pattern::ListRest(opt_pattern_as) => {
            buf.indent(indent);
            buf.push_str("..");

            if let Some((list_rest_spaces, pattern_as)) = opt_pattern_as {
                // these spaces "belong" to the `..`, which can never be multiline
                fmt_comments_only(buf, list_rest_spaces.iter(), NewlineAt::Bottom, indent);

                pattern_as.format(buf, indent + INDENT);
            }
        }

        Pattern::As(pattern, pattern_as) => {
            let needs_parens = parens == Parens::InAsPattern || parens == Parens::InApply;

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

        Pattern::SpaceBefore(..) | Pattern::SpaceAfter(..) => {
            unreachable!("handled by lift_spaces")
        }

        // Malformed
        Pattern::Malformed(string) | Pattern::MalformedIdent(string, _) => {
            buf.indent(indent);
            buf.push_str(string);
        }
        Pattern::QualifiedIdentifier { module_name, ident } => {
            buf.indent(indent);
            if !module_name.is_empty() {
                buf.push_str(module_name);
                buf.push('.');
            }

            snakify_camel_ident(buf, ident);
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

/// Convert camelCase identifier to snake case
fn snakify_camel_ident(buf: &mut Buf, string: &str) {
    let chars: Vec<char> = string.chars().collect();
    if !buf.flags().snakify || (string.contains('_') && !string.ends_with('_')) {
        buf.push_str(string);
        return;
    }
    let mut index = 0;
    let len = chars.len();

    while index < len {
        let prev = if index == 0 {
            None
        } else {
            Some(chars[index - 1])
        };
        let c = chars[index];
        let next = chars.get(index + 1);
        let boundary = match (prev, c, next) {
            // LUU, LUN, and LUL (simplified to LU_)
            (Some(p), curr, _) if !p.is_ascii_uppercase() && curr.is_ascii_uppercase() => true,
            // UUL
            (Some(p), curr, Some(n))
                if p.is_ascii_uppercase()
                    && curr.is_ascii_uppercase()
                    && n.is_ascii_lowercase() =>
            {
                true
            }
            _ => false,
        };
        // those are boundary transitions - should push _ and curr
        if boundary {
            buf.push('_');
        }
        buf.push(c.to_ascii_lowercase());
        index += 1;
    }
}

#[cfg(test)]
mod snakify_test {
    use bumpalo::Bump;

    use super::snakify_camel_ident;
    use crate::{Buf, MigrationFlags};

    fn check_snakify(arena: &Bump, original: &str) -> String {
        let flags = MigrationFlags::new(true);
        let mut buf = Buf::new_in(arena, flags);
        buf.indent(0);
        snakify_camel_ident(&mut buf, original);
        buf.text.to_string()
    }

    #[test]
    fn test_snakify_camel_ident() {
        let arena = Bump::new();
        assert_eq!(check_snakify(&arena, "A"), "a");
        assert_eq!(check_snakify(&arena, "Ba"), "ba");
        assert_eq!(check_snakify(&arena, "aB"), "a_b");
        assert_eq!(check_snakify(&arena, "aBa"), "a_ba");
        assert_eq!(check_snakify(&arena, "mBB"), "m_bb");
        assert_eq!(check_snakify(&arena, "NbA"), "nb_a");
        assert_eq!(check_snakify(&arena, "doIT"), "do_it");
        assert_eq!(check_snakify(&arena, "ROC"), "roc");
        assert_eq!(
            check_snakify(&arena, "someHTTPRequest"),
            "some_http_request"
        );
        assert_eq!(check_snakify(&arena, "usingXML"), "using_xml");
        assert_eq!(check_snakify(&arena, "some123"), "some123");
        assert_eq!(
            check_snakify(&arena, "theHTTPStatus404"),
            "the_http_status404"
        );
        assert_eq!(
            check_snakify(&arena, "inThe99thPercentile"),
            "in_the99th_percentile"
        );
        assert_eq!(
            check_snakify(&arena, "all400SeriesErrorCodes"),
            "all400_series_error_codes",
        );
        assert_eq!(check_snakify(&arena, "number4Yellow"), "number4_yellow");
        assert_eq!(check_snakify(&arena, "useCases4Cobol"), "use_cases4_cobol");
        assert_eq!(check_snakify(&arena, "c3PO"), "c3_po")
    }
}
