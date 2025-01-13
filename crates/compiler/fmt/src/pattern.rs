use crate::annotation::{Formattable, Newlines, Parens};
use crate::collection::{fmt_collection, fmt_record_like, Braces};
use crate::expr::{fmt_str_literal, format_spaces, format_sq_literal, is_str_multiline};
use crate::node::{Node, NodeInfo, NodeSequenceBuilder, Prec, Sp};
use crate::spaces::{
    assigned_field_to_spaces, fmt_comments_only, fmt_spaces, merge_spaces_conservative, NewlineAt,
    INDENT,
};
use crate::Buf;
use bumpalo::collections::Vec;
use bumpalo::Bump;
use roc_parse::ast::{
    Base, Collection, CommentOrNewline, Pattern, PatternAs, Spaceable, Spaces, SpacesAfter,
    SpacesBefore,
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
                true
            }

            Pattern::RecordDestructure(fields) => fields.iter().any(|f| f.is_multiline()),
            Pattern::ExprWrapped(e) => e.is_multiline(),
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
            Pattern::PncApply(pat, args) => {
                pat.is_multiline()
                    || args.iter().any(|a| a.is_multiline())
                    || !args.final_comments().is_empty()
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
        fmt_pattern_inner(self, buf, parens, indent);
    }
}

fn fmt_pattern_inner(pat: &Pattern<'_>, buf: &mut Buf, parens: Parens, indent: u16) {
    let me = pattern_lift_spaces(buf.text.bump(), pat);

    if !me.before.is_empty() {
        fmt_spaces(buf, me.before.iter(), indent);
    }

    fmt_pattern_only(&me.item, buf, parens, indent, me.item.is_multiline());

    if !me.after.is_empty() {
        fmt_spaces(buf, me.after.iter(), indent);
    }
}

fn fmt_pattern_only(
    me: &Pattern<'_>,
    buf: &mut Buf<'_>,
    parens: Parens,
    indent: u16,
    is_multiline: bool,
) {
    match me {
        Pattern::Identifier { ident } => {
            buf.indent(indent);
            snakify_camel_ident(buf, ident);
        }
        Pattern::Tag(name) | Pattern::OpaqueRef(name) => {
            buf.indent(indent);
            buf.push_str(name);
        }
        Pattern::PncApply(loc_pattern, loc_arg_patterns) => {
            pattern_fmt_pnc_apply(buf, loc_pattern.value, *loc_arg_patterns, indent);
        }
        Pattern::Apply(loc_pattern, loc_arg_patterns) => {
            if buf.flags().parens_and_commas {
                pattern_fmt_pnc_apply(
                    buf,
                    loc_pattern.value,
                    Collection::with_items(loc_arg_patterns),
                    indent,
                );
            } else {
                pattern_fmt_apply(
                    buf,
                    loc_pattern.value,
                    loc_arg_patterns,
                    parens,
                    indent,
                    is_multiline,
                );
            }
        }
        Pattern::RecordDestructure(loc_patterns) => {
            fmt_record_like(buf, None, *loc_patterns, indent, assigned_field_to_spaces);
        }
        Pattern::ExprWrapped(e) => {
            e.format(buf, indent);
        }

        Pattern::NumLiteral(string) => {
            buf.indent(indent);
            let needs_parens = parens == Parens::InClosurePattern;
            if needs_parens {
                buf.push('(');
            }
            buf.push_str(string);
            if needs_parens {
                buf.push(')');
            }
        }
        Pattern::NonBase10Literal {
            base,
            string,
            is_negative,
        } => {
            buf.indent(indent);
            let needs_parens = parens == Parens::InClosurePattern;
            if needs_parens {
                buf.push('(');
            }
            if *is_negative {
                buf.push('-');
            }

            match base {
                Base::Hex => buf.push_str("0x"),
                Base::Octal => buf.push_str("0o"),
                Base::Binary => buf.push_str("0b"),
                Base::Decimal => { /* nothing */ }
            }

            buf.push_str(string);

            if needs_parens {
                buf.push(')');
            }
        }
        Pattern::FloatLiteral(string) => {
            buf.indent(indent);
            let needs_parens = parens == Parens::InClosurePattern;
            if needs_parens {
                buf.push('(');
            }
            buf.push_str(string);
            if needs_parens {
                buf.push(')');
            }
        }
        Pattern::StrLiteral(literal) => {
            let needs_parens = parens == Parens::InClosurePattern;
            if needs_parens {
                buf.push('(');
            }
            fmt_str_literal(buf, *literal, indent);
            if needs_parens {
                buf.push(')');
            }
        }
        Pattern::SingleQuote(string) => {
            buf.indent(indent);
            let needs_parens = parens == Parens::InClosurePattern;
            if needs_parens {
                buf.push('(');
            }
            format_sq_literal(buf, string);
            if needs_parens {
                buf.push(')');
            }
        }
        Pattern::Underscore(name) => {
            buf.indent(indent);
            buf.push('_');
            buf.push_str(name);
        }
        Pattern::Tuple(loc_patterns) => {
            buf.indent(indent);
            buf.push_str("(");

            let mut it = loc_patterns.iter().peekable();
            while let Some(loc_pattern) = it.next() {
                fmt_pattern_inner(&loc_pattern.value, buf, Parens::NotNeeded, indent);

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
            fmt_collection(buf, indent, Braces::Square, *loc_patterns, Newlines::No);
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
            let needs_parens = parens == Parens::InAsPattern
                || parens == Parens::InApply
                || parens == Parens::InClosurePattern;

            if needs_parens {
                buf.indent(indent);
                buf.push('(');
            }

            fmt_pattern(buf, &pattern.value, indent, Parens::InAsPattern);

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

fn fmt_pattern_collection(
    buf: &mut Buf<'_>,

    indent: u16,
    braces: Braces,
    items: Collection<'_, &Loc<Pattern<'_>>>,
    newlines: Newlines,
) {
    let arena = buf.text.bump();
    let mut new_items: Vec<'_, &Pattern<'_>> = Vec::with_capacity_in(items.len(), arena);

    let mut last_after: &[CommentOrNewline<'_>] = &[];

    for item in items.items {
        let mut lifted = pattern_lift_spaces(arena, &item.value);
        lifted.before = merge_spaces_conservative(arena, last_after, lifted.before);
        last_after = lifted.after;
        lifted.after = &[];
        new_items.push(arena.alloc(lower(arena, lifted)));
    }

    let final_comments = merge_spaces_conservative(arena, last_after, items.final_comments());

    let new_items =
        Collection::with_items_and_comments(arena, new_items.into_bump_slice(), final_comments);

    fmt_collection(buf, indent, braces, new_items, newlines)
}

fn lower<'a, 'b: 'a>(arena: &'b Bump, lifted: Spaces<'b, Pattern<'b>>) -> Pattern<'b> {
    if lifted.before.is_empty() && lifted.after.is_empty() {
        return lifted.item;
    }
    if lifted.before.is_empty() {
        return Pattern::SpaceAfter(arena.alloc(lifted.item), lifted.after);
    }
    if lifted.after.is_empty() {
        return Pattern::SpaceBefore(arena.alloc(lifted.item), lifted.before);
    }
    Pattern::SpaceBefore(
        arena.alloc(Pattern::SpaceAfter(arena.alloc(lifted.item), lifted.after)),
        lifted.before,
    )
}

fn pattern_fmt_pnc_apply(
    buf: &mut Buf<'_>,
    func: Pattern<'_>,
    args: Collection<'_, Loc<Pattern<'_>>>,
    indent: u16,
) {
    let patt = pattern_lift_spaces(buf.text.bump(), &func);

    if !patt.before.is_empty() {
        format_spaces(buf, patt.before, Newlines::Yes, indent);
    }

    patt.item
        .format_with_options(buf, Parens::NotNeeded, Newlines::Yes, indent);
    fmt_pattern_collection(
        buf,
        indent,
        Braces::Round,
        args.ptrify_items(buf.text.bump()),
        Newlines::No,
    );
}

#[allow(clippy::too_many_arguments)]
pub fn pattern_fmt_apply(
    buf: &mut Buf<'_>,
    func: Pattern<'_>,
    args: &[Loc<Pattern<'_>>],
    parens: Parens,
    indent: u16,
    is_multiline: bool,
) {
    buf.indent(indent);
    // Sometimes, an Apply pattern needs parens around it.
    // In particular when an Apply's argument is itself an Apply (> 0) arguments
    let parens = !args.is_empty() && parens == Parens::InApply;

    let indent_more = if is_multiline {
        indent + INDENT
    } else {
        indent
    };

    if parens {
        buf.push('(');
    }

    let func = pattern_lift_spaces(buf.text.bump(), &func);

    if !func.before.is_empty() {
        if !is_multiline {
            fmt_comments_only(buf, func.before.iter(), NewlineAt::Bottom, indent)
        } else {
            fmt_spaces(buf, func.before.iter(), indent);
        }
    }

    fmt_pattern_only(&func.item, buf, Parens::InApply, indent, is_multiline);

    let mut last_after = func.after;

    let mut add_newlines = is_multiline;

    for loc_arg in args.iter() {
        buf.spaces(1);

        let parens = Parens::InApply;
        let arg = pattern_lift_spaces(buf.text.bump(), &loc_arg.value);

        let mut was_multiline = arg.item.is_multiline();

        let mut before = merge_spaces(buf.text.bump(), last_after, arg.before);

        if !before.is_empty() {
            if starts_with_block_str(&arg.item) {
                // Ick!
                // The block string will keep "generating" newlines when formatted (it wants to start on its own line),
                // so we strip one out here.
                //
                // Note that this doesn't affect Expr's because those have explicit parens, and we can control
                // whether spaces cross that boundary.
                let chop_off = before
                    .iter()
                    .rev()
                    .take_while(|&&s| matches!(s, CommentOrNewline::Newline))
                    .count();
                before = &before[..before.len() - chop_off];
            }
            handle_multiline_str_spaces(&arg.item, &mut before);

            if !is_multiline {
                was_multiline |= before.iter().any(|s| s.is_comment());
                fmt_comments_only(buf, before.iter(), NewlineAt::Bottom, indent_more)
            } else {
                was_multiline |= true;
                fmt_spaces(buf, before.iter(), indent_more);
            }
        }

        if add_newlines {
            buf.ensure_ends_with_newline();
        }

        if matches!(
            arg.item,
            Pattern::Identifier {
                ident: "implements"
            }
        ) {
            buf.indent(indent_more);
            buf.push_str("(implements)");
        } else {
            fmt_pattern_only(&arg.item, buf, parens, indent_more, arg.item.is_multiline());
        }

        last_after = arg.after;

        add_newlines |= was_multiline;
    }

    if !last_after.is_empty() {
        if !is_multiline {
            fmt_comments_only(buf, last_after.iter(), NewlineAt::Bottom, indent_more)
        } else {
            fmt_spaces(buf, last_after.iter(), indent_more);
        }
    }

    if parens {
        buf.push(')');
    }
}

pub fn pattern_apply_to_node<'b, 'a: 'b>(
    arena: &'b Bump,
    func: Pattern<'a>,
    args: &[Loc<Pattern<'a>>],
) -> NodeInfo<'b> {
    let func_lifted = pattern_lift_spaces(arena, &func);
    let mut b = NodeSequenceBuilder::new(arena, Node::Pattern(func_lifted.item), args.len(), true);

    let mut last_after = func_lifted.after;

    for arg in args {
        let arg_lifted = pattern_lift_spaces(arena, &arg.value);
        b.push(
            Sp::with_space(merge_spaces(arena, last_after, arg_lifted.before)),
            Node::Pattern(arg_lifted.item),
        );
        last_after = arg_lifted.after;
    }

    NodeInfo {
        before: func_lifted.before,
        node: b.build(),
        after: last_after,
        needs_indent: true,
        prec: if args.is_empty() {
            pattern_prec(func)
        } else {
            Prec::Apply
        },
    }
}

fn pattern_prec(pat: Pattern<'_>) -> Prec {
    match pat {
        Pattern::Identifier { .. }
        | Pattern::QualifiedIdentifier { .. }
        | Pattern::Tag(_)
        | Pattern::OpaqueRef(_)
        | Pattern::RecordDestructure(..)
        | Pattern::ExprWrapped(_)
        | Pattern::NumLiteral(_)
        | Pattern::NonBase10Literal { .. }
        | Pattern::FloatLiteral(_)
        | Pattern::StrLiteral(..)
        | Pattern::Underscore(_)
        | Pattern::SingleQuote(_)
        | Pattern::Tuple(..)
        | Pattern::List(..)
        | Pattern::ListRest(_)
        | Pattern::PncApply(_, _) => Prec::Term,
        Pattern::Apply(_, _) | Pattern::As(_, _) => Prec::Apply,
        Pattern::SpaceBefore(inner, _) | Pattern::SpaceAfter(inner, _) => pattern_prec(*inner),
        Pattern::Malformed(_) | Pattern::MalformedIdent(..) => Prec::Term,
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
        Pattern::PncApply(func, args) => {
            let func_lifted = pattern_lift_spaces_before(arena, &func.value);

            Spaces {
                before: func_lifted.before,
                item: Pattern::PncApply(arena.alloc(Loc::at(func.region, func_lifted.item)), *args),
                after: &[],
            }
        }
        Pattern::SpaceBefore(expr, spaces) => {
            let mut inner = pattern_lift_spaces(arena, expr);
            inner.before = merge_spaces_conservative(arena, spaces, inner.before);

            inner
        }
        Pattern::SpaceAfter(expr, spaces) => {
            let mut inner = pattern_lift_spaces(arena, expr);
            inner.after = merge_spaces_conservative(arena, inner.after, spaces);
            inner
        }
        _ => Spaces {
            before: &[],
            item: *pat,
            after: &[],
        },
    }
}

fn handle_multiline_str_spaces<'a>(pat: &Pattern<'_>, before: &mut &'a [CommentOrNewline<'a>]) {
    if starts_with_block_str(pat) {
        // Ick!
        // The block string will keep "generating" newlines when formatted (it wants to start on its own line),
        // so we strip one out here.
        //
        // Note that this doesn't affect Expr's because those have explicit parens, and we can control
        // whether spaces cross that boundary.
        let chop_off = before
            .iter()
            .rev()
            .take_while(|&&s| matches!(s, CommentOrNewline::Newline))
            .count();
        *before = &before[..before.len() - chop_off];
    }
}

fn starts_with_block_str(item: &Pattern<'_>) -> bool {
    match item {
        Pattern::As(inner, _) | Pattern::Apply(inner, _) | Pattern::PncApply(inner, _) => {
            starts_with_block_str(&inner.value)
        }
        Pattern::SpaceBefore(inner, _) | Pattern::SpaceAfter(inner, _) => {
            starts_with_block_str(inner)
        }
        Pattern::StrLiteral(str_literal) => is_str_multiline(str_literal),
        _ => false,
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
pub fn snakify_camel_ident(buf: &mut Buf, string: &str) {
    let chars: std::vec::Vec<char> = string.chars().collect();
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
        let flags = MigrationFlags {
            snakify: true,
            parens_and_commas: false,
        };
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
