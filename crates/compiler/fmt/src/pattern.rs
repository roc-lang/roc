use crate::annotation::{Formattable, Newlines, Parens};
use crate::expr::{expr_lift_spaces_after, fmt_str_literal, format_sq_literal, is_str_multiline};
use crate::node::{Node, NodeInfo, NodeSequenceBuilder, Prec, Sp};
use crate::spaces::{fmt_comments_only, fmt_spaces, NewlineAt, INDENT};
use crate::Buf;
use bumpalo::Bump;
use roc_parse::ast::{
    Base, CommentOrNewline, Pattern, PatternAs, RecordFieldPattern, Spaceable, Spaces, SpacesAfter,
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

impl<'a> Formattable for RecordFieldPattern<'a> {
    fn is_multiline(&self) -> bool {
        match self {
            RecordFieldPattern::RequiredField { label: _, inner } => inner.is_multiline(),
            RecordFieldPattern::Identifier { label: _ } => false,
            RecordFieldPattern::OptionalField {
                label: _,
                default_value,
            } => default_value.is_multiline(),
            RecordFieldPattern::Spread { opt_pattern } => {
                opt_pattern.is_some_and(|pattern| pattern.is_multiline())
            }
            RecordFieldPattern::SpaceBefore(inner, _)
            | RecordFieldPattern::SpaceAfter(inner, _) => inner.is_multiline(),
        }
    }

    fn format_with_options(&self, buf: &mut Buf, parens: Parens, newlines: Newlines, indent: u16) {
        let is_multiline = newlines == Newlines::Yes;

        match self {
            RecordFieldPattern::RequiredField { label, inner } => {
                if is_multiline {
                    buf.newline();
                }

                buf.indent(indent);

                if buf.flags().snakify {
                    snakify_camel_ident(buf, label);
                } else {
                    buf.push_str(label);
                }

                buf.push_str_allow_spaces(" : ");

                inner.value.format(buf, indent);
            }
            RecordFieldPattern::Identifier { label } => {
                if is_multiline {
                    buf.newline();
                }

                buf.indent(indent);

                if buf.flags().snakify {
                    snakify_camel_ident(buf, label);
                } else {
                    buf.push_str(label);
                }
            }
            RecordFieldPattern::OptionalField {
                label,
                default_value,
            } => {
                if is_multiline {
                    buf.newline();
                }

                buf.indent(indent);

                if buf.flags().snakify {
                    snakify_camel_ident(buf, label);
                } else {
                    buf.push_str(label);
                }

                buf.push_str_allow_spaces(" ?? ");

                default_value.value.format(buf, indent);
            }
            RecordFieldPattern::Spread { opt_pattern } => {
                if is_multiline {
                    buf.newline();
                }

                buf.indent(indent);

                buf.push_str("..");

                if let Some(spread) = opt_pattern {
                    spread.value.format(buf, indent);
                }
            }
            RecordFieldPattern::SpaceBefore(sub_field, spaces) => {
                fmt_comments_only(buf, spaces.iter(), NewlineAt::Bottom, indent);
                sub_field.format_with_options(buf, parens, newlines, indent);
            }
            RecordFieldPattern::SpaceAfter(sub_field, spaces) => {
                sub_field.format_with_options(buf, parens, newlines, indent);
                fmt_comments_only(buf, spaces.iter(), NewlineAt::Bottom, indent);
            }
        }
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
            | Pattern::MalformedExpr(_)
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

    fmt_pattern_only(&me.item, buf, parens, indent, is_multiline);

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
            pattern_fmt_apply(
                buf,
                loc_pattern.value,
                loc_arg_patterns.items,
                Parens::NotNeeded,
                indent,
                is_multiline,
                true,
                Some(loc_arg_patterns.final_comments()),
            );
        }
        Pattern::Apply(loc_pattern, loc_arg_patterns) => {
            pattern_fmt_apply(
                buf,
                loc_pattern.value,
                loc_arg_patterns,
                parens,
                indent,
                is_multiline,
                false,
                None,
            );
        }
        Pattern::RecordDestructure(loc_patterns) => {
            buf.indent(indent);
            buf.push_str("{");

            if !loc_patterns.is_empty() {
                buf.spaces(1);
                let mut last_was_multiline = false;
                let mut it = loc_patterns.iter().peekable();
                while let Some(loc_pattern) = it.next() {
                    let item = record_field_pattern_lift_spaces(loc_pattern.value, buf.text.bump());

                    if !item.before.is_empty() {
                        if !is_multiline {
                            fmt_comments_only(buf, item.before.iter(), NewlineAt::Bottom, indent)
                        } else {
                            fmt_spaces(buf, item.before.iter(), indent);
                        }
                    }

                    if last_was_multiline {
                        buf.ensure_ends_with_newline();
                    }

                    // fmt_pattern_inner(
                    //     &item.item,
                    //     buf,
                    //     Parens::NotNeeded,
                    //     indent,
                    //     is_multiline,
                    //     false,
                    // );
                    // TODO

                    let is_multiline = item.item.is_multiline();
                    last_was_multiline = is_multiline;

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

        // Pattern::RequiredField(name, loc_pattern) => {
        //     buf.indent(indent);
        //     snakify_camel_ident(buf, name);
        //     buf.push_str(":");
        //     buf.spaces(1);
        //     fmt_pattern_inner(
        //         &loc_pattern.value,
        //         buf,
        //         Parens::NotNeeded,
        //         indent,
        //         is_multiline,
        //         false,
        //     );
        // }

        // Pattern::OptionalField(name, loc_pattern) => {
        //     buf.indent(indent);
        //     snakify_camel_ident(buf, name);
        //     buf.push_str(" ??");
        //     buf.spaces(1);
        //     loc_pattern.format(buf, indent);
        // }
        Pattern::NumLiteral(string) => {
            buf.indent(indent);
            let needs_parens = parens == Parens::InClosurePattern
                || (parens == Parens::InPncApplyFunc && string.starts_with('-'));
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
            let needs_parens = parens == Parens::InClosurePattern
                || (parens == Parens::InPncApplyFunc && *is_negative);
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
            let needs_parens = parens == Parens::InClosurePattern
                || (parens == Parens::InPncApplyFunc && string.starts_with('-'));
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
            let needs_parens = parens == Parens::InAsPattern
                || parens == Parens::InApply
                || parens == Parens::InPncApplyFunc
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
        Pattern::MalformedExpr(expr) => {
            buf.indent(indent);
            expr.format(buf, indent);
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

#[allow(clippy::too_many_arguments)]
pub fn pattern_fmt_apply(
    buf: &mut Buf<'_>,
    func: Pattern<'_>,
    args: &[Loc<Pattern<'_>>],
    parens: Parens,
    indent: u16,
    is_multiline: bool,
    is_pnc: bool,
    final_comments: Option<&[CommentOrNewline]>,
) {
    let use_commas_and_parens = is_pnc || buf.flags().parens_and_commas;
    buf.indent(indent);
    // Sometimes, an Apply pattern needs parens around it.
    // In particular when an Apply's argument is itself an Apply (> 0) arguments
    let parens = !args.is_empty()
        && (parens == Parens::InApply || parens == Parens::InPncApplyFunc)
        && !use_commas_and_parens;

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

    fmt_pattern_only(
        &func.item,
        buf,
        if is_pnc {
            Parens::InPncApplyFunc
        } else {
            Parens::InApply
        },
        indent,
        is_multiline,
    );

    if use_commas_and_parens {
        buf.push('(');
    }

    let mut last_after = func.after;

    let mut add_newlines = is_multiline;

    for (i, loc_arg) in args.iter().enumerate() {
        let is_last_arg = i == args.len() - 1;
        let is_first_arg = i == 0;

        if !(is_first_arg && use_commas_and_parens) {
            buf.spaces(1);
        }

        let parens = if use_commas_and_parens {
            Parens::NotNeeded
        } else {
            Parens::InApply
        };
        let arg = pattern_lift_spaces(buf.text.bump(), &loc_arg.value);

        let mut was_multiline = arg.item.is_multiline();

        let mut before = merge_spaces(buf.text.bump(), last_after, arg.before);

        if !before.is_empty() {
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
        if use_commas_and_parens && (!is_last_arg || is_multiline) {
            buf.push(',');
        }

        last_after = arg.after;

        add_newlines |= was_multiline;
    }

    if let Some(comments) = final_comments {
        if !is_multiline {
            fmt_comments_only(buf, comments.iter(), NewlineAt::Bottom, indent_more);
        } else {
            fmt_spaces(buf, comments.iter(), indent_more);
        }
    }
    if !last_after.is_empty() {
        if !is_multiline {
            fmt_comments_only(buf, last_after.iter(), NewlineAt::Bottom, indent_more)
        } else {
            fmt_spaces(buf, last_after.iter(), indent_more);
        }
    }

    if use_commas_and_parens {
        if is_multiline {
            buf.ensure_ends_with_newline();
            buf.indent(indent);
        }
        if buf.ends_with_newline() {
            buf.indent(indent);
        }
        if buf.ends_with_newline() {
            buf.indent(indent);
        }
        buf.push(')');
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
        Pattern::Malformed(_) | Pattern::MalformedIdent(..) | Pattern::MalformedExpr(_) => {
            Prec::Term
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

pub fn record_field_pattern_lift_spaces<'a>(
    field: RecordFieldPattern<'a>,
    arena: &'a Bump,
) -> Spaces<'a, RecordFieldPattern<'a>> {
    match field {
        RecordFieldPattern::RequiredField { label, inner } => {
            let lifted = pattern_lift_spaces_after(arena, &inner.value);

            Spaces {
                before: &[],
                item: RecordFieldPattern::RequiredField {
                    label,
                    inner: arena.alloc(Loc::at(inner.region, lifted.item)),
                },
                after: lifted.after,
            }
        }
        RecordFieldPattern::Identifier { label } => Spaces {
            before: &[],
            item: RecordFieldPattern::Identifier { label },
            after: &[],
        },
        RecordFieldPattern::OptionalField {
            label,
            default_value,
        } => {
            let lifted = expr_lift_spaces_after(Parens::NotNeeded, arena, &default_value.value);

            Spaces {
                before: &[],
                item: RecordFieldPattern::OptionalField {
                    label,
                    default_value: arena.alloc(Loc::at(default_value.region, lifted.item)),
                },
                after: lifted.after,
            }
        }
        RecordFieldPattern::Spread {
            opt_pattern: Some(pattern),
        } => {
            let lifted = pattern_lift_spaces_after(arena, &pattern.value);

            Spaces {
                before: &[],
                item: RecordFieldPattern::Spread {
                    opt_pattern: Some(arena.alloc(Loc::at(pattern.region, lifted.item))),
                },
                after: lifted.after,
            }
        }
        RecordFieldPattern::Spread { opt_pattern: None } => Spaces {
            before: &[],
            item: RecordFieldPattern::Spread { opt_pattern: None },
            after: &[],
        },

        RecordFieldPattern::SpaceBefore(expr, spaces) => {
            let mut inner = record_field_pattern_lift_spaces(*expr, arena);
            inner.before = merge_spaces(arena, spaces, inner.before);

            inner
        }
        RecordFieldPattern::SpaceAfter(expr, spaces) => {
            let mut inner = record_field_pattern_lift_spaces(*expr, arena);
            inner.after = merge_spaces(arena, inner.after, spaces);
            inner
        }
    }
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
                item: Pattern::PncApply(arena.alloc(Loc::at_zero(func_lifted.item)), *args),
                after: &[],
            }
        }
        Pattern::SpaceBefore(expr, spaces) => {
            let mut inner = pattern_lift_spaces(arena, expr);
            inner.before = merge_spaces(arena, spaces, inner.before);

            handle_multiline_str_spaces(expr, &mut inner.before);

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
