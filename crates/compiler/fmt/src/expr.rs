use crate::annotation::{except_last, is_collection_multiline, Formattable, Newlines, Parens};
use crate::collection::{fmt_collection, Braces};
use crate::def::{fmt_defs, valdef_lift_spaces_before};
use crate::pattern::{
    fmt_pattern, pattern_lift_spaces, pattern_lift_spaces_before, starts_with_inline_comment,
};
use crate::spaces::{
    count_leading_newlines, fmt_comments_only, fmt_spaces, fmt_spaces_no_blank_lines,
    fmt_spaces_with_newline_mode, NewlineAt, SpacesNewlineMode, INDENT,
};
use crate::Buf;
use bumpalo::collections::Vec;
use bumpalo::Bump;
use roc_module::called_via::{self, BinOp, UnaryOp};
use roc_parse::ast::{
    AssignedField, Base, Collection, CommentOrNewline, Expr, ExtractSpaces, Pattern, Spaceable,
    Spaces, SpacesAfter, SpacesBefore, TryTarget, WhenBranch,
};
use roc_parse::ast::{StrLiteral, StrSegment};
use roc_parse::expr::merge_spaces;
use roc_parse::ident::Accessor;
use roc_parse::keyword;
use roc_region::all::Loc;
use soa::Slice;

impl<'a> Formattable for Expr<'a> {
    fn is_multiline(&self) -> bool {
        // TODO cache these answers using a Map<Pointer, bool>, so
        // we don't have to traverse subexpressions repeatedly
        expr_is_multiline(self, false)
    }

    fn format_with_options(&self, buf: &mut Buf, parens: Parens, newlines: Newlines, indent: u16) {
        let me = expr_lift_spaces(parens, buf.text.bump(), self);

        if !me.before.is_empty() {
            format_spaces(buf, me.before, newlines, indent);
        }

        format_expr_only(&me.item, buf, parens, newlines, indent);

        if !me.after.is_empty() {
            format_spaces(buf, me.after, newlines, indent);
        }
    }
}

fn format_expr_only(
    item: &Expr<'_>,
    buf: &mut Buf,
    parens: Parens,
    newlines: Newlines,
    indent: u16,
) {
    match &item {
        Expr::SpaceBefore(_sub_expr, _spaces) | Expr::SpaceAfter(_sub_expr, _spaces) => {
            unreachable!()
        }
        Expr::ParensAround(sub_expr) => {
            if parens == Parens::NotNeeded && !sub_expr_requests_parens(sub_expr) {
                sub_expr.format_with_options(buf, Parens::NotNeeded, newlines, indent);
            } else {
                fmt_parens(sub_expr, buf, indent);
            }
        }
        Expr::Str(literal) => {
            fmt_str_literal(buf, *literal, indent);
        }
        Expr::Var { module_name, ident } => {
            buf.indent(indent);
            if !module_name.is_empty() {
                buf.push_str(module_name);
                buf.push('.');
            }

            buf.push_str(ident);
        }
        Expr::Underscore(name) => {
            buf.indent(indent);
            buf.push('_');
            buf.push_str(name);
        }
        Expr::Crash => {
            buf.indent(indent);
            buf.push_str("crash");
        }
        Expr::Try => {
            buf.indent(indent);
            buf.push_str("try");
        }
        Expr::Apply(loc_expr, loc_args, _) => {
            let apply_needs_parens = parens == Parens::InApply || parens == Parens::InApplyLastArg;

            if apply_needs_parens && !loc_args.is_empty() {
                fmt_parens(item, buf, indent);
            } else {
                fmt_apply(loc_expr, loc_args, indent, buf);
            }
        }
        &Expr::Num(string) => {
            buf.indent(indent);
            buf.push_str(string);
        }
        &Expr::Float(string) => {
            buf.indent(indent);
            buf.push_str(string);
        }
        Expr::Tag(string) | Expr::OpaqueRef(string) => {
            buf.indent(indent);
            buf.push_str(string)
        }
        Expr::SingleQuote(string) => {
            buf.indent(indent);
            format_sq_literal(buf, string);
        }
        Expr::NonBase10Int {
            base,
            string,
            is_negative,
        } => {
            buf.indent(indent);
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
        }
        Expr::Record(fields) => {
            fmt_record_like(buf, None, *fields, indent, assigned_field_to_spaces);
        }
        Expr::RecordUpdate { update, fields } => {
            fmt_record_like(
                buf,
                Some(RecordPrefix::Update(update)),
                *fields,
                indent,
                assigned_field_to_spaces,
            );
        }
        Expr::RecordBuilder { mapper, fields } => {
            fmt_record_like(
                buf,
                Some(RecordPrefix::Mapper(mapper)),
                *fields,
                indent,
                assigned_field_to_spaces,
            );
        }
        Expr::Closure(loc_patterns, loc_ret) => {
            fmt_closure(buf, loc_patterns, loc_ret, indent);
        }
        Expr::Backpassing(loc_patterns, loc_body, loc_ret) => {
            fmt_backpassing(buf, loc_patterns, loc_body, loc_ret, indent);
        }
        Expr::Defs(defs, ret) => {
            let defs_needs_parens = parens == Parens::InOperator || parens == Parens::InApply;

            if defs_needs_parens {
                fmt_parens(item, buf, indent)
            } else {
                // It should theoretically be impossible to *parse* an empty defs list.
                // (Canonicalization can remove defs later, but that hasn't happened yet!)
                debug_assert!(!defs.is_empty());

                fmt_defs(buf, defs, indent);

                match &ret.value {
                    Expr::SpaceBefore(sub_expr, spaces) => {
                        buf.spaces(1);
                        fmt_spaces(buf, spaces.iter(), indent);

                        buf.indent(indent);

                        sub_expr.format_with_options(buf, Parens::NotNeeded, Newlines::Yes, indent);
                    }
                    _ => {
                        buf.ensure_ends_with_newline();
                        buf.indent(indent);
                        // Even if there were no defs, which theoretically should never happen,
                        // still print the return value.
                        ret.format_with_options(buf, Parens::NotNeeded, Newlines::Yes, indent);
                    }
                }
            }
        }
        Expr::Dbg => {
            buf.indent(indent);
            buf.push_str("dbg");
        }
        Expr::DbgStmt {
            first: condition,
            extra_args,
            continuation,
        } => {
            fmt_dbg_stmt(buf, condition, extra_args, continuation, parens, indent);
        }
        Expr::LowLevelDbg(_, _, _) => {
            unreachable!("LowLevelDbg should only exist after desugaring, not during formatting")
        }
        Expr::LowLevelTry(..) => {
            unreachable!("LowLevelTry should only exist after desugaring, not during formatting")
        }
        Expr::Return(return_value, after_return) => {
            fmt_return(buf, return_value, after_return, parens, newlines, indent);
        }
        Expr::If {
            if_thens: branches,
            final_else,
            indented_else,
        } => {
            fmt_if(
                buf,
                branches,
                final_else,
                item.is_multiline(),
                *indented_else,
                indent,
            );
        }
        Expr::When(loc_condition, branches) => fmt_when(buf, loc_condition, branches, indent),
        Expr::Tuple(items) => fmt_expr_collection(buf, indent, Braces::Round, *items, Newlines::No),
        Expr::List(items) => fmt_expr_collection(buf, indent, Braces::Square, *items, Newlines::No),
        Expr::BinOps(lefts, right) => fmt_binops(buf, lefts, right, indent),
        Expr::UnaryOp(sub_expr, unary_op) => {
            buf.indent(indent);
            match &unary_op.value {
                called_via::UnaryOp::Negate => {
                    buf.push('-');
                }
                called_via::UnaryOp::Not => {
                    buf.push('!');
                }
            }

            let lifted = expr_lift_spaces(Parens::InOperator, buf.text.bump(), &sub_expr.value);

            let before_all_newlines = lifted.before.iter().all(|s| s.is_newline());

            let needs_newline = !before_all_newlines
                || match &lifted.item {
                    Expr::Str(text) => is_str_multiline(text),
                    _ => false,
                };

            let needs_parens = (needs_newline
                && matches!(unary_op.value, called_via::UnaryOp::Negate))
                || matches!(
                    lifted.item,
                    Expr::Apply(..) | Expr::BinOps(..) | Expr::Defs(..)
                )
                || (matches!(unary_op.value, called_via::UnaryOp::Negate)
                    && requires_space_after_unary(&lifted.item));

            if needs_parens {
                // Unary negation can't be followed by whitespace (which is what a newline is) - so
                // we need to wrap the negated value in parens.
                fmt_parens(&sub_expr.value, buf, indent);
            } else {
                if matches!(unary_op.value, called_via::UnaryOp::Not)
                    && requires_space_after_unary(&lifted.item)
                {
                    // If the subexpression is an accessor function, we need to add a space,
                    // since `!.foo` doesn't parse. Yes, this wouldn't be valid anyway,
                    // but the formatter needs to be able to format invalid code.
                    buf.spaces(1);
                }

                let inner_indent = if needs_newline {
                    indent + INDENT
                } else {
                    indent
                };

                let inner_parens = if needs_parens {
                    Parens::NotNeeded
                } else {
                    Parens::InApply
                };

                if !before_all_newlines {
                    format_spaces(buf, lifted.before, newlines, inner_indent);
                }
                lifted
                    .item
                    .format_with_options(buf, inner_parens, newlines, inner_indent);
                format_spaces(buf, lifted.after, newlines, inner_indent);
            }
        }
        Expr::AccessorFunction(key) => {
            buf.indent(indent);
            buf.push('.');
            match key {
                Accessor::RecordField(key) => buf.push_str(key),
                Accessor::TupleIndex(key) => buf.push_str(key),
            }
        }
        Expr::RecordUpdater(key) => {
            buf.indent(indent);
            buf.push('&');
            buf.push_str(key);
        }
        Expr::RecordAccess(expr, key) => {
            expr.format_with_options(buf, Parens::InApply, Newlines::Yes, indent);
            buf.push('.');
            buf.push_str(key);
        }
        Expr::TupleAccess(expr, key) => {
            expr.format_with_options(buf, Parens::InApply, Newlines::Yes, indent);
            buf.push('.');
            buf.push_str(key);
        }
        Expr::TrySuffix { expr, target } => {
            expr.format_with_options(buf, Parens::InApply, Newlines::Yes, indent);
            match target {
                TryTarget::Task => buf.push('!'),
                TryTarget::Result => buf.push('?'),
            }
        }
        Expr::MalformedIdent(str, _) => {
            buf.indent(indent);
            buf.push_str(str)
        }
        Expr::MalformedSuffixed(loc_expr) => {
            buf.indent(indent);
            loc_expr.format_with_options(buf, parens, newlines, indent);
        }
        Expr::PrecedenceConflict { .. } => {}
        Expr::EmptyRecordBuilder { .. } => {}
        Expr::SingleFieldRecordBuilder { .. } => {}
        Expr::OptionalFieldInRecordBuilder(_, _) => {}
    }
}

pub fn expr_is_multiline(me: &Expr<'_>, comments_only: bool) -> bool {
    match me {
        // Return whether these spaces contain any Newlines
        Expr::SpaceBefore(sub_expr, spaces) | Expr::SpaceAfter(sub_expr, spaces) => {
            debug_assert!(!spaces.is_empty());

            if comments_only {
                spaces.iter().any(|s| s.is_comment()) || expr_is_multiline(sub_expr, comments_only)
            } else {
                true
            }
        }

        Expr::MalformedSuffixed(loc_expr) => expr_is_multiline(&loc_expr.value, comments_only),

        // These expressions never have newlines
        Expr::Float(..)
        | Expr::Num(..)
        | Expr::NonBase10Int { .. }
        | Expr::SingleQuote(_)
        | Expr::AccessorFunction(_)
        | Expr::RecordUpdater(_)
        | Expr::Var { .. }
        | Expr::Underscore { .. }
        | Expr::MalformedIdent(_, _)
        | Expr::Tag(_)
        | Expr::OpaqueRef(_)
        | Expr::Crash
        | Expr::Dbg
        | Expr::Try => false,
        Expr::LowLevelTry(_, _) => {
            unreachable!("LowLevelTry should only exist after desugaring, not during formatting")
        }

        Expr::RecordAccess(inner, _)
        | Expr::TupleAccess(inner, _)
        | Expr::TrySuffix { expr: inner, .. } => expr_is_multiline(inner, comments_only),

        // These expressions always have newlines
        Expr::Defs(_, _) | Expr::When(_, _) => true,

        Expr::List(items) => is_collection_multiline(items),

        Expr::Str(literal) => is_str_multiline(literal),
        Expr::Apply(loc_expr, args, _) => {
            expr_is_multiline(&loc_expr.value, comments_only)
                || args
                    .iter()
                    .any(|loc_arg| expr_is_multiline(&loc_arg.value, comments_only))
        }

        Expr::DbgStmt {
            first: condition,
            extra_args,
            ..
        } => {
            expr_is_multiline(&condition.value, comments_only)
                || extra_args
                    .iter()
                    .any(|loc_arg| expr_is_multiline(&loc_arg.value, comments_only))
        }
        Expr::LowLevelDbg(_, _, _) => {
            unreachable!("LowLevelDbg should only exist after desugaring, not during formatting")
        }
        Expr::Return(_return_value, _after_return) => true,

        Expr::If {
            if_thens: branches,
            final_else,
            ..
        } => {
            expr_is_multiline(&final_else.value, comments_only)
                || branches.iter().any(|(c, t)| {
                    expr_is_multiline(&c.value, comments_only)
                        || expr_is_multiline(&t.value, comments_only)
                })
        }

        Expr::BinOps(lefts, loc_right) => {
            lefts
                .iter()
                .any(|(loc_expr, _)| expr_is_multiline(&loc_expr.value, comments_only))
                || expr_is_multiline(&loc_right.value, comments_only)
        }

        Expr::UnaryOp(loc_subexpr, _)
        | Expr::PrecedenceConflict(roc_parse::ast::PrecedenceConflict {
            expr: loc_subexpr, ..
        })
        | Expr::EmptyRecordBuilder(loc_subexpr)
        | Expr::SingleFieldRecordBuilder(loc_subexpr)
        | Expr::OptionalFieldInRecordBuilder(_, loc_subexpr) => {
            expr_is_multiline(&loc_subexpr.value, comments_only)
        }

        Expr::ParensAround(subexpr) => expr_is_multiline(subexpr, comments_only),

        Expr::Closure(loc_patterns, loc_body) => {
            // check the body first because it's more likely to be multiline
            expr_is_multiline(&loc_body.value, comments_only)
                || loc_patterns
                    .iter()
                    .any(|loc_pattern| loc_pattern.value.is_multiline())
        }
        Expr::Backpassing(loc_patterns, loc_body, loc_ret) => {
            // check the body first because it's more likely to be multiline
            expr_is_multiline(&loc_body.value, comments_only)
                || expr_is_multiline(&loc_ret.value, comments_only)
                || loc_patterns
                    .iter()
                    .any(|loc_pattern| loc_pattern.value.is_multiline())
        }

        Expr::Record(fields) => is_collection_multiline(fields),
        Expr::Tuple(fields) => is_collection_multiline(fields),
        Expr::RecordUpdate { fields, .. } => is_collection_multiline(fields),
        Expr::RecordBuilder { fields, .. } => is_collection_multiline(fields),
    }
}

fn lower<'a, 'b: 'a>(arena: &'b Bump, lifted: Spaces<'b, Expr<'b>>) -> Expr<'b> {
    if lifted.before.is_empty() && lifted.after.is_empty() {
        return lifted.item;
    }
    if lifted.before.is_empty() {
        return Expr::SpaceAfter(arena.alloc(lifted.item), lifted.after);
    }
    if lifted.after.is_empty() {
        return Expr::SpaceBefore(arena.alloc(lifted.item), lifted.before);
    }
    Expr::SpaceBefore(
        arena.alloc(Expr::SpaceAfter(arena.alloc(lifted.item), lifted.after)),
        lifted.before,
    )
}

fn fmt_expr_collection(
    buf: &mut Buf<'_>,

    indent: u16,
    braces: Braces,
    items: Collection<'_, &Loc<Expr<'_>>>,
    newlines: Newlines,
) {
    let arena = buf.text.bump();
    let mut new_items: Vec<'_, &Expr<'_>> = Vec::with_capacity_in(items.len(), arena);

    let mut last_after: &[CommentOrNewline<'_>] = &[];

    for item in items.items {
        let mut lifted = expr_lift_spaces(Parens::InCollection, arena, &item.value);
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

fn requires_space_after_unary(item: &Expr<'_>) -> bool {
    match item {
        Expr::AccessorFunction(_) | Expr::UnaryOp(..) => true,
        Expr::Num(text) | Expr::Float(text) => text.starts_with('-'),
        Expr::NonBase10Int {
            string: _,
            base: _,
            is_negative,
        } => *is_negative,
        Expr::RecordUpdater(..) => true,
        Expr::RecordAccess(inner, _field) => requires_space_after_unary(inner),
        Expr::Apply(inner, _, _) => requires_space_after_unary(&inner.value),
        Expr::TrySuffix { target: _, expr } => requires_space_after_unary(expr),
        Expr::SpaceAfter(inner, _) | Expr::SpaceBefore(inner, _) => {
            requires_space_after_unary(inner)
        }
        _ => false,
    }
}

fn fmt_apply(
    loc_expr: &Loc<Expr<'_>>,
    loc_args: &[&Loc<Expr<'_>>],

    indent: u16,
    buf: &mut Buf<'_>,
) {
    // should_reflow_outdentable, aka should we transform this:
    //
    // ```
    // foo bar
    //   [
    //     1,
    //     2,
    //   ]
    // ```
    //
    // Into this:
    //
    // ```
    // foo bar [
    //   1,
    //   2,
    // ]
    // ```
    let should_reflow_outdentable = loc_expr.extract_spaces().after.is_empty()
        && except_last(loc_args).all(|a| !a.is_multiline())
        && loc_args
            .last()
            .map(|a| {
                a.extract_spaces().item.is_multiline()
                    && is_outdentable_collection(&a.value.extract_spaces().item)
                    && (a.extract_spaces().before == [CommentOrNewline::Newline]
                        || a.extract_spaces().before.is_empty())
            })
            .unwrap_or_default();

    let needs_indent = !should_reflow_outdentable
        && (!loc_expr.extract_spaces().after.is_empty()
            || except_last(loc_args).any(|a| a.is_multiline())
            || loc_expr.is_multiline()
            || loc_args
                .last()
                .map(|a| {
                    a.is_multiline()
                        && (!a.extract_spaces().before.is_empty() || !is_outdentable(&a.value))
                })
                .unwrap_or_default());

    let arg_indent = if needs_indent {
        indent + INDENT
    } else {
        indent
    };

    let expr = expr_lift_spaces(Parens::InApply, buf.text.bump(), &loc_expr.value);

    if !expr.before.is_empty() {
        format_spaces(buf, expr.before, Newlines::Yes, indent);
    }

    expr.item
        .format_with_options(buf, Parens::InApply, Newlines::Yes, indent);

    let mut last_after = expr.after;

    for (i, loc_arg) in loc_args.iter().enumerate() {
        let is_last_arg = i == loc_args.len() - 1;

        let arg = expr_lift_spaces(
            if is_last_arg {
                Parens::InApplyLastArg
            } else {
                Parens::InApply
            },
            buf.text.bump(),
            &loc_arg.value,
        );

        if !should_reflow_outdentable {
            if !last_after.is_empty() {
                format_spaces(buf, last_after, Newlines::Yes, arg_indent);
            }
            if !arg.before.is_empty() {
                format_spaces(buf, arg.before, Newlines::Yes, arg_indent);
            }
        }

        last_after = arg.after;

        if should_reflow_outdentable {
            buf.spaces(1);

            // Ignore any comments+newlines before/after.
            // We checked above that there's only a single newline before the last arg,
            // which we're intentionally ignoring.

            format_expr_only(&arg.item, buf, Parens::InApply, Newlines::Yes, arg_indent);
        } else if needs_indent {
            buf.ensure_ends_with_newline();
            format_expr_only(&arg.item, buf, Parens::InApply, Newlines::Yes, arg_indent);
        } else {
            buf.spaces(1);
            format_expr_only(&arg.item, buf, Parens::InApply, Newlines::Yes, arg_indent);
        }
    }

    if !last_after.is_empty() {
        format_spaces(buf, last_after, Newlines::Yes, arg_indent);
    }
}

fn is_outdentable_collection(expr: &Expr<'_>) -> bool {
    match expr {
        Expr::Tuple(items) => is_collection_multiline(items),
        Expr::List(items) => is_collection_multiline(items),
        Expr::Record(items) => is_collection_multiline(items),
        _ => false,
    }
}

fn fmt_parens(sub_expr: &Expr<'_>, buf: &mut Buf<'_>, indent: u16) {
    let should_add_newlines = match sub_expr {
        Expr::Closure(..) | Expr::SpaceBefore(..) | Expr::SpaceAfter(Expr::Closure(..), ..) => {
            false
        }
        _ => sub_expr.is_multiline(),
    };

    buf.indent(indent);
    buf.push('(');
    if should_add_newlines {
        buf.newline();
    }

    let next_indent = if starts_with_newline(sub_expr) || should_add_newlines {
        match sub_expr {
            Expr::Closure(..) | Expr::SpaceAfter(Expr::Closure(..), ..) => indent,
            _ => indent + INDENT,
        }
    } else {
        indent
    };

    sub_expr.format_with_options(buf, Parens::NotNeeded, Newlines::Yes, next_indent);

    if !matches!(sub_expr, Expr::SpaceAfter(..)) && should_add_newlines {
        buf.newline();
    }
    buf.indent(indent);
    buf.push(')');
}

pub fn is_str_multiline(literal: &StrLiteral) -> bool {
    use roc_parse::ast::StrLiteral::*;

    match literal {
        PlainLine(string) => {
            // When a PlainLine contains '\n' or '"', format as a block string
            string.contains('"') || string.contains('\n')
        }
        Line(_) => {
            // If this had any newlines, it'd have parsed as Block.
            false
        }
        Block(_) => {
            // Block strings are always formatted on multiple lines,
            // even if the string is only a single line.
            true
        }
    }
}

fn needs_unicode_escape(ch: char) -> bool {
    matches!(ch, '\u{0000}'..='\u{001f}' | '\u{007f}'..='\u{009f}')
}

pub(crate) fn format_sq_literal(buf: &mut Buf, s: &str) {
    buf.push('\'');
    for c in s.chars() {
        if c == '"' {
            buf.push_char_literal('"')
        } else {
            match c {
                '"' => buf.push_str("\""),
                '\'' => buf.push_str("\\\'"),
                '\t' => buf.push_str("\\t"),
                '\r' => buf.push_str("\\r"),
                '\n' => buf.push_str("\\n"),
                '\\' => buf.push_str("\\\\"),
                _ => {
                    if needs_unicode_escape(c) {
                        buf.push_str(&format!("\\u({:x})", c as u32))
                    } else {
                        buf.push_char_literal(c)
                    }
                }
            }
        }
    }
    buf.push('\'');
}

fn is_outdentable(expr: &Expr) -> bool {
    matches!(
        expr.extract_spaces().item,
        Expr::Tuple(_) | Expr::List(_) | Expr::Record(_) | Expr::Closure(..)
    )
}

fn starts_with_newline(expr: &Expr) -> bool {
    use roc_parse::ast::Expr::*;

    match expr {
        SpaceBefore(_, comment_or_newline) => {
            matches!(comment_or_newline.first(), Some(CommentOrNewline::Newline))
        }
        _ => false,
    }
}

fn fmt_str_body(body: &str, buf: &mut Buf) {
    for c in body.chars() {
        match c {
            // Format blank characters as unicode escapes
            '\u{200a}' => buf.push_str("\\u(200a)"),
            '\u{200b}' => buf.push_str("\\u(200b)"),
            '\u{200c}' => buf.push_str("\\u(200c)"),
            '\u{feff}' => buf.push_str("\\u(feff)"),
            // Don't change anything else in the string
            ' ' => buf.push_str_allow_spaces(" "),
            '\n' => buf.push_str_allow_spaces("\n"),
            _ => buf.push(c),
        }
    }
}

fn format_str_segment(seg: &StrSegment, buf: &mut Buf) {
    use StrSegment::*;

    match seg {
        Plaintext(string) => {
            // Lines in block strings will end with Plaintext ending in "\n" to indicate
            // a line break in the input string
            match string.strip_suffix('\n') {
                Some(string_without_newline) => {
                    fmt_str_body(string_without_newline, buf);
                    buf.newline();
                }
                None => fmt_str_body(string, buf),
            }
        }
        Unicode(loc_str) => {
            buf.push_str("\\u(");
            buf.push_str(loc_str.value); // e.g. "00A0" in "\u(00A0)"
            buf.push(')');
        }
        EscapedChar(escaped) => {
            buf.push('\\');
            buf.push(escaped.to_parsed_char());
        }
        Interpolated(loc_expr) => {
            buf.push_str("$(");
            // e.g. (name) in "Hi, $(name)!"
            let min_indent = buf.cur_line_indent() + INDENT;
            loc_expr.value.format_with_options(
                buf,
                Parens::NotNeeded, // We already printed parens!
                Newlines::No,      // Interpolations can never have newlines
                min_indent,
            );
            buf.push(')');
        }
    }
}

fn push_op(buf: &mut Buf, op: BinOp) {
    match op {
        called_via::BinOp::Caret => buf.push('^'),
        called_via::BinOp::Star => buf.push('*'),
        called_via::BinOp::Slash => buf.push('/'),
        called_via::BinOp::DoubleSlash => buf.push_str("//"),
        called_via::BinOp::Percent => buf.push('%'),
        called_via::BinOp::Plus => buf.push('+'),
        called_via::BinOp::Minus => buf.push('-'),
        called_via::BinOp::Equals => buf.push_str("=="),
        called_via::BinOp::NotEquals => buf.push_str("!="),
        called_via::BinOp::LessThan => buf.push('<'),
        called_via::BinOp::GreaterThan => buf.push('>'),
        called_via::BinOp::LessThanOrEq => buf.push_str("<="),
        called_via::BinOp::GreaterThanOrEq => buf.push_str(">="),
        called_via::BinOp::And => buf.push_str("&&"),
        called_via::BinOp::Or => buf.push_str("||"),
        called_via::BinOp::Pizza => buf.push_str("|>"),
    }
}

pub fn fmt_str_literal(buf: &mut Buf, literal: StrLiteral, indent: u16) {
    use roc_parse::ast::StrLiteral::*;

    match literal {
        PlainLine(string) => {
            // When a PlainLine contains '\n' or '"', format as a block string
            if string.contains('"') || string.contains('\n') {
                buf.ensure_ends_with_newline();
                buf.indent(indent);
                buf.push_str("\"\"\"");
                buf.push_newline_literal();
                for line in string.split('\n') {
                    buf.indent(indent);
                    fmt_str_body(line, buf);
                    buf.push_newline_literal();
                }
                buf.indent(indent);
                buf.push_str("\"\"\"");
            } else {
                buf.indent(indent);
                buf.push('"');
                fmt_str_body(string, buf);
                buf.push('"');
            };
        }
        Line(segments) => {
            buf.indent(indent);
            buf.push('"');
            for seg in segments.iter() {
                format_str_segment(seg, buf)
            }
            buf.push('"');
        }
        Block(lines) => {
            // Block strings will always be formatted with """ on new lines
            buf.ensure_ends_with_newline();
            buf.indent(indent);
            buf.push_str("\"\"\"");
            buf.push_newline_literal();

            for segments in lines.iter() {
                for seg in segments.iter() {
                    // only add indent if the line isn't empty
                    if *seg != StrSegment::Plaintext("\n") {
                        buf.indent(indent);
                        format_str_segment(seg, buf);
                    } else {
                        buf.push_newline_literal();
                    }
                }

                buf.push_newline_literal();
            }
            buf.indent(indent);
            buf.push_str("\"\"\"");
        }
    }
}

pub fn expr_lift_and_lower<'a, 'b: 'a>(
    _parens: Parens,
    arena: &'a Bump,
    expr: &Expr<'b>,
) -> Expr<'a> {
    lower(arena, expr_lift_spaces(Parens::NotNeeded, arena, expr))
}

pub fn expr_lift_spaces<'a, 'b: 'a>(
    parens: Parens,
    arena: &'a Bump,
    expr: &Expr<'b>,
) -> Spaces<'a, Expr<'a>> {
    match expr {
        Expr::Apply(func, args, called_via) => {
            if args.is_empty() {
                return expr_lift_spaces(Parens::NotNeeded, arena, &func.value);
            }

            let func_lifted = expr_lift_spaces(Parens::InApply, arena, &func.value);
            let args = arena.alloc_slice_copy(args);
            let mut res = if let Some(last) = args.last_mut() {
                let last_lifted = expr_lift_spaces(Parens::InApplyLastArg, arena, &last.value);
                if last_lifted.before.is_empty() {
                    *last = arena.alloc(Loc::at(last.region, last_lifted.item));
                } else {
                    *last = arena.alloc(Loc::at(
                        last.region,
                        Expr::SpaceBefore(arena.alloc(last_lifted.item), last_lifted.before),
                    ));
                }

                let func_fixed = if func_lifted.after.is_empty() {
                    func_lifted.item
                } else {
                    Expr::SpaceAfter(arena.alloc(func_lifted.item), func_lifted.after)
                };

                Spaces {
                    before: func_lifted.before,
                    item: Expr::Apply(
                        arena.alloc(Loc::at(func.region, func_fixed)),
                        args,
                        *called_via,
                    ),
                    after: last_lifted.after,
                }
            } else {
                Spaces {
                    before: func_lifted.before,
                    item: Expr::Apply(
                        arena.alloc(Loc::at(func.region, func_lifted.item)),
                        args,
                        *called_via,
                    ),
                    after: func_lifted.after,
                }
            };

            if parens == Parens::InApply || parens == Parens::InApplyLastArg {
                res = Spaces {
                    before: &[],
                    item: Expr::ParensAround(arena.alloc(lower(arena, res))),
                    after: &[],
                };
            }
            res
        }
        Expr::Defs(defs, final_expr) => {
            let mut defs = (*defs).clone();
            let mut before: &[CommentOrNewline] = &[];
            if let Some(spaces_range) = defs.space_before.first_mut() {
                if !spaces_range.is_empty() {
                    before = &defs.spaces[spaces_range.indices()];
                    *spaces_range = Slice::empty();
                }
            }

            let inner_before = match defs.tags[0].split() {
                Ok(_td) => &[],
                Err(vd) => {
                    let lifted = valdef_lift_spaces_before(arena, defs.value_defs[vd.index()]);
                    defs.value_defs[vd.index()] = lifted.item;
                    lifted.before
                }
            };

            let final_expr_lifted = expr_lift_spaces(Parens::NotNeeded, arena, &final_expr.value);

            let new_final_expr = if final_expr_lifted.before.is_empty() {
                final_expr_lifted.item
            } else {
                Expr::SpaceBefore(
                    arena.alloc(final_expr_lifted.item),
                    final_expr_lifted.before,
                )
            };

            let before = merge_spaces(arena, arena.alloc_slice_copy(before), inner_before);

            let mut item = Expr::Defs(
                arena.alloc(defs),
                arena.alloc(Loc::at(final_expr.region, new_final_expr)),
            );

            if parens == Parens::InCollection {
                item = Expr::ParensAround(arena.alloc(item));
            }

            Spaces {
                before,
                item,
                after: final_expr_lifted.after,
            }
        }
        Expr::Closure(pats, body) => {
            if parens == Parens::InApply {
                return Spaces {
                    before: &[],
                    item: Expr::ParensAround(arena.alloc(*expr)),
                    after: &[],
                };
            }
            let body_lifted = expr_lift_spaces_after(Parens::NotNeeded, arena, &body.value);

            Spaces {
                before: &[],
                item: Expr::Closure(pats, arena.alloc(Loc::at(body.region, body_lifted.item))),
                after: body_lifted.after,
            }
        }
        Expr::If { .. } | Expr::When(_, _) | Expr::Return(_, _) => {
            if parens == Parens::InApply || parens == Parens::InApplyLastArg {
                Spaces {
                    before: &[],
                    item: Expr::ParensAround(arena.alloc(*expr)),
                    after: &[],
                }
            } else {
                Spaces {
                    before: &[],
                    item: *expr,
                    after: &[],
                }
            }
        }
        Expr::Backpassing(pats, call, continuation) => {
            let pats = arena.alloc_slice_copy(pats);
            let before = if let Some(first) = pats.first_mut() {
                let lifted = pattern_lift_spaces_before(arena, &first.value);
                *first = Loc::at(first.region, lifted.item);
                lifted.before
            } else {
                &[]
            };
            let continuation_lifted =
                expr_lift_spaces_after(Parens::NotNeeded, arena, &continuation.value);

            let mut res = Spaces {
                before,
                item: Expr::Backpassing(
                    pats,
                    call,
                    arena.alloc(Loc::at(continuation.region, continuation_lifted.item)),
                ),
                after: continuation_lifted.after,
            };

            if parens == Parens::InApply || parens == Parens::InApplyLastArg {
                res = Spaces {
                    before: &[],
                    item: Expr::ParensAround(arena.alloc(lower(arena, res))),
                    after: &[],
                };
            }
            res
        }
        Expr::SpaceBefore(expr, spaces) => {
            let mut inner = expr_lift_spaces(parens, arena, expr);
            inner.before = merge_spaces_conservative(arena, spaces, inner.before);
            inner
        }
        Expr::SpaceAfter(expr, spaces) => {
            let mut inner = expr_lift_spaces(parens, arena, expr);
            inner.after = merge_spaces_conservative(arena, inner.after, spaces);
            inner
        }
        Expr::ParensAround(inner) => {
            if (parens == Parens::NotNeeded || parens == Parens::InCollection)
                && !sub_expr_requests_parens(inner)
            {
                expr_lift_spaces(Parens::NotNeeded, arena, inner)
            } else {
                Spaces {
                    before: &[],
                    item: *expr,
                    after: &[],
                }
            }
        }
        Expr::Float(_)
        | Expr::Num(_)
        | Expr::NonBase10Int { .. }
        | Expr::Str(_)
        | Expr::SingleQuote(_)
        | Expr::AccessorFunction(_)
        | Expr::RecordUpdater(_)
        | Expr::RecordAccess(_, _)
        | Expr::TupleAccess(_, _)
        | Expr::Var { .. }
        | Expr::Underscore(_)
        | Expr::Crash
        | Expr::Tag(_)
        | Expr::OpaqueRef(_)
        | Expr::Dbg
        | Expr::Try
        | Expr::List(_)
        | Expr::Record(_)
        | Expr::Tuple(_)
        | Expr::RecordBuilder { .. }
        | Expr::RecordUpdate { .. } => Spaces {
            before: &[],
            item: *expr,
            after: &[],
        },

        Expr::TrySuffix { target, expr } => {
            let expr_lifted = expr_lift_spaces_after(Parens::InApply, arena, expr);

            Spaces {
                before: &[],
                item: Expr::TrySuffix {
                    target: *target,
                    expr: arena.alloc(expr_lifted.item),
                },
                after: expr_lifted.after,
            }
        }
        Expr::DbgStmt {
            first,
            extra_args,
            continuation,
        } => {
            let continuation_lifted =
                expr_lift_spaces_after(Parens::NotNeeded, arena, &continuation.value);

            Spaces {
                before: &[],
                item: Expr::DbgStmt {
                    first,
                    extra_args,
                    continuation: arena
                        .alloc(Loc::at(continuation.region, continuation_lifted.item)),
                },
                after: continuation_lifted.after,
            }
        }
        Expr::LowLevelDbg(_, _, _) => {
            unreachable!("LowLevelDbg should only exist after desugaring, not during formatting")
        }
        Expr::LowLevelTry(..) => {
            unreachable!("LowLevelTry should only exist after desugaring, not during formatting")
        }
        Expr::BinOps(lefts, right) => {
            let lefts = arena.alloc_slice_copy(lefts);

            let before = if let Some(first) = lefts.first_mut() {
                let lifted = expr_lift_spaces_before(Parens::InOperator, arena, &first.0.value);
                *first = (Loc::at(first.0.region, lifted.item), first.1);
                lifted.before
            } else {
                &[]
            };

            let right_lifted = expr_lift_spaces_after(Parens::InOperator, arena, &right.value);

            Spaces {
                before,
                item: Expr::BinOps(lefts, arena.alloc(Loc::at(right.region, right_lifted.item))),
                after: right_lifted.after,
            }
        }
        Expr::UnaryOp(expr, op) => {
            let expr_lifted = expr_lift_spaces_after(Parens::InOperator, arena, &expr.value);

            Spaces {
                before: &[],
                item: Expr::UnaryOp(arena.alloc(Loc::at(expr.region, expr_lifted.item)), *op),
                after: expr_lifted.after,
            }
        }

        Expr::MalformedIdent(_, _)
        | Expr::MalformedSuffixed(_)
        | Expr::PrecedenceConflict(_)
        | Expr::EmptyRecordBuilder(_)
        | Expr::SingleFieldRecordBuilder(_)
        | Expr::OptionalFieldInRecordBuilder(_, _) => Spaces {
            before: &[],
            item: *expr,
            after: &[],
        }, // _ => Spaces {
           //     before: &[],
           //     item: *expr,
           //     after: &[],
           // },
    }
}

pub fn expr_lift_spaces_before<'a, 'b: 'a>(
    parens: Parens,
    arena: &'a Bump,
    expr: &Expr<'b>,
) -> SpacesBefore<'a, Expr<'a>> {
    let lifted = expr_lift_spaces(parens, arena, expr);
    SpacesBefore {
        before: lifted.before,
        item: lifted.item.maybe_after(arena, lifted.after),
    }
}

pub fn expr_lift_spaces_after<'a, 'b: 'a>(
    parens: Parens,
    arena: &'a Bump,
    expr: &Expr<'b>,
) -> SpacesAfter<'a, Expr<'a>> {
    let lifted = expr_lift_spaces(parens, arena, expr);
    SpacesAfter {
        item: lifted.item.maybe_before(arena, lifted.before),
        after: lifted.after,
    }
}

pub fn merge_spaces_conservative<'a>(
    arena: &'a Bump,
    a: &'a [CommentOrNewline<'a>],
    b: &'a [CommentOrNewline<'a>],
) -> &'a [CommentOrNewline<'a>] {
    if a.is_empty() {
        b
    } else if b.is_empty() {
        a
    } else {
        let mut merged = Vec::with_capacity_in(a.len() + b.len(), arena);
        merged.extend_from_slice(a);
        let mut it = b.iter();
        for item in it.by_ref() {
            if item.is_comment() {
                merged.push(*item);
                break;
            }
        }
        merged.extend(it);
        merged.into_bump_slice()
    }
}

fn fmt_binops<'a>(
    buf: &mut Buf,
    lefts: &'a [(Loc<Expr<'a>>, Loc<BinOp>)],
    loc_right_side: &'a Loc<Expr<'a>>,

    indent: u16,
) {
    let is_multiline = loc_right_side.value.is_multiline()
        || lefts.iter().any(|(expr, _)| expr.value.is_multiline());

    for (loc_left_side, loc_binop) in lefts {
        let binop = loc_binop.value;

        let lifted_left_side =
            expr_lift_spaces(Parens::InOperator, buf.text.bump(), &loc_left_side.value);
        format_spaces(buf, lifted_left_side.before, Newlines::Yes, indent);

        let need_parens = matches!(lifted_left_side.item, Expr::BinOps(..))
            || starts_with_unary_minus(lifted_left_side.item);

        if need_parens {
            fmt_parens(&lifted_left_side.item, buf, indent);
        } else {
            lifted_left_side.item.format_with_options(
                buf,
                Parens::InOperator,
                Newlines::Yes,
                indent,
            );
        }

        format_spaces(buf, lifted_left_side.after, Newlines::Yes, indent);

        if is_multiline {
            buf.ensure_ends_with_newline();
            buf.indent(indent);
        } else {
            buf.spaces(1);
        }

        push_op(buf, binop);

        buf.spaces(1);
    }

    let lifted_right_side =
        expr_lift_spaces(Parens::InOperator, buf.text.bump(), &loc_right_side.value);
    format_spaces(buf, lifted_right_side.before, Newlines::Yes, indent);

    let need_parens = matches!(lifted_right_side.item, Expr::BinOps(..))
        || starts_with_unary_minus(lifted_right_side.item);

    if need_parens {
        fmt_parens(&lifted_right_side.item, buf, indent);
    } else {
        lifted_right_side
            .item
            .format_with_options(buf, Parens::InOperator, Newlines::Yes, indent);
    }

    format_spaces(buf, lifted_right_side.after, Newlines::Yes, indent);
}

fn starts_with_unary_minus(item: Expr<'_>) -> bool {
    match item {
        Expr::UnaryOp(
            _,
            Loc {
                value: UnaryOp::Negate,
                ..
            },
        ) => true,
        Expr::SpaceAfter(expr, _) | Expr::SpaceBefore(expr, _) => starts_with_unary_minus(*expr),
        Expr::Apply(expr, _args, _) => starts_with_unary_minus(expr.value),
        Expr::BinOps(lefts, _right) => lefts
            .first()
            .map_or(false, |(expr, _)| starts_with_unary_minus(expr.value)),
        _ => false,
    }
}

pub fn format_spaces(buf: &mut Buf, spaces: &[CommentOrNewline], newlines: Newlines, indent: u16) {
    match newlines {
        Newlines::Yes => {
            fmt_spaces(buf, spaces.iter(), indent);
        }
        Newlines::No => {
            fmt_comments_only(buf, spaces.iter(), NewlineAt::Bottom, indent);
        }
    }
}

fn is_when_patterns_multiline(when_branch: &WhenBranch) -> bool {
    let patterns = when_branch.patterns;
    let (first_pattern, rest) = patterns.split_first().unwrap();

    let is_multiline_patterns = if let Some((last_pattern, inner_patterns)) = rest.split_last() {
        !first_pattern.value.extract_spaces().after.is_empty()
            || !last_pattern.value.extract_spaces().before.is_empty()
            || inner_patterns.iter().any(|p| {
                let spaces = p.value.extract_spaces();
                !spaces.before.is_empty() || !spaces.after.is_empty()
            })
    } else {
        false
    };

    is_multiline_patterns
}

fn fmt_if_or_when_condition<'a>(buf: &mut Buf, loc_condition: &'a Loc<Expr<'a>>, indent: u16) {
    let is_multiline_condition = loc_condition.is_multiline();

    if is_multiline_condition {
        let condition = expr_lift_spaces(Parens::NotNeeded, buf.text.bump(), &loc_condition.value);
        fmt_comments_only(
            buf,
            condition.before.iter(),
            NewlineAt::Both,
            indent + INDENT,
        );
        buf.ensure_ends_with_newline();
        condition.item.format(buf, indent + INDENT);
        if condition.after.iter().any(|s| s.is_newline()) {
            buf.ensure_ends_with_newline();
        }
        fmt_comments_only(
            buf,
            condition.after.iter(),
            NewlineAt::Bottom,
            indent + INDENT,
        );
        buf.ensure_ends_with_newline();
        buf.indent(indent);
    } else {
        buf.spaces(1);
        loc_condition.format_with_options(buf, Parens::NotNeeded, Newlines::Yes, indent);
        buf.spaces(1);
    }
}

fn fmt_when<'a>(
    buf: &mut Buf,
    loc_condition: &'a Loc<Expr<'a>>,
    branches: &[&'a WhenBranch<'a>],

    indent: u16,
) {
    buf.ensure_ends_with_newline();
    buf.indent(indent);
    buf.push_str("when");
    fmt_if_or_when_condition(buf, loc_condition, indent);
    buf.push_str("is");
    buf.newline();

    let mut prev_branch_was_multiline = false;

    for (branch_index, branch) in branches.iter().enumerate() {
        let expr = &branch.value;
        let patterns = &branch.patterns;
        let is_multiline_expr = expr.is_multiline();
        let is_multiline_patterns = is_when_patterns_multiline(branch);

        for (pattern_index, pattern) in patterns.iter().enumerate() {
            if pattern_index == 0 {
                match &pattern.value {
                    Pattern::SpaceBefore(sub_pattern, spaces) => {
                        let added_blank_line;

                        if branch_index > 0 // Never render newlines before the first branch.
                            && matches!(spaces.first(), Some(CommentOrNewline::Newline))
                        {
                            if prev_branch_was_multiline {
                                // Multiline branches always get a full blank line after them.
                                buf.ensure_ends_with_blank_line();
                                added_blank_line = true;
                            } else {
                                buf.ensure_ends_with_newline();
                                added_blank_line = false;
                            }
                        } else {
                            added_blank_line = false;
                        }

                        // Write comments (which may have been attached to the previous
                        // branch's expr, if there was a previous branch).
                        fmt_comments_only(buf, spaces.iter(), NewlineAt::Bottom, indent + INDENT);

                        if branch_index > 0 {
                            if prev_branch_was_multiline && !added_blank_line {
                                // Multiline branches always get a full blank line after them
                                // (which we may already have added before a comment).
                                buf.ensure_ends_with_blank_line();
                            } else {
                                buf.ensure_ends_with_newline();
                            }
                        }

                        fmt_pattern(buf, sub_pattern, indent + INDENT, Parens::NotNeeded);
                    }
                    other => {
                        if branch_index > 0 {
                            if prev_branch_was_multiline {
                                // Multiline branches always get a full blank line after them.
                                buf.ensure_ends_with_blank_line();
                            } else {
                                buf.ensure_ends_with_newline();
                            }
                        }

                        fmt_pattern(buf, other, indent + INDENT, Parens::NotNeeded);
                    }
                }
            } else {
                if is_multiline_patterns {
                    buf.ensure_ends_with_newline();
                    buf.indent(indent + INDENT);
                    buf.push('|');
                } else {
                    buf.push_str(" |");
                }

                buf.spaces(1);

                fmt_pattern(buf, &pattern.value, indent + INDENT, Parens::NotNeeded);
            }
        }

        if let Some(guard_expr) = &branch.guard {
            buf.push_str(" if");
            buf.spaces(1);
            guard_expr.format_with_options(buf, Parens::NotNeeded, Newlines::Yes, indent + INDENT);
        }

        buf.indent(indent + INDENT);
        buf.push_str(" ->");

        match expr.value {
            Expr::SpaceBefore(nested, spaces) => {
                fmt_spaces_no_blank_lines(buf, spaces.iter(), indent + (INDENT * 2));

                if is_multiline_expr {
                    buf.ensure_ends_with_newline();
                } else {
                    buf.spaces(1);
                }

                nested.format_with_options(
                    buf,
                    Parens::NotNeeded,
                    Newlines::Yes,
                    indent + 2 * INDENT,
                );
            }
            _ => {
                if is_multiline_expr {
                    buf.ensure_ends_with_newline();
                } else {
                    buf.spaces(1);
                }

                expr.format_with_options(
                    buf,
                    Parens::NotNeeded,
                    Newlines::Yes,
                    indent + 2 * INDENT,
                );
            }
        }

        prev_branch_was_multiline = is_multiline_expr || is_multiline_patterns;
    }
}

fn fmt_dbg_stmt<'a>(
    buf: &mut Buf,
    condition: &'a Loc<Expr<'a>>,
    extra_args: &'a [&'a Loc<Expr<'a>>],
    continuation: &'a Loc<Expr<'a>>,
    parens: Parens,

    indent: u16,
) {
    let mut args = Vec::with_capacity_in(extra_args.len() + 1, buf.text.bump());
    args.push(condition);
    args.extend_from_slice(extra_args);

    Expr::Apply(
        &Loc::at_zero(Expr::Dbg),
        args.into_bump_slice(),
        called_via::CalledVia::Space,
    )
    .format_with_options(buf, parens, Newlines::Yes, indent);

    let cont_lifted = expr_lift_spaces(Parens::NotNeeded, buf.text.bump(), &continuation.value);

    if !cont_lifted.before.is_empty() {
        format_spaces(buf, cont_lifted.before, Newlines::Yes, indent);
    }

    // Always put a newline after the `dbg` line(s)
    buf.ensure_ends_with_newline();

    format_expr_only(
        &cont_lifted.item,
        buf,
        Parens::NotNeeded,
        Newlines::Yes,
        indent,
    );

    if !cont_lifted.after.is_empty() {
        format_spaces(buf, cont_lifted.after, Newlines::Yes, indent);
    }
}

fn fmt_return<'a>(
    buf: &mut Buf,
    return_value: &'a Loc<Expr<'a>>,
    after_return: &Option<&'a Loc<Expr<'a>>>,
    parens: Parens,
    newlines: Newlines,

    indent: u16,
) {
    buf.ensure_ends_with_newline();
    buf.indent(indent);
    buf.push_str(keyword::RETURN);

    if matches!(return_value.value.extract_spaces().item, Expr::Defs(..)) {
        buf.ensure_ends_with_newline();
    } else {
        buf.spaces(1);
    }

    let return_indent = if return_value.is_multiline() {
        indent + INDENT
    } else {
        indent
    };

    return_value.format_with_options(buf, parens, Newlines::No, return_indent);

    if let Some(after_return) = after_return {
        let lifted = expr_lift_spaces(Parens::NotNeeded, buf.text.bump(), &after_return.value);
        if lifted.before.is_empty() {
            buf.ensure_ends_with_newline();
        } else {
            fmt_spaces(buf, lifted.before.iter(), indent);
        }
        lifted
            .item
            .format_with_options(buf, parens, newlines, indent);
        fmt_spaces(buf, lifted.after.iter(), indent);
    } else if parens != Parens::NotNeeded {
        buf.ensure_ends_with_newline();
    }
}

fn fmt_if<'a>(
    buf: &mut Buf,
    branches: &'a [(Loc<Expr<'a>>, Loc<Expr<'a>>)],
    final_else: &'a Loc<Expr<'a>>,
    is_multiline: bool,
    indented_else: bool,

    indent: u16,
) {
    //    let is_multiline_then = loc_then.is_multiline();
    //    let is_multiline_else = final_else.is_multiline();
    //    let is_multiline_condition = loc_condition.is_multiline();
    //    let is_multiline = is_multiline_then || is_multiline_else || is_multiline_condition;

    let return_indent = if is_multiline {
        indent + INDENT
    } else {
        indent
    };

    for (i, (loc_condition, loc_then)) in branches.iter().enumerate() {
        buf.indent(indent);

        if i > 0 {
            buf.push_str("else");
            buf.spaces(1);
        }

        buf.push_str("if");
        fmt_if_or_when_condition(buf, loc_condition, indent);
        buf.push_str("then");

        if is_multiline {
            let then = expr_lift_spaces(Parens::NotNeeded, buf.text.bump(), &loc_then.value);
            fmt_comments_only(buf, then.before.iter(), NewlineAt::Both, return_indent);
            buf.ensure_ends_with_newline();
            then.item.format(buf, return_indent);
            if then.after.iter().any(|s| s.is_newline()) {
                buf.ensure_ends_with_newline();
            }
            fmt_comments_only(buf, then.after.iter(), NewlineAt::Bottom, return_indent);
            buf.ensure_ends_with_newline();
        } else {
            buf.push_str("");
            buf.spaces(1);
            loc_then.format(buf, return_indent);
        }
    }

    if indented_else {
        buf.indent(indent + INDENT);
        buf.push_str("else");
        buf.newline();
        buf.newline();
    } else if is_multiline {
        buf.indent(indent);
        buf.push_str("else");
        buf.newline();
    } else {
        buf.indent(indent);
        buf.push_str(" else");
        buf.spaces(1);
    }
    let indent = if indented_else { indent } else { return_indent };
    final_else.format(buf, indent);
}

fn fmt_closure<'a>(
    buf: &mut Buf,
    loc_patterns: &'a [Loc<Pattern<'a>>],
    loc_ret: &'a Loc<Expr<'a>>,

    indent: u16,
) {
    use self::Expr::*;

    buf.indent(indent);
    buf.push('\\');

    let arguments_are_multiline = loc_patterns
        .iter()
        .any(|loc_pattern| loc_pattern.is_multiline());

    // If the arguments are multiline, go down a line and indent.
    let indent = if arguments_are_multiline {
        indent + INDENT
    } else {
        indent
    };

    let mut first = true;

    for loc_pattern in loc_patterns.iter() {
        if !first {
            buf.indent(indent);
            if arguments_are_multiline {
                buf.push(',');
                buf.newline();
            } else {
                buf.push_str(",");
                buf.spaces(1);
            }
        }
        first = false;

        let arg = pattern_lift_spaces(buf.text.bump(), &loc_pattern.value);

        if !arg.before.is_empty() {
            fmt_comments_only(buf, arg.before.iter(), NewlineAt::Bottom, indent)
        }

        arg.item
            .format_with_options(buf, Parens::InAsPattern, Newlines::No, indent);

        if !arg.after.is_empty() {
            if starts_with_inline_comment(arg.after.iter()) {
                buf.spaces(1);
            }
            fmt_comments_only(buf, arg.after.iter(), NewlineAt::Bottom, indent)
        }
    }

    if arguments_are_multiline {
        buf.ensure_ends_with_newline();
        buf.indent(indent);
    } else {
        buf.spaces(1);
    }

    buf.push_str("->");

    let is_multiline = loc_ret.value.is_multiline();

    // If the body is multiline, go down a line and indent.
    let body_indent = if is_multiline {
        indent + INDENT
    } else {
        indent
    };

    // the body of the Closure can be on the same line, or
    // on a new line. If it's on the same line, insert a space.

    match &loc_ret.value {
        SpaceBefore(_, _) => {
            // the body starts with (first comment and then) a newline
            // do nothing
        }
        _ => {
            // add a space after the `->`
            buf.spaces(1);
        }
    };

    if is_multiline {
        match &loc_ret.value {
            SpaceBefore(sub_expr, spaces) => {
                let should_outdent = match sub_expr {
                    Record { .. } | List { .. } => {
                        let is_only_newlines = spaces.iter().all(|s| s.is_newline());
                        is_only_newlines && sub_expr.is_multiline()
                    }
                    _ => false,
                };

                if should_outdent {
                    buf.spaces(1);
                    sub_expr.format_with_options(buf, Parens::NotNeeded, Newlines::Yes, indent);
                } else {
                    loc_ret.format_with_options(buf, Parens::NotNeeded, Newlines::Yes, body_indent);
                }
            }
            Record { .. } | List { .. } => {
                loc_ret.format_with_options(buf, Parens::NotNeeded, Newlines::Yes, indent);
            }
            _ => {
                loc_ret.format_with_options(buf, Parens::NotNeeded, Newlines::Yes, body_indent);
            }
        }
    } else {
        loc_ret.format_with_options(buf, Parens::NotNeeded, Newlines::Yes, body_indent);
    }
}

fn fmt_backpassing<'a>(
    buf: &mut Buf,
    loc_patterns: &'a [Loc<Pattern<'a>>],
    loc_body: &'a Loc<Expr<'a>>,
    loc_ret: &'a Loc<Expr<'a>>,

    outer_indent: u16,
) {
    use self::Expr::*;

    let arguments_are_multiline = loc_patterns
        .iter()
        .any(|loc_pattern| loc_pattern.is_multiline());

    // If the arguments are multiline, go down a line and indent.
    let arg_indent = if arguments_are_multiline {
        outer_indent + INDENT
    } else {
        outer_indent
    };

    let mut first = true;

    for loc_pattern in loc_patterns.iter() {
        let needs_parens = if pattern_needs_parens_when_backpassing(&loc_pattern.value) {
            Parens::InApply
        } else {
            Parens::NotNeeded
        };

        let pat = loc_pattern.value.extract_spaces();

        if !first {
            buf.indent(arg_indent);
            buf.push(',');
        }

        fmt_comments_only(buf, pat.before.iter(), NewlineAt::Bottom, arg_indent);

        if !first {
            if arguments_are_multiline {
                buf.ensure_ends_with_newline();
            } else {
                buf.spaces(1);
            }
        }

        pat.item.format_with_options(
            buf,
            needs_parens,
            Newlines::No,
            if first { outer_indent } else { arg_indent },
        );
        fmt_comments_only(buf, pat.after.iter(), NewlineAt::Bottom, arg_indent);

        first = false;
    }

    if arguments_are_multiline {
        buf.ensure_ends_with_newline();
        buf.indent(arg_indent);
    } else {
        buf.spaces(1);
    }

    buf.push_str("<-");

    let is_multiline = loc_ret.value.is_multiline();

    // If the body is multiline, go down a line and indent.
    let body_indent = if is_multiline {
        arg_indent + INDENT
    } else {
        arg_indent
    };

    // the body of the Backpass can be on the same line, or
    // on a new line. If it's on the same line, insert a space.

    match &loc_body.value {
        SpaceBefore(_, _) => {
            // the body starts with (first comment and then) a newline
            // do nothing
        }
        _ => {
            // add a space after the `<-`
            buf.spaces(1);
        }
    };

    loc_body.format_with_options(buf, Parens::NotNeeded, Newlines::Yes, body_indent);
    loc_ret.format_with_options(buf, Parens::NotNeeded, Newlines::Yes, outer_indent);
}

fn pattern_needs_parens_when_backpassing(pat: &Pattern) -> bool {
    match pat {
        Pattern::Apply(_, _) => true,
        Pattern::SpaceBefore(a, _) | Pattern::SpaceAfter(a, _) => {
            pattern_needs_parens_when_backpassing(a)
        }
        _ => false,
    }
}

enum RecordPrefix<'a> {
    Update(&'a Loc<Expr<'a>>),
    Mapper(&'a Loc<Expr<'a>>),
}

fn fmt_record_like<'a, 'b: 'a, Field, ToSpacesAround>(
    buf: &'a mut Buf,
    prefix: Option<RecordPrefix<'b>>,
    fields: Collection<'b, Loc<Field>>,
    indent: u16,
    to_space_around: ToSpacesAround,
) where
    Field: Formattable,
    ToSpacesAround: Fn(&'a Bump, &'b Field) -> Spaces<'a, Field>,
{
    let loc_fields = fields.items;
    let final_comments = fields.final_comments();
    buf.indent(indent);
    if loc_fields.is_empty() && final_comments.is_empty() && prefix.is_none() {
        buf.push_str("{}");
    } else {
        buf.push('{');

        match prefix {
            None => {}
            // We are presuming this to be a Var()
            // If it wasnt a Var() we would not have made
            // it this far. For example "{ 4 & hello = 9 }"
            // doesnt make sense.
            Some(RecordPrefix::Update(record_var)) => {
                buf.spaces(1);
                record_var.format(buf, indent);
                buf.indent(indent);
                buf.push_str(" &");
            }
            Some(RecordPrefix::Mapper(mapper_var)) => {
                buf.spaces(1);
                mapper_var.format(buf, indent);
                buf.indent(indent);
                buf.push_str(" <-");
            }
        }

        let is_multiline = loc_fields.iter().any(|loc_field| loc_field.is_multiline())
            || !final_comments.is_empty();

        if is_multiline {
            let field_indent = indent + INDENT;

            let mut last_after: &[CommentOrNewline<'_>] = &[];

            for (iter, field) in loc_fields.iter().enumerate() {
                // comma addition is handled by the `format_field_multiline` function
                // since we can have stuff like:
                // { x # comment
                // , y
                // }
                // In this case, we have to move the comma before the comment.

                let field_lifted = to_space_around(buf.text.bump(), &field.value);

                let before = merge_spaces(buf.text.bump(), last_after, field_lifted.before);

                if iter == 0 || count_leading_newlines(before.iter()) == 0 {
                    buf.ensure_ends_with_newline();
                }

                let newline_mode = if iter == 0 {
                    if loc_fields.len() == 1 {
                        SpacesNewlineMode::SkipNewlinesAtBoth
                    } else {
                        SpacesNewlineMode::SkipNewlinesAtStart
                    }
                } else {
                    SpacesNewlineMode::Normal
                };

                fmt_spaces_with_newline_mode(buf, before, field_indent, newline_mode);
                field_lifted.item.format_with_options(
                    buf,
                    Parens::NotNeeded,
                    Newlines::No,
                    field_indent,
                );
                buf.push_str(",");
                last_after = field_lifted.after;
            }

            let after = merge_spaces(buf.text.bump(), last_after, final_comments);

            if count_leading_newlines(after.iter()) == 0 {
                buf.ensure_ends_with_newline();
            }

            fmt_spaces_with_newline_mode(
                buf,
                after,
                field_indent,
                SpacesNewlineMode::SkipNewlinesAtEnd,
            );

            buf.ensure_ends_with_newline();
        } else {
            // is_multiline == false
            buf.spaces(1);
            let field_indent = indent;
            let mut iter = loc_fields.iter().peekable();
            while let Some(field) = iter.next() {
                field.format_with_options(buf, Parens::NotNeeded, Newlines::No, field_indent);

                if iter.peek().is_some() {
                    buf.push_str(",");
                    buf.spaces(1);
                }
            }
            buf.spaces(1);
            // if we are here, that means that `final_comments` is empty, thus we don't have
            // to add a comment. Anyway, it is not possible to have a single line record with
            // a comment in it.
        };

        // closes the initial bracket
        buf.indent(indent);
        buf.push('}');
    }
}

fn assigned_field_to_spaces<'a, 'b: 'a, T: Copy>(
    arena: &'a Bump,
    field: &'b AssignedField<'b, T>,
) -> Spaces<'a, AssignedField<'a, T>> {
    match field {
        AssignedField::SpaceBefore(sub_field, spaces) => {
            let mut inner = assigned_field_to_spaces(arena, sub_field);
            inner.before = merge_spaces(arena, spaces, inner.before);
            inner
        }
        AssignedField::SpaceAfter(sub_field, spaces) => {
            let mut inner = assigned_field_to_spaces(arena, sub_field);
            inner.after = merge_spaces(arena, inner.after, spaces);
            inner
        }
        _ => Spaces {
            before: &[],
            item: *field,
            after: &[],
        },
    }
}

pub fn sub_expr_requests_parens(expr: &Expr<'_>) -> bool {
    match expr {
        Expr::BinOps(left_side, _) => {
            left_side
                .iter()
                .any(|(_, loc_binop)| match loc_binop.value {
                    BinOp::Caret
                    | BinOp::Star
                    | BinOp::Slash
                    | BinOp::DoubleSlash
                    | BinOp::Percent
                    | BinOp::Plus
                    | BinOp::Minus
                    | BinOp::Equals
                    | BinOp::NotEquals
                    | BinOp::LessThan
                    | BinOp::GreaterThan
                    | BinOp::LessThanOrEq
                    | BinOp::GreaterThanOrEq
                    | BinOp::And
                    | BinOp::Or
                    | BinOp::Pizza => true,
                })
        }
        Expr::If { .. } => true,
        Expr::Defs(_, _) => true,
        Expr::Return(..) | Expr::Backpassing(..) | Expr::DbgStmt { .. } => {
            // This is because e.g. (return x)\nfoo would be de-parenthesized and cause the `after_return` to be `foo`.
            // That _is_ a semantic change technically right now, because that transform is done in the parser.
            // When that's moved to `can`, we can remove this
            true
        }
        Expr::SpaceBefore(e, _) => sub_expr_requests_parens(e),
        Expr::SpaceAfter(e, _) => sub_expr_requests_parens(e),
        _ => false,
    }
}
