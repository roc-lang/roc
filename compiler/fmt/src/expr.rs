use crate::annotation::{Formattable, Newlines, Parens};
use crate::def::fmt_def;
use crate::pattern::fmt_pattern;
use crate::spaces::{add_spaces, fmt_comments_only, fmt_spaces, newline, NewlineAt, INDENT};
use bumpalo::collections::String;
use roc_module::operator::{self, BinOp};
use roc_parse::ast::StrSegment;
use roc_parse::ast::{
    AssignedField, Base, Collection, CommentOrNewline, Expr, Pattern, WhenBranch,
};
use roc_region::all::Located;

impl<'a> Formattable<'a> for Expr<'a> {
    fn is_multiline(&self) -> bool {
        use roc_parse::ast::Expr::*;
        // TODO cache these answers using a Map<Pointer, bool>, so
        // we don't have to traverse subexpressions repeatedly

        match self {
            // Return whether these spaces contain any Newlines
            SpaceBefore(_sub_expr, spaces) | SpaceAfter(_sub_expr, spaces) => {
                debug_assert!(!spaces.is_empty());

                // "spaces" always contain either a newline or comment, and comments have newlines
                true
            }

            // These expressions never have newlines
            Float(_)
            | Num(_)
            | NonBase10Int { .. }
            | Access(_, _)
            | AccessorFunction(_)
            | Var { .. }
            | Underscore { .. }
            | MalformedIdent(_, _)
            | MalformedClosure
            | GlobalTag(_)
            | PrivateTag(_) => false,

            // These expressions always have newlines
            Defs(_, _) | When(_, _) => true,

            List(items) => items.iter().any(|loc_expr| loc_expr.is_multiline()),

            Str(literal) => {
                use roc_parse::ast::StrLiteral::*;

                match literal {
                    PlainLine(_) | Line(_) => {
                        // If this had any newlines, it'd have parsed as Block.
                        false
                    }
                    Block(lines) => {
                        // Block strings don't *have* to be multiline!
                        lines.len() > 1
                    }
                }
            }
            Apply(loc_expr, args, _) => {
                loc_expr.is_multiline() || args.iter().any(|loc_arg| loc_arg.is_multiline())
            }

            Expect(condition, continuation) => {
                condition.is_multiline() || continuation.is_multiline()
            }

            If(branches, final_else) => {
                final_else.is_multiline()
                    || branches
                        .iter()
                        .any(|(c, t)| c.is_multiline() || t.is_multiline())
            }

            BinOps(lefts, loc_right) => {
                lefts.iter().any(|(loc_expr, _)| loc_expr.is_multiline())
                    || loc_right.is_multiline()
            }

            UnaryOp(loc_subexpr, _)
            | PrecedenceConflict(roc_parse::ast::PrecedenceConflict {
                expr: loc_subexpr, ..
            }) => loc_subexpr.is_multiline(),

            ParensAround(subexpr) => subexpr.is_multiline(),

            Closure(loc_patterns, loc_body) => {
                // check the body first because it's more likely to be multiline
                loc_body.is_multiline()
                    || loc_patterns
                        .iter()
                        .any(|loc_pattern| loc_pattern.is_multiline())
            }
            Backpassing(loc_patterns, loc_body, loc_ret) => {
                // check the body first because it's more likely to be multiline
                loc_body.is_multiline()
                    || loc_ret.is_multiline()
                    || loc_patterns
                        .iter()
                        .any(|loc_pattern| loc_pattern.is_multiline())
            }

            Record(fields) => fields.iter().any(|loc_field| loc_field.is_multiline()),
            RecordUpdate { fields, .. } => fields.iter().any(|loc_field| loc_field.is_multiline()),
        }
    }

    fn format_with_options(
        &self,
        buf: &mut String<'a>,
        parens: Parens,
        newlines: Newlines,
        indent: u16,
    ) {
        use self::Expr::*;

        let format_newlines = newlines == Newlines::Yes;
        let apply_needs_parens = parens == Parens::InApply;

        match self {
            SpaceBefore(sub_expr, spaces) => {
                if format_newlines {
                    fmt_spaces(buf, spaces.iter(), indent);
                } else {
                    fmt_comments_only(buf, spaces.iter(), NewlineAt::Bottom, indent);
                }
                sub_expr.format_with_options(buf, parens, newlines, indent);
            }
            SpaceAfter(sub_expr, spaces) => {
                sub_expr.format_with_options(buf, parens, newlines, indent);
                if format_newlines {
                    fmt_spaces(buf, spaces.iter(), indent);
                } else {
                    fmt_comments_only(buf, spaces.iter(), NewlineAt::Bottom, indent);
                }
            }
            ParensAround(sub_expr) => {
                if parens == Parens::NotNeeded && !sub_expr_requests_parens(sub_expr) {
                    sub_expr.format_with_options(buf, Parens::NotNeeded, Newlines::Yes, indent);
                } else {
                    buf.push('(');
                    sub_expr.format_with_options(buf, Parens::NotNeeded, Newlines::Yes, indent);
                    buf.push(')');
                }
            }
            Str(literal) => {
                use roc_parse::ast::StrLiteral::*;

                buf.push('"');
                match literal {
                    PlainLine(string) => {
                        buf.push_str(string);
                    }
                    Line(segments) => {
                        for seg in segments.iter() {
                            format_str_segment(seg, buf, 0)
                        }
                    }
                    Block(lines) => {
                        buf.push_str("\"\"");

                        if lines.len() > 1 {
                            // Since we have multiple lines, format this with
                            // the `"""` symbols on their own lines, and the
                            newline(buf, indent);

                            for segments in lines.iter() {
                                for seg in segments.iter() {
                                    format_str_segment(seg, buf, indent);
                                }

                                newline(buf, indent);
                            }
                        } else {
                            // This is a single-line block string, for example:
                            //
                            //     """Whee, "quotes" inside quotes!"""

                            // This loop will run either 0 or 1 times.
                            for segments in lines.iter() {
                                for seg in segments.iter() {
                                    format_str_segment(seg, buf, indent);
                                }

                                // Don't print a newline here, because we either
                                // just printed 1 or 0 lines.
                            }
                        }

                        buf.push_str("\"\"");
                    }
                }
                buf.push('"');
            }
            Var { module_name, ident } => {
                if !module_name.is_empty() {
                    buf.push_str(module_name);
                    buf.push('.');
                }

                buf.push_str(ident);
            }
            Underscore(name) => {
                buf.push('_');
                buf.push_str(name);
            }
            Apply(loc_expr, loc_args, _) => {
                if apply_needs_parens && !loc_args.is_empty() {
                    buf.push('(');
                }

                loc_expr.format_with_options(buf, Parens::InApply, Newlines::Yes, indent);

                let multiline_args = loc_args.iter().any(|loc_arg| loc_arg.is_multiline());

                if multiline_args {
                    let arg_indent = indent + INDENT;

                    for loc_arg in loc_args.iter() {
                        newline(buf, arg_indent);
                        loc_arg.format_with_options(buf, Parens::InApply, Newlines::No, arg_indent);
                    }
                } else {
                    for loc_arg in loc_args.iter() {
                        buf.push(' ');
                        loc_arg.format_with_options(buf, Parens::InApply, Newlines::Yes, indent);
                    }
                }

                if apply_needs_parens && !loc_args.is_empty() {
                    buf.push(')');
                }
            }
            Num(string) | Float(string) | GlobalTag(string) | PrivateTag(string) => {
                buf.push_str(string)
            }
            NonBase10Int {
                base,
                string,
                is_negative,
            } => {
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
            Record(fields) => {
                fmt_record(buf, None, *fields, indent);
            }
            RecordUpdate { update, fields } => {
                fmt_record(buf, Some(*update), *fields, indent);
            }
            Closure(loc_patterns, loc_ret) => {
                fmt_closure(buf, loc_patterns, loc_ret, indent);
            }
            Backpassing(loc_patterns, loc_body, loc_ret) => {
                fmt_backpassing(buf, loc_patterns, loc_body, loc_ret, indent);
            }
            Defs(defs, ret) => {
                // It should theoretically be impossible to *parse* an empty defs list.
                // (Canonicalization can remove defs later, but that hasn't happened yet!)
                debug_assert!(!defs.is_empty());

                for loc_def in defs.iter() {
                    fmt_def(buf, &loc_def.value, indent);
                }

                let empty_line_before_return = empty_line_before_expr(&ret.value);

                if !empty_line_before_return {
                    buf.push('\n');
                }

                // Even if there were no defs, which theoretically should never happen,
                // still print the return value.
                ret.format_with_options(buf, Parens::NotNeeded, Newlines::Yes, indent);
            }
            Expect(condition, continuation) => {
                fmt_expect(buf, condition, continuation, self.is_multiline(), indent);
            }
            If(branches, final_else) => {
                fmt_if(buf, branches, final_else, self.is_multiline(), indent);
            }
            When(loc_condition, branches) => fmt_when(buf, loc_condition, branches, indent),
            List(items) => {
                fmt_list(buf, *items, indent);
            }
            BinOps(lefts, right) => fmt_bin_ops(buf, lefts, right, false, parens, indent),
            UnaryOp(sub_expr, unary_op) => {
                match &unary_op.value {
                    operator::UnaryOp::Negate => {
                        buf.push('-');
                    }
                    operator::UnaryOp::Not => {
                        buf.push('!');
                    }
                }

                sub_expr.format_with_options(buf, parens, newlines, indent);
            }
            AccessorFunction(key) => {
                buf.push('.');
                buf.push_str(key);
            }
            Access(expr, key) => {
                expr.format_with_options(buf, Parens::InApply, Newlines::Yes, indent);
                buf.push('.');
                buf.push_str(key);
            }
            MalformedIdent(_, _) => {}
            MalformedClosure => {}
            PrecedenceConflict { .. } => {}
        }
    }
}

fn format_str_segment<'a>(seg: &StrSegment<'a>, buf: &mut String<'a>, indent: u16) {
    use StrSegment::*;

    match seg {
        Plaintext(string) => {
            buf.push_str(string);
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
            buf.push_str("\\(");
            // e.g. (name) in "Hi, \(name)!"
            loc_expr.value.format_with_options(
                buf,
                Parens::NotNeeded, // We already printed parens!
                Newlines::No,      // Interpolations can never have newlines
                indent,
            );
            buf.push(')');
        }
    }
}

fn push_op(buf: &mut String, op: BinOp) {
    match op {
        operator::BinOp::Caret => buf.push('^'),
        operator::BinOp::Star => buf.push('*'),
        operator::BinOp::Slash => buf.push('/'),
        operator::BinOp::DoubleSlash => buf.push_str("//"),
        operator::BinOp::Percent => buf.push('%'),
        operator::BinOp::DoublePercent => buf.push_str("%%"),
        operator::BinOp::Plus => buf.push('+'),
        operator::BinOp::Minus => buf.push('-'),
        operator::BinOp::Equals => buf.push_str("=="),
        operator::BinOp::NotEquals => buf.push_str("!="),
        operator::BinOp::LessThan => buf.push('<'),
        operator::BinOp::GreaterThan => buf.push('>'),
        operator::BinOp::LessThanOrEq => buf.push_str("<="),
        operator::BinOp::GreaterThanOrEq => buf.push_str(">="),
        operator::BinOp::And => buf.push_str("&&"),
        operator::BinOp::Or => buf.push_str("||"),
        operator::BinOp::Pizza => buf.push_str("|>"),
        operator::BinOp::Assignment => unreachable!(),
        operator::BinOp::HasType => unreachable!(),
        operator::BinOp::Backpassing => unreachable!(),
    }
}

fn fmt_bin_ops<'a>(
    buf: &mut String<'a>,
    lefts: &'a [(Located<Expr<'a>>, Located<BinOp>)],
    loc_right_side: &'a Located<Expr<'a>>,
    part_of_multi_line_bin_ops: bool,
    apply_needs_parens: Parens,
    indent: u16,
) {
    let is_multiline = part_of_multi_line_bin_ops
        || (&loc_right_side.value).is_multiline()
        || lefts.iter().any(|(expr, _)| expr.value.is_multiline());

    for (loc_left_side, loc_bin_op) in lefts {
        let bin_op = loc_bin_op.value;

        loc_left_side.format_with_options(buf, apply_needs_parens, Newlines::No, indent);

        if is_multiline {
            newline(buf, indent + INDENT)
        } else {
            buf.push(' ');
        }

        push_op(buf, bin_op);

        buf.push(' ');
    }

    loc_right_side.format_with_options(buf, apply_needs_parens, Newlines::Yes, indent);
}

fn fmt_list<'a>(buf: &mut String<'a>, items: Collection<'a, &'a Located<Expr<'a>>>, indent: u16) {
    let loc_items = items.items;
    let final_comments = items.final_comments();
    if loc_items.is_empty() && final_comments.iter().all(|c| c.is_newline()) {
        buf.push_str("[]");
    } else {
        buf.push('[');
        let is_multiline = loc_items.iter().any(|item| (&item.value).is_multiline());
        if is_multiline {
            let item_indent = indent + INDENT;
            for item in loc_items.iter() {
                match &item.value {
                    // TODO?? These SpaceAfter/SpaceBefore litany seems overcomplicated
                    // Can we simplify this? Or at least move this in a separate function.
                    Expr::SpaceBefore(expr_below, spaces_above_expr) => {
                        newline(buf, item_indent);
                        fmt_comments_only(
                            buf,
                            spaces_above_expr.iter(),
                            NewlineAt::Bottom,
                            item_indent,
                        );

                        match &expr_below {
                            Expr::SpaceAfter(expr_above, spaces_below_expr) => {
                                expr_above.format(buf, item_indent);
                                buf.push(',');

                                fmt_comments_only(
                                    buf,
                                    spaces_below_expr.iter(),
                                    NewlineAt::Top,
                                    item_indent,
                                );
                            }
                            _ => {
                                expr_below.format(buf, item_indent);
                                buf.push(',');
                            }
                        }
                    }

                    Expr::SpaceAfter(sub_expr, spaces) => {
                        newline(buf, item_indent);

                        sub_expr.format(buf, item_indent);
                        buf.push(',');

                        fmt_comments_only(buf, spaces.iter(), NewlineAt::Top, item_indent);
                    }

                    _ => {
                        newline(buf, item_indent);
                        item.format_with_options(
                            buf,
                            Parens::NotNeeded,
                            Newlines::Yes,
                            item_indent,
                        );
                        buf.push(',');
                    }
                }
            }
            fmt_comments_only(buf, final_comments.iter(), NewlineAt::Top, item_indent);
            newline(buf, indent);
            buf.push(']');
        } else {
            // is_multiline == false
            let mut iter = loc_items.iter().peekable();
            while let Some(item) = iter.next() {
                buf.push(' ');
                item.format_with_options(buf, Parens::NotNeeded, Newlines::Yes, indent);
                if iter.peek().is_some() {
                    buf.push(',');
                }
            }
            buf.push_str(" ]");
        }
    }
}

fn empty_line_before_expr<'a>(expr: &'a Expr<'a>) -> bool {
    use roc_parse::ast::Expr::*;

    match expr {
        SpaceBefore(_, spaces) => {
            let mut has_at_least_one_newline = false;

            for comment_or_newline in spaces.iter() {
                match comment_or_newline {
                    CommentOrNewline::Newline => {
                        if has_at_least_one_newline {
                            return true;
                        } else {
                            has_at_least_one_newline = true;
                        }
                    }
                    CommentOrNewline::LineComment(_) | CommentOrNewline::DocComment(_) => {}
                }
            }

            false
        }

        _ => false,
    }
}

fn fmt_when<'a>(
    buf: &mut String<'a>,
    loc_condition: &'a Located<Expr<'a>>,
    branches: &[&'a WhenBranch<'a>],
    indent: u16,
) {
    let is_multiline_condition = loc_condition.is_multiline();
    buf.push_str(
        "\
         when",
    );
    if is_multiline_condition {
        let condition_indent = indent + INDENT;

        match &loc_condition.value {
            Expr::SpaceBefore(expr_below, spaces_above_expr) => {
                fmt_comments_only(
                    buf,
                    spaces_above_expr.iter(),
                    NewlineAt::Top,
                    condition_indent,
                );
                newline(buf, condition_indent);
                match &expr_below {
                    Expr::SpaceAfter(expr_above, spaces_below_expr) => {
                        expr_above.format(buf, condition_indent);
                        fmt_comments_only(
                            buf,
                            spaces_below_expr.iter(),
                            NewlineAt::Top,
                            condition_indent,
                        );
                        newline(buf, indent);
                    }
                    _ => {
                        expr_below.format(buf, condition_indent);
                    }
                }
            }
            _ => {
                newline(buf, condition_indent);
                loc_condition.format(buf, condition_indent);
                newline(buf, indent);
            }
        }
    } else {
        buf.push(' ');
        loc_condition.format(buf, indent);
        buf.push(' ');
    }
    buf.push_str("is\n");

    let mut it = branches.iter().peekable();
    while let Some(branch) = it.next() {
        let patterns = &branch.patterns;
        let expr = &branch.value;
        add_spaces(buf, indent + INDENT);
        let (first_pattern, rest) = patterns.split_first().unwrap();
        let is_multiline = match rest.last() {
            None => false,
            Some(last_pattern) => first_pattern.region.start_line != last_pattern.region.end_line,
        };

        fmt_pattern(
            buf,
            &first_pattern.value,
            indent + INDENT,
            Parens::NotNeeded,
        );
        for when_pattern in rest {
            if is_multiline {
                buf.push_str("\n");
                add_spaces(buf, indent + INDENT);
                buf.push_str("| ");
            } else {
                buf.push_str(" | ");
            }
            fmt_pattern(buf, &when_pattern.value, indent + INDENT, Parens::NotNeeded);
        }

        if let Some(guard_expr) = &branch.guard {
            buf.push_str(" if ");
            guard_expr.format_with_options(buf, Parens::NotNeeded, Newlines::Yes, indent + INDENT);
        }

        buf.push_str(" ->\n");

        add_spaces(buf, indent + (INDENT * 2));
        match expr.value {
            Expr::SpaceBefore(nested, spaces) => {
                fmt_comments_only(buf, spaces.iter(), NewlineAt::Bottom, indent + (INDENT * 2));
                nested.format_with_options(
                    buf,
                    Parens::NotNeeded,
                    Newlines::Yes,
                    indent + 2 * INDENT,
                );
            }
            _ => {
                expr.format_with_options(
                    buf,
                    Parens::NotNeeded,
                    Newlines::Yes,
                    indent + 2 * INDENT,
                );
            }
        }

        if it.peek().is_some() {
            buf.push('\n');
            buf.push('\n');
        }
    }
}

fn fmt_expect<'a>(
    buf: &mut String<'a>,
    condition: &'a Located<Expr<'a>>,
    continuation: &'a Located<Expr<'a>>,
    is_multiline: bool,
    indent: u16,
) {
    let return_indent = if is_multiline {
        indent + INDENT
    } else {
        indent
    };

    buf.push_str("expect");
    condition.format(buf, return_indent);
    buf.push('\n');
    continuation.format(buf, return_indent);
}

fn fmt_if<'a>(
    buf: &mut String<'a>,
    branches: &'a [(Located<Expr<'a>>, Located<Expr<'a>>)],
    final_else: &'a Located<Expr<'a>>,
    is_multiline: bool,
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

    for (loc_condition, loc_then) in branches.iter() {
        let is_multiline_condition = loc_condition.is_multiline();

        buf.push_str("if");

        if is_multiline_condition {
            match &loc_condition.value {
                Expr::SpaceBefore(expr_below, spaces_above_expr) => {
                    fmt_comments_only(buf, spaces_above_expr.iter(), NewlineAt::Top, return_indent);
                    newline(buf, return_indent);

                    match &expr_below {
                        Expr::SpaceAfter(expr_above, spaces_below_expr) => {
                            expr_above.format(buf, return_indent);
                            fmt_comments_only(
                                buf,
                                spaces_below_expr.iter(),
                                NewlineAt::Top,
                                return_indent,
                            );
                            newline(buf, indent);
                        }

                        _ => {
                            expr_below.format(buf, return_indent);
                        }
                    }
                }

                Expr::SpaceAfter(expr_above, spaces_below_expr) => {
                    newline(buf, return_indent);
                    expr_above.format(buf, return_indent);
                    fmt_comments_only(buf, spaces_below_expr.iter(), NewlineAt::Top, return_indent);
                    newline(buf, indent);
                }

                _ => {
                    newline(buf, return_indent);
                    loc_condition.format(buf, return_indent);
                    newline(buf, indent);
                }
            }
        } else {
            buf.push(' ');
            loc_condition.format_with_options(buf, Parens::NotNeeded, Newlines::Yes, indent);
            buf.push(' ');
        }

        buf.push_str("then");

        if is_multiline {
            match &loc_then.value {
                Expr::SpaceBefore(expr_below, spaces_below) => {
                    // we want exactly one newline, user-inserted extra newlines are ignored.
                    newline(buf, return_indent);
                    fmt_comments_only(buf, spaces_below.iter(), NewlineAt::Bottom, return_indent);

                    match &expr_below {
                        Expr::SpaceAfter(expr_above, spaces_above) => {
                            expr_above.format(buf, return_indent);

                            fmt_comments_only(
                                buf,
                                spaces_above.iter(),
                                NewlineAt::Top,
                                return_indent,
                            );
                            newline(buf, indent);
                        }

                        _ => {
                            expr_below.format(buf, return_indent);
                        }
                    }
                }
                _ => {
                    loc_condition.format(buf, return_indent);
                }
            }
        } else {
            buf.push_str(" ");
            loc_then.format(buf, return_indent);
        }
    }

    if is_multiline {
        buf.push_str("else");
        newline(buf, return_indent);
    } else {
        buf.push_str(" else ");
    }

    final_else.format(buf, return_indent);
}

fn fmt_closure<'a>(
    buf: &mut String<'a>,
    loc_patterns: &'a [Located<Pattern<'a>>],
    loc_ret: &'a Located<Expr<'a>>,
    indent: u16,
) {
    use self::Expr::*;

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

    let mut it = loc_patterns.iter().peekable();

    while let Some(loc_pattern) = it.next() {
        loc_pattern.format(buf, indent);

        if it.peek().is_some() {
            if arguments_are_multiline {
                buf.push(',');
                newline(buf, indent);
            } else {
                buf.push_str(", ");
            }
        }
    }

    if arguments_are_multiline {
        newline(buf, indent);
    } else {
        buf.push(' ');
    }

    buf.push_str("->");

    let is_multiline = (&loc_ret.value).is_multiline();

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
            buf.push(' ');
        }
    };

    loc_ret.format_with_options(buf, Parens::NotNeeded, Newlines::Yes, body_indent);
}

fn fmt_backpassing<'a>(
    buf: &mut String<'a>,
    loc_patterns: &'a [Located<Pattern<'a>>],
    loc_body: &'a Located<Expr<'a>>,
    loc_ret: &'a Located<Expr<'a>>,
    indent: u16,
) {
    use self::Expr::*;

    let arguments_are_multiline = loc_patterns
        .iter()
        .any(|loc_pattern| loc_pattern.is_multiline());

    // If the arguments are multiline, go down a line and indent.
    let indent = if arguments_are_multiline {
        indent + INDENT
    } else {
        indent
    };

    let mut it = loc_patterns.iter().peekable();

    while let Some(loc_pattern) = it.next() {
        loc_pattern.format(buf, indent);

        if it.peek().is_some() {
            if arguments_are_multiline {
                buf.push(',');
                newline(buf, indent);
            } else {
                buf.push_str(", ");
            }
        }
    }

    if arguments_are_multiline {
        newline(buf, indent);
    } else {
        buf.push(' ');
    }

    buf.push_str("<-");

    let is_multiline = (&loc_ret.value).is_multiline();

    // If the body is multiline, go down a line and indent.
    let body_indent = if is_multiline {
        indent + INDENT
    } else {
        indent
    };

    // the body of the Backpass can be on the same line, or
    // on a new line. If it's on the same line, insert a space.

    match &loc_ret.value {
        SpaceBefore(_, _) => {
            // the body starts with (first comment and then) a newline
            // do nothing
        }
        _ => {
            // add a space after the `<-`
            buf.push(' ');
        }
    };

    loc_body.format_with_options(buf, Parens::NotNeeded, Newlines::Yes, body_indent);
    loc_ret.format_with_options(buf, Parens::NotNeeded, Newlines::Yes, indent);
}

fn fmt_record<'a>(
    buf: &mut String<'a>,
    update: Option<&'a Located<Expr<'a>>>,
    fields: Collection<'a, Located<AssignedField<'a, Expr<'a>>>>,
    indent: u16,
) {
    let loc_fields = fields.items;
    let final_comments = fields.final_comments();
    if loc_fields.is_empty() && final_comments.iter().all(|c| c.is_newline()) {
        buf.push_str("{}");
    } else {
        buf.push('{');

        match update {
            None => {}
            // We are presuming this to be a Var()
            // If it wasnt a Var() we would not have made
            // it this far. For example "{ 4 & hello = 9 }"
            // doesnt make sense.
            Some(record_var) => {
                buf.push(' ');
                record_var.format(buf, indent);
                buf.push_str(" &");
            }
        }

        let is_multiline = loc_fields.iter().any(|loc_field| loc_field.is_multiline())
            || !final_comments.is_empty();

        if is_multiline {
            let field_indent = indent + INDENT;
            for field in loc_fields.iter() {
                // comma addition is handled by the `format_field_multiline` function
                // since we can have stuff like:
                // { x # comment
                // , y
                // }
                // In this case, we have to move the comma before the comment.
                format_field_multiline(buf, &field.value, field_indent, "");
            }

            fmt_comments_only(buf, final_comments.iter(), NewlineAt::Top, field_indent);

            newline(buf, indent);
        } else {
            // is_multiline == false
            buf.push(' ');
            let field_indent = indent;
            let mut iter = loc_fields.iter().peekable();
            while let Some(field) = iter.next() {
                field.format_with_options(buf, Parens::NotNeeded, Newlines::No, field_indent);

                if iter.peek().is_some() {
                    buf.push_str(", ");
                }
            }
            buf.push(' ');
            // if we are here, that means that `final_comments` is empty, thus we don't have
            // to add a comment. Anyway, it is not possible to have a single line record with
            // a comment in it.
        };

        // closes the initial bracket
        buf.push('}');
    }
}

fn format_field_multiline<'a, T>(
    buf: &mut String<'a>,
    field: &AssignedField<'a, T>,
    indent: u16,
    separator_prefix: &str,
) where
    T: Formattable<'a>,
{
    use self::AssignedField::*;
    match field {
        RequiredValue(name, spaces, ann) => {
            newline(buf, indent);
            buf.push_str(name.value);

            if !spaces.is_empty() {
                fmt_spaces(buf, spaces.iter(), indent);
            }

            buf.push_str(separator_prefix);
            buf.push_str(": ");
            ann.value.format(buf, indent);
            buf.push(',');
        }
        OptionalValue(name, spaces, ann) => {
            newline(buf, indent);
            buf.push_str(name.value);

            if !spaces.is_empty() {
                fmt_spaces(buf, spaces.iter(), indent);
            }

            buf.push_str(separator_prefix);
            buf.push_str("? ");
            ann.value.format(buf, indent);
            buf.push(',');
        }
        LabelOnly(name) => {
            newline(buf, indent);
            buf.push_str(name.value);
            buf.push(',');
        }
        AssignedField::SpaceBefore(sub_field, spaces) => {
            // We have something like that:
            // ```
            // # comment
            // field,
            // ```
            // we'd like to preserve this

            fmt_comments_only(buf, spaces.iter(), NewlineAt::Top, indent);
            format_field_multiline(buf, sub_field, indent, separator_prefix);
        }
        AssignedField::SpaceAfter(sub_field, spaces) => {
            // We have something like that:
            // ```
            // field # comment
            // , otherfield
            // ```
            // we'd like to transform it into:
            // ```
            // field,
            // # comment
            // otherfield
            // ```
            format_field_multiline(buf, sub_field, indent, separator_prefix);
            fmt_comments_only(buf, spaces.iter(), NewlineAt::Top, indent);
        }
        Malformed(raw) => {
            buf.push_str(raw);
        }
    }
}

fn sub_expr_requests_parens(expr: &Expr<'_>) -> bool {
    match expr {
        Expr::BinOps(left_side, _) => {
            left_side
                .iter()
                .any(|(_, loc_bin_op)| match loc_bin_op.value {
                    BinOp::Caret
                    | BinOp::Star
                    | BinOp::Slash
                    | BinOp::DoubleSlash
                    | BinOp::Percent
                    | BinOp::DoublePercent
                    | BinOp::Plus
                    | BinOp::Minus
                    | BinOp::Equals
                    | BinOp::NotEquals
                    | BinOp::LessThan
                    | BinOp::GreaterThan
                    | BinOp::LessThanOrEq
                    | BinOp::GreaterThanOrEq
                    | BinOp::And
                    | BinOp::Or => true,
                    BinOp::Pizza | BinOp::Assignment | BinOp::HasType | BinOp::Backpassing => false,
                })
        }
        _ => false,
    }
}
