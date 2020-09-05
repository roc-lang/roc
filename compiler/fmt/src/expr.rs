use crate::annotation::{Formattable, Newlines, Parens};
use crate::def::fmt_def;
use crate::pattern::fmt_pattern;
use crate::spaces::{
    add_spaces, fmt_comments_only, fmt_condition_spaces, fmt_spaces, newline, INDENT,
};
use bumpalo::collections::{String, Vec};
use roc_module::operator::{self, BinOp};
use roc_parse::ast::StrSegment;
use roc_parse::ast::{AssignedField, Base, CommentOrNewline, Expr, Pattern, WhenBranch};
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
            | MalformedIdent(_)
            | MalformedClosure
            | GlobalTag(_)
            | PrivateTag(_) => false,

            // These expressions always have newlines
            Defs(_, _) | When(_, _) => true,

            List(elems) => elems.iter().any(|loc_expr| loc_expr.is_multiline()),

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

            If(loc_cond, loc_if_true, loc_if_false) => {
                loc_cond.is_multiline() || loc_if_true.is_multiline() || loc_if_false.is_multiline()
            }

            BinOp((loc_left, _, loc_right)) => {
                let next_is_multiline_bin_op: bool = match &loc_right.value {
                    Expr::BinOp((_, _, nested_loc_right)) => nested_loc_right.is_multiline(),
                    _ => false,
                };

                next_is_multiline_bin_op || loc_left.is_multiline() || loc_right.is_multiline()
            }

            UnaryOp(loc_subexpr, _) | PrecedenceConflict(_, _, _, loc_subexpr) => {
                loc_subexpr.is_multiline()
            }

            ParensAround(subexpr) | Nested(subexpr) => subexpr.is_multiline(),

            Closure(loc_patterns, loc_body) => {
                // check the body first because it's more likely to be multiline
                loc_body.is_multiline()
                    || loc_patterns
                        .iter()
                        .any(|loc_pattern| loc_pattern.is_multiline())
            }

            Record { fields, .. } => fields.iter().any(|loc_field| loc_field.is_multiline()),
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
                    fmt_comments_only(buf, spaces.iter(), indent);
                }
                sub_expr.format_with_options(buf, parens, newlines, indent);
            }
            SpaceAfter(sub_expr, spaces) => {
                sub_expr.format_with_options(buf, parens, newlines, indent);
                if format_newlines {
                    fmt_spaces(buf, spaces.iter(), indent);
                } else {
                    fmt_comments_only(buf, spaces.iter(), indent);
                }
            }
            ParensAround(sub_expr) => {
                buf.push('(');
                sub_expr.format_with_options(buf, Parens::NotNeeded, Newlines::Yes, indent);
                buf.push(')');
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
            Apply(loc_expr, loc_args, _) => {
                if apply_needs_parens {
                    buf.push('(');
                }

                loc_expr.format_with_options(buf, Parens::InApply, Newlines::Yes, indent);

                let multiline_args = loc_args.iter().any(|loc_arg| loc_arg.is_multiline());

                if multiline_args {
                    let arg_indent = indent + INDENT;

                    for loc_arg in loc_args {
                        newline(buf, arg_indent);
                        loc_arg.format_with_options(buf, Parens::InApply, Newlines::No, arg_indent);
                    }
                } else {
                    for loc_arg in loc_args {
                        buf.push(' ');
                        loc_arg.format_with_options(buf, Parens::InApply, Newlines::Yes, indent);
                    }
                }

                if apply_needs_parens {
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
            Record { fields, update } => {
                fmt_record(buf, *update, fields, indent);
            }
            Closure(loc_patterns, loc_ret) => {
                fmt_closure(buf, loc_patterns, loc_ret, indent);
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
            If(loc_condition, loc_then, loc_else) => {
                fmt_if(buf, loc_condition, loc_then, loc_else, indent);
            }
            When(loc_condition, branches) => fmt_when(buf, loc_condition, branches, indent),
            List(loc_items) => {
                fmt_list(buf, &loc_items, indent);
            }
            BinOp((loc_left_side, bin_op, loc_right_side)) => fmt_bin_op(
                buf,
                loc_left_side,
                bin_op,
                loc_right_side,
                false,
                parens,
                indent,
            ),
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
            Nested(nested_expr) => {
                nested_expr.format_with_options(buf, parens, newlines, indent);
            }
            AccessorFunction(key) => {
                buf.push('.');
                buf.push_str(key);
            }
            Access(expr, key) => {
                expr.format_with_options(buf, parens, Newlines::Yes, indent);
                buf.push('.');
                buf.push_str(key);
            }
            MalformedIdent(_) => {}
            MalformedClosure => {}
            PrecedenceConflict(_, _, _, _) => {}
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

fn fmt_bin_op<'a>(
    buf: &mut String<'a>,
    loc_left_side: &'a Located<Expr<'a>>,
    loc_bin_op: &'a Located<BinOp>,
    loc_right_side: &'a Located<Expr<'a>>,
    part_of_multi_line_bin_ops: bool,
    apply_needs_parens: Parens,
    indent: u16,
) {
    loc_left_side.format_with_options(buf, apply_needs_parens, Newlines::No, indent);

    let is_multiline = (&loc_right_side.value).is_multiline()
        || (&loc_left_side.value).is_multiline()
        || part_of_multi_line_bin_ops;

    if is_multiline {
        newline(buf, indent + INDENT)
    } else {
        buf.push(' ');
    }

    match &loc_bin_op.value {
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
    }

    buf.push(' ');

    match &loc_right_side.value {
        Expr::BinOp((nested_left_side, nested_bin_op, nested_right_side)) => {
            fmt_bin_op(
                buf,
                nested_left_side,
                nested_bin_op,
                nested_right_side,
                is_multiline,
                apply_needs_parens,
                indent,
            );
        }

        _ => {
            loc_right_side.format_with_options(buf, apply_needs_parens, Newlines::Yes, indent);
        }
    }
}

pub fn fmt_list<'a>(buf: &mut String<'a>, loc_items: &[&Located<Expr<'a>>], indent: u16) {
    buf.push('[');

    let mut iter = loc_items.iter().peekable();

    let is_multiline = loc_items.iter().any(|item| (&item.value).is_multiline());

    let item_indent = if is_multiline {
        indent + INDENT
    } else {
        indent
    };

    while let Some(item) = iter.next() {
        if is_multiline {
            match &item.value {
                Expr::SpaceBefore(expr_below, spaces_above_expr) => {
                    newline(buf, item_indent);
                    fmt_comments_only(buf, spaces_above_expr.iter(), item_indent);

                    match &expr_below {
                        Expr::SpaceAfter(expr_above, spaces_below_expr) => {
                            expr_above.format(buf, item_indent);

                            if iter.peek().is_some() {
                                buf.push(',');
                            }

                            fmt_condition_spaces(buf, spaces_below_expr.iter(), item_indent);
                        }
                        _ => {
                            expr_below.format(buf, item_indent);
                            if iter.peek().is_some() {
                                buf.push(',');
                            }
                        }
                    }
                }

                Expr::SpaceAfter(sub_expr, spaces) => {
                    newline(buf, item_indent);

                    sub_expr.format(buf, item_indent);

                    if iter.peek().is_some() {
                        buf.push(',');
                    }

                    fmt_condition_spaces(buf, spaces.iter(), item_indent);
                }

                _ => {
                    newline(buf, item_indent);
                    item.format_with_options(buf, Parens::NotNeeded, Newlines::Yes, item_indent);
                    if iter.peek().is_some() {
                        buf.push(',');
                    }
                }
            }
        } else {
            buf.push(' ');
            item.format_with_options(buf, Parens::NotNeeded, Newlines::Yes, item_indent);
            if iter.peek().is_some() {
                buf.push(',');
            }
        }
    }

    if is_multiline {
        newline(buf, indent);
    }

    if !loc_items.is_empty() && !is_multiline {
        buf.push(' ');
    }
    buf.push(']');
}

pub fn empty_line_before_expr<'a>(expr: &'a Expr<'a>) -> bool {
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

        Nested(nested_expr) => empty_line_before_expr(nested_expr),

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
                fmt_condition_spaces(buf, spaces_above_expr.iter(), condition_indent);
                newline(buf, condition_indent);
                match &expr_below {
                    Expr::SpaceAfter(expr_above, spaces_below_expr) => {
                        expr_above.format(buf, condition_indent);
                        fmt_condition_spaces(buf, spaces_below_expr.iter(), condition_indent);
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
                fmt_comments_only(buf, spaces.iter(), indent + (INDENT * 2));
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

fn fmt_if<'a>(
    buf: &mut String<'a>,
    loc_condition: &'a Located<Expr<'a>>,
    loc_then: &'a Located<Expr<'a>>,
    loc_else: &'a Located<Expr<'a>>,
    indent: u16,
) {
    let is_multiline_then = loc_then.is_multiline();
    let is_multiline_else = loc_else.is_multiline();
    let is_multiline_condition = loc_condition.is_multiline();
    let is_multiline = is_multiline_then || is_multiline_else || is_multiline_condition;

    let return_indent = if is_multiline {
        indent + INDENT
    } else {
        indent
    };

    buf.push_str("if");

    if is_multiline_condition {
        match &loc_condition.value {
            Expr::SpaceBefore(expr_below, spaces_above_expr) => {
                fmt_condition_spaces(buf, spaces_above_expr.iter(), return_indent);
                newline(buf, return_indent);

                match &expr_below {
                    Expr::SpaceAfter(expr_above, spaces_below_expr) => {
                        expr_above.format(buf, return_indent);
                        fmt_condition_spaces(buf, spaces_below_expr.iter(), return_indent);
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
                fmt_condition_spaces(buf, spaces_below_expr.iter(), return_indent);
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
                fmt_comments_only(buf, spaces_below.iter(), return_indent);

                match &expr_below {
                    Expr::SpaceAfter(expr_above, spaces_above) => {
                        expr_above.format(buf, return_indent);

                        fmt_condition_spaces(buf, spaces_above.iter(), return_indent);
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

    if is_multiline {
        buf.push_str("else");
        newline(buf, return_indent);
    } else {
        buf.push_str(" else ");
    }

    loc_else.format(buf, return_indent);
}

pub fn fmt_closure<'a>(
    buf: &mut String<'a>,
    loc_patterns: &'a Vec<'a, Located<Pattern<'a>>>,
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

pub fn fmt_record<'a>(
    buf: &mut String<'a>,
    update: Option<&'a Located<Expr<'a>>>,
    loc_fields: &[Located<AssignedField<'a, Expr<'a>>>],
    indent: u16,
) {
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

    let is_multiline = loc_fields.iter().any(|loc_field| loc_field.is_multiline());

    let mut iter = loc_fields.iter().peekable();
    let field_indent = if is_multiline {
        indent + INDENT
    } else {
        if !loc_fields.is_empty() {
            buf.push(' ');
        }

        indent
    };

    // we abuse the `Newlines` type to decide between multiline or single-line layout
    let newlines = if is_multiline {
        Newlines::Yes
    } else {
        Newlines::No
    };

    while let Some(field) = iter.next() {
        field.format_with_options(buf, Parens::NotNeeded, newlines, field_indent);

        if iter.peek().is_some() {
            buf.push(',');

            if !is_multiline {
                buf.push(' ');
            }
        }
    }

    if is_multiline {
        newline(buf, indent)
    } else if !loc_fields.is_empty() {
        buf.push(' ');
    }

    buf.push('}');
}
