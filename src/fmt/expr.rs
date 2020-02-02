use crate::fmt::def::fmt_def;
use crate::fmt::pattern::fmt_pattern;
use crate::fmt::spaces::{
    add_spaces, fmt_comments_only, fmt_if_spaces, fmt_spaces, is_comment, newline, INDENT,
};
use crate::parse::ast::{AssignedField, Base, CommentOrNewline, Expr, Pattern};
use crate::region::Located;
use bumpalo::collections::{String, Vec};

pub fn fmt_expr<'a>(
    buf: &mut String<'a>,
    expr: &'a Expr<'a>,
    indent: u16,
    apply_needs_parens: bool,
    format_newlines: bool,
) {
    use self::Expr::*;

    match expr {
        SpaceBefore(sub_expr, spaces) => {
            if format_newlines {
                fmt_spaces(buf, spaces.iter(), indent);
            } else {
                fmt_comments_only(buf, spaces.iter(), indent);
            }
            fmt_expr(buf, sub_expr, indent, apply_needs_parens, format_newlines);
        }
        SpaceAfter(sub_expr, spaces) => {
            fmt_expr(buf, sub_expr, indent, apply_needs_parens, format_newlines);
            if format_newlines {
                fmt_spaces(buf, spaces.iter(), indent);
            } else {
                fmt_comments_only(buf, spaces.iter(), indent);
            }
        }
        ParensAround(sub_expr) => {
            buf.push('(');
            fmt_expr(buf, sub_expr, indent, false, true);
            buf.push(')');
        }
        Str(string) => {
            buf.push('"');
            buf.push_str(string);
            buf.push('"');
        }
        Var(module_parts, name) => {
            for part in module_parts.iter() {
                buf.push_str(part);
                buf.push('.');
            }

            buf.push_str(name);
        }
        Apply(loc_expr, loc_args, _) => {
            if apply_needs_parens {
                buf.push('(');
            }

            fmt_expr(buf, &loc_expr.value, indent, true, true);

            for loc_arg in loc_args {
                buf.push(' ');

                fmt_expr(buf, &loc_arg.value, indent, true, true);
            }

            if apply_needs_parens {
                buf.push(')');
            }
        }
        BlockStr(lines) => {
            buf.push_str("\"\"\"");
            for line in lines.iter() {
                buf.push_str(line);
            }
            buf.push_str("\"\"\"");
        }
        Int(string) | Float(string) | GlobalTag(string) | PrivateTag(string) => {
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

            buf.push('0');

            buf.push(match base {
                Base::Hex => 'x',
                Base::Octal => 'o',
                Base::Binary => 'b',
            });

            buf.push_str(string);
        }
        Record { fields, update } => {
            fmt_record(buf, *update, fields, indent, apply_needs_parens);
        }
        Closure(loc_patterns, loc_ret) => {
            fmt_closure(buf, loc_patterns, loc_ret, indent);
        }
        Defs(defs, ret) => {
            // It should theoretically be impossible to *parse* an empty defs list.
            // (Canonicalization can remove defs later, but that hasn't happened yet!)
            debug_assert!(!defs.is_empty());

            // The first def is located last in the list, because it gets added there
            // with .push() for efficiency. (The order of parsed defs doesn't
            // matter because canonicalization sorts them anyway.)
            // The other defs in the list are in their usual order.
            if let Some(loc_first_def) = defs.last() {
                let other_spaced_defs = &defs[0..defs.len() - 1];

                fmt_def(buf, &loc_first_def.value, indent);

                for loc_def in other_spaced_defs.iter() {
                    fmt_def(buf, &loc_def.value, indent);
                }
            }

            let empty_line_before_return = empty_line_before_expr(&ret.value);

            if !empty_line_before_return {
                buf.push('\n');
            }

            // Even if there were no defs, which theoretically should never happen,
            // still print the return value.
            fmt_expr(buf, &ret.value, indent, false, true);
        }
        If((loc_condition, loc_then, loc_else)) => {
            fmt_if(buf, loc_condition, loc_then, loc_else, indent);
        }
        When(loc_condition, branches) => {

            buf.push_str(
                "\
                 when ",
            );
            fmt_expr(buf, &loc_condition.value, indent, false, true);
            buf.push_str(" is\n");

            let mut it = branches.iter().peekable();
            while let Some(branch) = it.next() {
                let patterns = &branch.patterns;
                let expr = &branch.value;
                add_spaces(buf, indent + INDENT);
                let (first_pattern, rest) = patterns.split_first().unwrap();
                let is_multiline = match rest.last() {
                    None => false,
                    Some(last_pattern) => {
                        first_pattern.region.start_line != last_pattern.region.end_line
                    }
                };

                fmt_pattern(buf, &first_pattern.value, indent + INDENT, false, true);
                for when_pattern in rest {
                    if is_multiline {
                        buf.push_str("\n");
                        add_spaces(buf, indent + INDENT);
                        buf.push_str("| ");
                    } else {
                        buf.push_str(" | ");
                    }
                    fmt_pattern(buf, &when_pattern.value, indent + INDENT, false, true);
                }

                buf.push_str(" ->\n");

                add_spaces(buf, indent + (INDENT * 2));
                match expr.value {
                    Expr::SpaceBefore(nested, spaces) => {
                        fmt_comments_only(buf, spaces.iter(), indent + (INDENT * 2));
                        fmt_expr(buf, &nested, indent + (INDENT * 2), false, true);
                    }
                    _ => {
                        fmt_expr(buf, &expr.value, indent + (INDENT * 2), false, true);
                    }
                }

                if it.peek().is_some() {
                    buf.push('\n');
                    buf.push('\n');
                }
            }
        }
        List(loc_items) => {
            fmt_list(buf, &loc_items, indent);
        }
        other => panic!("TODO implement Display for AST variant {:?}", other),
    }
}

pub fn fmt_list<'a>(
    buf: &mut String<'a>,
    loc_items: &'a Vec<'a, &'a Located<Expr<'a>>>,
    indent: u16,
) {
    buf.push('[');

    let mut iter = loc_items.iter().peekable();

    let is_multiline = loc_items.iter().any(|item| is_multiline_expr(&item.value));

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
                            fmt_expr(buf, expr_above, item_indent, false, false);

                            if iter.peek().is_some() {
                                buf.push(',');
                            }

                            fmt_if_spaces(buf, spaces_below_expr.iter(), item_indent);
                        }
                        _ => {
                            fmt_expr(buf, expr_below, item_indent, false, false);
                            if iter.peek().is_some() {
                                buf.push(',');
                            }
                        }
                    }
                }

                Expr::SpaceAfter(sub_expr, spaces) => {
                    newline(buf, item_indent);

                    fmt_expr(buf, sub_expr, item_indent, false, false);

                    if iter.peek().is_some() {
                        buf.push(',');
                    }

                    fmt_if_spaces(buf, spaces.iter(), item_indent);
                }

                _ => {
                    newline(buf, item_indent);
                    fmt_expr(buf, &item.value, item_indent, false, true);
                    if iter.peek().is_some() {
                        buf.push(',');
                    }
                }
            }
        } else {
            buf.push(' ');
            fmt_expr(buf, &item.value, item_indent, false, true);
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

pub fn fmt_field<'a>(
    buf: &mut String<'a>,
    assigned_field: &'a AssignedField<'a, Expr<'a>>,
    is_multiline: bool,
    indent: u16,
    apply_needs_parens: bool,
) {
    use self::AssignedField::*;

    match assigned_field {
        LabeledValue(name, spaces, value) => {
            if is_multiline {
                newline(buf, indent);
            }

            buf.push_str(name.value);

            if !spaces.is_empty() {
                fmt_spaces(buf, spaces.iter(), indent);
            }

            buf.push(':');
            buf.push(' ');
            fmt_expr(buf, &value.value, indent, apply_needs_parens, true);
        }
        LabelOnly(name) => {
            if is_multiline {
                newline(buf, indent);
            }

            buf.push_str(name.value);
        }
        AssignedField::SpaceBefore(sub_expr, spaces) => {
            fmt_comments_only(buf, spaces.iter(), indent);
            fmt_field(buf, sub_expr, is_multiline, indent, apply_needs_parens);
        }
        AssignedField::SpaceAfter(sub_expr, spaces) => {
            fmt_field(buf, sub_expr, is_multiline, indent, apply_needs_parens);
            fmt_comments_only(buf, spaces.iter(), indent);
        }
        Malformed(string) => buf.push_str(string),
    }
}

pub fn empty_line_before_expr<'a>(expr: &'a Expr<'a>) -> bool {
    use crate::parse::ast::Expr::*;

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
                    CommentOrNewline::LineComment(_) => {}
                }
            }

            false
        }

        Nested(nested_expr) => empty_line_before_expr(nested_expr),

        _ => false,
    }
}

pub fn is_multiline_pattern<'a>(pattern: &'a Pattern<'a>) -> bool {
    match pattern {
        Pattern::SpaceBefore(_, spaces) | Pattern::SpaceAfter(_, spaces) => {
            spaces.iter().any(|space| space.contains_newline())
        }

        Pattern::Nested(nested_pat) => is_multiline_pattern(nested_pat),
        Pattern::Identifier(_)
        | Pattern::GlobalTag(_)
        | Pattern::PrivateTag(_)
        | Pattern::Apply(_, _)
        | Pattern::RecordDestructure(_)
        | Pattern::RecordField(_, _)
        | Pattern::IntLiteral(_)
        | Pattern::NonBase10Literal { .. }
        | Pattern::FloatLiteral(_)
        | Pattern::StrLiteral(_)
        | Pattern::BlockStrLiteral(_)
        | Pattern::Underscore
        | Pattern::Malformed(_)
        | Pattern::QualifiedIdentifier(_) => false,
    }
}

pub fn is_multiline_expr<'a>(expr: &'a Expr<'a>) -> bool {
    use crate::parse::ast::Expr::*;
    // TODO cache these answers using a Map<Pointer, bool>, so
    // we don't have to traverse subexpressions repeatedly

    match expr {
        // Return whether these spaces contain any Newlines
        SpaceBefore(_, spaces) | SpaceAfter(_, spaces) => {
            spaces.iter().any(|space| space.contains_newline())
        }

        // These expressions never have newlines
        Float(_)
        | Int(_)
        | NonBase10Int { .. }
        | Str(_)
        | Access(_, _)
        | AccessorFunction(_)
        | Var(_, _)
        | MalformedIdent(_)
        | MalformedClosure
        | GlobalTag(_)
        | PrivateTag(_) => false,

        // These expressions always have newlines
        Defs(_, _) | When(_, _) => true,

        List(elems) => elems
            .iter()
            .any(|loc_expr| is_multiline_expr(&loc_expr.value)),

        BlockStr(lines) => lines.len() > 1,
        Apply(loc_expr, args, _) => {
            is_multiline_expr(&loc_expr.value)
                || args.iter().any(|loc_arg| is_multiline_expr(&loc_arg.value))
        }

        If((loc_cond, loc_if_true, loc_if_false)) => {
            is_multiline_expr(&loc_cond.value)
                || is_multiline_expr(&loc_if_true.value)
                || is_multiline_expr(&loc_if_false.value)
        }

        BinOp((loc_left, _, loc_right)) => {
            is_multiline_expr(&loc_left.value) || is_multiline_expr(&loc_right.value)
        }

        UnaryOp(loc_subexpr, _) | PrecedenceConflict(_, _, loc_subexpr) => {
            is_multiline_expr(&loc_subexpr.value)
        }

        ParensAround(subexpr) | Nested(subexpr) => is_multiline_expr(&subexpr),

        Closure(loc_patterns, loc_body) => {
            // check the body first because it's more likely to be multiline
            is_multiline_expr(&loc_body.value)
                || loc_patterns
                    .iter()
                    .any(|loc_pattern| is_multiline_pattern(&loc_pattern.value))
        }

        Record { fields, .. } => fields
            .iter()
            .any(|loc_field| is_multiline_field(&loc_field.value)),
    }
}

pub fn is_multiline_field<'a, Val>(field: &'a AssignedField<'a, Val>) -> bool {
    use self::AssignedField::*;

    match field {
        LabeledValue(_, spaces, _) => !spaces.is_empty(),
        LabelOnly(_) => false,
        AssignedField::SpaceBefore(_, _) | AssignedField::SpaceAfter(_, _) => true,
        Malformed(text) => text.chars().any(|c| c == '\n'),
    }
}

fn fmt_if<'a>(
    buf: &mut String<'a>,
    loc_condition: &'a Located<Expr<'a>>,
    loc_then: &'a Located<Expr<'a>>,
    loc_else: &'a Located<Expr<'a>>,
    indent: u16,
) {
    let is_multiline_then = is_multiline_expr(&loc_then.value);
    let is_multiline_else = is_multiline_expr(&loc_else.value);
    let is_multiline_condition = is_multiline_expr(&loc_condition.value);
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
                fmt_if_spaces(buf, spaces_above_expr.iter(), return_indent);
                newline(buf, return_indent);

                match &expr_below {
                    Expr::SpaceAfter(expr_above, spaces_below_expr) => {
                        fmt_expr(buf, &expr_above, return_indent, false, false);
                        fmt_if_spaces(buf, spaces_below_expr.iter(), return_indent);
                        newline(buf, indent);
                    }

                    _ => {
                        fmt_expr(buf, &expr_below, return_indent, false, false);
                    }
                }
            }
            _ => {
                fmt_expr(buf, &loc_condition.value, return_indent, false, false);
            }
        }
    } else {
        buf.push(' ');
        fmt_expr(buf, &loc_condition.value, indent, false, true);
        buf.push(' ');
    }

    buf.push_str("then");

    if is_multiline {
        match &loc_then.value {
            Expr::SpaceBefore(expr_below, spaces_below) => {
                let any_comments_below = spaces_below.iter().any(is_comment);

                if !any_comments_below {
                    newline(buf, return_indent);
                }

                fmt_if_spaces(buf, spaces_below.iter(), return_indent);

                if any_comments_below {
                    newline(buf, return_indent);
                }

                match &expr_below {
                    Expr::SpaceAfter(expr_above, spaces_above) => {
                        fmt_expr(buf, &expr_above, return_indent, false, false);

                        fmt_if_spaces(buf, spaces_above.iter(), return_indent);
                        newline(buf, indent);
                    }

                    _ => {
                        fmt_expr(buf, &expr_below, return_indent, false, false);
                    }
                }
            }
            _ => {
                fmt_expr(buf, &loc_condition.value, return_indent, false, false);
            }
        }
    } else {
        buf.push_str(" ");
        fmt_expr(buf, &loc_then.value, return_indent, false, false);
    }

    if is_multiline {
        buf.push_str("else");
        newline(buf, return_indent);
    } else {
        buf.push_str(" else ");
    }

    fmt_expr(buf, &loc_else.value, return_indent, false, false);
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
        .any(|loc_pattern| is_multiline_pattern(&loc_pattern.value));

    // If the arguments are multiline, go down a line and indent.
    let indent = if arguments_are_multiline {
        indent + INDENT
    } else {
        indent
    };

    let mut any_args_printed = false;

    for loc_pattern in loc_patterns.iter() {
        if any_args_printed {
            buf.push(',');

            if !arguments_are_multiline {
                buf.push(' ');
            }
        } else {
            any_args_printed = true;
        }

        fmt_pattern(buf, &loc_pattern.value, indent, false, false);
    }

    if !arguments_are_multiline {
        buf.push(' ');
    }

    buf.push_str("->");

    let is_multiline = is_multiline_expr(&loc_ret.value);

    // If the body is multiline, go down a line and indent.
    let indent = if is_multiline {
        indent + INDENT
    } else {
        indent
    };

    let newline_is_next = match &loc_ret.value {
        SpaceBefore(_, _) => true,
        _ => false,
    };

    if !newline_is_next {
        // Push a space after the "->" preceding this.
        buf.push(' ');
    }

    fmt_expr(buf, &loc_ret.value, indent, false, true);
}

pub fn fmt_record<'a>(
    buf: &mut String<'a>,
    update: Option<&'a Located<Expr<'a>>>,
    loc_fields: &'a Vec<'a, Located<AssignedField<'a, Expr<'a>>>>,
    indent: u16,
    apply_needs_parens: bool,
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
            fmt_expr(buf, &record_var.value, indent, false, false);
            buf.push_str(" &");
        }
    }

    let is_multiline = loc_fields
        .iter()
        .any(|loc_field| is_multiline_field(&loc_field.value));

    let mut iter = loc_fields.iter().peekable();
    let field_indent = if is_multiline {
        indent + INDENT
    } else {
        if !loc_fields.is_empty() {
            buf.push(' ');
        }

        indent
    };

    while let Some(field) = iter.next() {
        fmt_field(
            buf,
            &field.value,
            is_multiline,
            field_indent,
            apply_needs_parens,
        );

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
