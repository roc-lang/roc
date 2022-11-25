use crate::annotation::{Formattable, Newlines, Parens};
use crate::collection::{fmt_collection, Braces};
use crate::def::fmt_defs;
use crate::pattern::fmt_pattern;
use crate::spaces::{
    count_leading_newlines, fmt_comments_only, fmt_spaces, fmt_spaces_no_blank_lines, NewlineAt,
    INDENT,
};
use crate::Buf;
use roc_module::called_via::{self, BinOp};
use roc_parse::ast::{
    AssignedField, Base, Collection, CommentOrNewline, Expr, ExtractSpaces, Pattern, WhenBranch,
};
use roc_parse::ast::{StrLiteral, StrSegment};
use roc_region::all::Loc;

impl<'a> Formattable for Expr<'a> {
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
            Float(..)
            | Num(..)
            | NonBase10Int { .. }
            | SingleQuote(_)
            | RecordAccess(_, _)
            | RecordAccessorFunction(_)
            | TupleAccess(_, _)
            | TupleAccessorFunction(_)
            | Var { .. }
            | Underscore { .. }
            | MalformedIdent(_, _)
            | MalformedClosure
            | Tag(_)
            | OpaqueRef(_)
            | Crash => false,

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
            Dbg(condition, continuation) => condition.is_multiline() || continuation.is_multiline(),

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
            Tuple(fields) => fields.iter().any(|loc_field| loc_field.is_multiline()),
            RecordUpdate { fields, .. } => fields.iter().any(|loc_field| loc_field.is_multiline()),
        }
    }

    fn format_with_options<'buf>(
        &self,
        buf: &mut Buf<'buf>,
        parens: Parens,
        newlines: Newlines,
        indent: u16,
    ) {
        use self::Expr::*;

        let apply_needs_parens = parens == Parens::InApply;

        match self {
            SpaceBefore(sub_expr, spaces) => {
                format_spaces(buf, spaces, newlines, indent);
                sub_expr.format_with_options(buf, parens, newlines, indent);
            }
            SpaceAfter(sub_expr, spaces) => {
                sub_expr.format_with_options(buf, parens, newlines, indent);
                format_spaces(buf, spaces, newlines, indent);
            }
            ParensAround(sub_expr) => {
                if parens == Parens::NotNeeded && !sub_expr_requests_parens(sub_expr) {
                    sub_expr.format_with_options(buf, Parens::NotNeeded, Newlines::Yes, indent);
                } else {
                    let should_add_newlines = match sub_expr {
                        Expr::Closure(..)
                        | Expr::SpaceBefore(..)
                        | Expr::SpaceAfter(Closure(..), ..) => false,
                        _ => sub_expr.is_multiline(),
                    };

                    buf.indent(indent);
                    buf.push('(');
                    if should_add_newlines {
                        buf.newline();
                    }

                    let next_indent = if starts_with_newline(sub_expr) || should_add_newlines {
                        match sub_expr {
                            Expr::Closure(..) | Expr::SpaceAfter(Closure(..), ..) => indent,
                            _ => indent + INDENT,
                        }
                    } else {
                        indent
                    };

                    sub_expr.format_with_options(
                        buf,
                        Parens::NotNeeded,
                        Newlines::Yes,
                        next_indent,
                    );

                    if !matches!(sub_expr, Expr::SpaceAfter(..)) && should_add_newlines {
                        buf.newline();
                    }
                    buf.indent(indent);
                    buf.push(')');
                }
            }
            Str(literal) => {
                fmt_str_literal(buf, *literal, indent);
            }
            Var { module_name, ident } => {
                buf.indent(indent);
                if !module_name.is_empty() {
                    buf.push_str(module_name);
                    buf.push('.');
                }

                buf.push_str(ident);
            }
            Underscore(name) => {
                buf.indent(indent);
                buf.push('_');
                buf.push_str(name);
            }
            Crash => {
                buf.indent(indent);
                buf.push_str("crash");
            }
            Apply(loc_expr, loc_args, _) => {
                buf.indent(indent);
                if apply_needs_parens && !loc_args.is_empty() {
                    buf.push('(');
                }

                loc_expr.format_with_options(buf, Parens::InApply, Newlines::Yes, indent);

                let multiline_args = loc_args.iter().any(|loc_arg| loc_arg.is_multiline());

                let mut found_multiline_expr = false;
                let mut iter = loc_args.iter().peekable();

                while let Some(loc_arg) = iter.next() {
                    if iter.peek().is_none() {
                        found_multiline_expr = match loc_arg.value {
                            SpaceBefore(sub_expr, spaces) => match sub_expr {
                                Record { .. } | List { .. } => {
                                    let is_only_newlines = spaces.iter().all(|s| s.is_newline());
                                    is_only_newlines
                                        && !found_multiline_expr
                                        && sub_expr.is_multiline()
                                }
                                _ => false,
                            },
                            Record { .. } | List { .. } | Closure { .. } => {
                                !found_multiline_expr && loc_arg.is_multiline()
                            }
                            _ => false,
                        }
                    } else {
                        found_multiline_expr = loc_arg.is_multiline();
                    }
                }

                let should_outdent_last_arg = found_multiline_expr;

                if multiline_args && !should_outdent_last_arg {
                    let arg_indent = indent + INDENT;

                    for loc_arg in loc_args.iter() {
                        buf.newline();
                        loc_arg.format_with_options(buf, Parens::InApply, Newlines::No, arg_indent);
                    }
                } else if multiline_args && should_outdent_last_arg {
                    let mut iter = loc_args.iter().peekable();
                    while let Some(loc_arg) = iter.next() {
                        buf.spaces(1);

                        if iter.peek().is_none() {
                            match loc_arg.value {
                                SpaceBefore(sub_expr, _) => {
                                    sub_expr.format_with_options(
                                        buf,
                                        Parens::InApply,
                                        Newlines::Yes,
                                        indent,
                                    );
                                }
                                _ => {
                                    loc_arg.format_with_options(
                                        buf,
                                        Parens::InApply,
                                        Newlines::Yes,
                                        indent,
                                    );
                                }
                            }
                        } else {
                            loc_arg.format_with_options(
                                buf,
                                Parens::InApply,
                                Newlines::Yes,
                                indent,
                            );
                        }
                    }
                } else {
                    for loc_arg in loc_args.iter() {
                        buf.spaces(1);
                        loc_arg.format_with_options(buf, Parens::InApply, Newlines::Yes, indent);
                    }
                }

                if apply_needs_parens && !loc_args.is_empty() {
                    buf.push(')');
                }
            }
            &Num(string) => {
                buf.indent(indent);
                buf.push_str(string);
            }
            &Float(string) => {
                buf.indent(indent);
                buf.push_str(string);
            }
            Tag(string) | OpaqueRef(string) => {
                buf.indent(indent);
                buf.push_str(string)
            }
            SingleQuote(string) => {
                buf.indent(indent);
                buf.push('\'');
                for c in string.chars() {
                    if c == '"' {
                        buf.push_char_literal('"')
                    } else {
                        for escaped in c.escape_default() {
                            buf.push_char_literal(escaped);
                        }
                    }
                }
                buf.push('\'');
            }
            &NonBase10Int {
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

                fmt_defs(buf, defs, indent);

                match &ret.value {
                    SpaceBefore(sub_expr, spaces) => {
                        let empty_line_before_return = empty_line_before_expr(&ret.value);
                        let has_inline_comment = has_line_comment_before(&ret.value);

                        if has_inline_comment {
                            buf.spaces(1);
                            format_spaces(buf, spaces, newlines, indent);

                            if !empty_line_before_return {
                                buf.newline();
                            }

                            sub_expr.format_with_options(
                                buf,
                                Parens::NotNeeded,
                                Newlines::Yes,
                                indent,
                            );
                        } else {
                            if !empty_line_before_return {
                                buf.newline();
                            }

                            ret.format_with_options(buf, Parens::NotNeeded, Newlines::Yes, indent);
                        }
                    }
                    _ => {
                        // Even if there were no defs, which theoretically should never happen,
                        // still print the return value.
                        ret.format_with_options(buf, Parens::NotNeeded, Newlines::Yes, indent);
                    }
                }
            }
            Expect(condition, continuation) => {
                fmt_expect(buf, condition, continuation, self.is_multiline(), indent);
            }
            Dbg(condition, continuation) => {
                fmt_dbg(buf, condition, continuation, self.is_multiline(), indent);
            }
            If(branches, final_else) => {
                fmt_if(buf, branches, final_else, self.is_multiline(), indent);
            }
            When(loc_condition, branches) => fmt_when(buf, loc_condition, branches, indent),
            Tuple(items) => fmt_collection(buf, indent, Braces::Round, *items, Newlines::No),
            List(items) => fmt_collection(buf, indent, Braces::Square, *items, Newlines::No),
            BinOps(lefts, right) => fmt_binops(buf, lefts, right, false, parens, indent),
            UnaryOp(sub_expr, unary_op) => {
                buf.indent(indent);
                match &unary_op.value {
                    called_via::UnaryOp::Negate => {
                        buf.push('-');
                    }
                    called_via::UnaryOp::Not => {
                        buf.push('!');
                    }
                }

                sub_expr.format_with_options(buf, Parens::InApply, newlines, indent);
            }
            RecordAccessorFunction(key) => {
                buf.indent(indent);
                buf.push('.');
                buf.push_str(key);
            }
            RecordAccess(expr, key) => {
                expr.format_with_options(buf, Parens::InApply, Newlines::Yes, indent);
                buf.push('.');
                buf.push_str(key);
            }
            TupleAccessorFunction(key) => {
                buf.indent(indent);
                buf.push('.');
                buf.push_str(key);
            }
            TupleAccess(expr, key) => {
                expr.format_with_options(buf, Parens::InApply, Newlines::Yes, indent);
                buf.push('.');
                buf.push_str(key);
            }
            MalformedIdent(str, _) => {
                buf.indent(indent);
                buf.push_str(str)
            }
            MalformedClosure => {}
            PrecedenceConflict { .. } => {}
        }
    }
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

fn format_str_segment<'a, 'buf>(seg: &StrSegment<'a>, buf: &mut Buf<'buf>, indent: u16) {
    use StrSegment::*;

    match seg {
        Plaintext(string) => {
            // Lines in block strings will end with Plaintext ending in "\n" to indicate
            // a line break in the input string
            match string.strip_suffix('\n') {
                Some(string_without_newline) => {
                    buf.push_str_allow_spaces(string_without_newline);
                    buf.newline();
                }
                None => buf.push_str_allow_spaces(string),
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
        called_via::BinOp::Assignment => unreachable!(),
        called_via::BinOp::IsAliasType => unreachable!(),
        called_via::BinOp::IsOpaqueType => unreachable!(),
        called_via::BinOp::Backpassing => unreachable!(),
    }
}

pub fn fmt_str_literal<'buf>(buf: &mut Buf<'buf>, literal: StrLiteral, indent: u16) {
    use roc_parse::ast::StrLiteral::*;

    match literal {
        PlainLine(string) => {
            buf.indent(indent);
            buf.push('"');
            // When a PlainLine contains '\n' or '"', format as a block string
            if string.contains('"') || string.contains('\n') {
                buf.push_str("\"\"");
                buf.newline();
                for line in string.split('\n') {
                    buf.indent(indent);
                    buf.push_str_allow_spaces(line);
                    buf.newline();
                }
                buf.indent(indent);
                buf.push_str("\"\"");
            } else {
                buf.push_str_allow_spaces(string);
            };
            buf.push('"');
        }
        Line(segments) => {
            buf.indent(indent);
            buf.push('"');
            for seg in segments.iter() {
                format_str_segment(seg, buf, 0)
            }
            buf.push('"');
        }
        Block(lines) => {
            // Block strings will always be formatted with """ on new lines
            buf.ensure_ends_with_newline();
            buf.indent(indent);
            buf.push_str("\"\"\"");
            buf.newline();

            for segments in lines.iter() {
                for seg in segments.iter() {
                    buf.indent(indent);
                    format_str_segment(seg, buf, indent);
                }

                buf.newline();
            }
            buf.indent(indent);
            buf.push_str("\"\"\"");
        }
    }
}

fn fmt_binops<'a, 'buf>(
    buf: &mut Buf<'buf>,
    lefts: &'a [(Loc<Expr<'a>>, Loc<BinOp>)],
    loc_right_side: &'a Loc<Expr<'a>>,
    part_of_multi_line_binops: bool,
    apply_needs_parens: Parens,
    indent: u16,
) {
    let is_multiline = part_of_multi_line_binops
        || loc_right_side.value.is_multiline()
        || lefts.iter().any(|(expr, _)| expr.value.is_multiline());

    for (loc_left_side, loc_binop) in lefts {
        let binop = loc_binop.value;

        loc_left_side.format_with_options(buf, apply_needs_parens, Newlines::No, indent);

        if is_multiline {
            buf.ensure_ends_with_newline();
            buf.indent(indent);
        } else {
            buf.spaces(1);
        }

        push_op(buf, binop);

        buf.spaces(1);
    }

    loc_right_side.format_with_options(buf, apply_needs_parens, Newlines::Yes, indent);
}

fn format_spaces<'a, 'buf>(
    buf: &mut Buf<'buf>,
    spaces: &[CommentOrNewline<'a>],
    newlines: Newlines,
    indent: u16,
) {
    match newlines {
        Newlines::Yes => {
            fmt_spaces(buf, spaces.iter(), indent);
        }
        Newlines::No => {
            fmt_comments_only(buf, spaces.iter(), NewlineAt::Bottom, indent);
        }
    }
}

fn has_line_comment_before<'a>(expr: &'a Expr<'a>) -> bool {
    use roc_parse::ast::Expr::*;

    match expr {
        SpaceBefore(_, spaces) => {
            matches!(spaces.iter().next(), Some(CommentOrNewline::LineComment(_)))
        }
        _ => false,
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

fn fmt_when<'a, 'buf>(
    buf: &mut Buf<'buf>,
    loc_condition: &'a Loc<Expr<'a>>,
    branches: &[&'a WhenBranch<'a>],
    indent: u16,
) {
    let is_multiline_condition = loc_condition.is_multiline();
    buf.indent(indent);
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
                buf.newline();
                match &expr_below {
                    Expr::SpaceAfter(expr_above, spaces_below_expr) => {
                        // If any of the spaces is a newline, add a newline at the top.
                        // Otherwise leave it as just a comment.
                        let newline_at = if spaces_below_expr
                            .iter()
                            .any(|spaces| matches!(spaces, CommentOrNewline::Newline))
                        {
                            NewlineAt::Top
                        } else {
                            NewlineAt::None
                        };

                        expr_above.format(buf, condition_indent);
                        fmt_comments_only(
                            buf,
                            spaces_below_expr.iter(),
                            newline_at,
                            condition_indent,
                        );
                        buf.newline();
                    }
                    _ => {
                        expr_below.format(buf, condition_indent);
                    }
                }
            }
            _ => {
                buf.newline();
                loc_condition.format(buf, condition_indent);
                buf.newline();
            }
        }
        buf.indent(indent);
    } else {
        buf.spaces(1);
        loc_condition.format(buf, indent);
        buf.spaces(1);
    }
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

fn fmt_dbg<'a, 'buf>(
    buf: &mut Buf<'buf>,
    condition: &'a Loc<Expr<'a>>,
    continuation: &'a Loc<Expr<'a>>,
    is_multiline: bool,
    indent: u16,
) {
    buf.ensure_ends_with_newline();
    buf.indent(indent);
    buf.push_str("dbg");

    let return_indent = if is_multiline {
        buf.newline();
        indent + INDENT
    } else {
        buf.spaces(1);
        indent
    };

    condition.format(buf, return_indent);

    // Always put a blank line after the `dbg` line(s)
    buf.ensure_ends_with_blank_line();

    continuation.format(buf, indent);
}

fn fmt_expect<'a, 'buf>(
    buf: &mut Buf<'buf>,
    condition: &'a Loc<Expr<'a>>,
    continuation: &'a Loc<Expr<'a>>,
    is_multiline: bool,
    indent: u16,
) {
    buf.ensure_ends_with_newline();
    buf.indent(indent);
    buf.push_str("expect");

    let return_indent = if is_multiline {
        buf.newline();
        indent + INDENT
    } else {
        buf.spaces(1);
        indent
    };

    condition.format(buf, return_indent);

    // Always put a blank line after the `expect` line(s)
    buf.ensure_ends_with_blank_line();

    continuation.format(buf, indent);
}

fn fmt_if<'a, 'buf>(
    buf: &mut Buf<'buf>,
    branches: &'a [(Loc<Expr<'a>>, Loc<Expr<'a>>)],
    final_else: &'a Loc<Expr<'a>>,
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

    for (i, (loc_condition, loc_then)) in branches.iter().enumerate() {
        let is_multiline_condition = loc_condition.is_multiline();

        buf.indent(indent);

        if i > 0 {
            buf.push_str("else");
            buf.spaces(1);
        }

        buf.push_str("if");

        if is_multiline_condition {
            match &loc_condition.value {
                Expr::SpaceBefore(expr_below, spaces_before_expr) => {
                    fmt_comments_only(
                        buf,
                        spaces_before_expr.iter(),
                        NewlineAt::Top,
                        return_indent,
                    );
                    buf.newline();

                    match &expr_below {
                        Expr::SpaceAfter(expr_above, spaces_after_expr) => {
                            expr_above.format(buf, return_indent);

                            // If any of the spaces is a newline, add a newline at the top.
                            // Otherwise leave it as just a comment.
                            let newline_at = if spaces_after_expr
                                .iter()
                                .any(|spaces| matches!(spaces, CommentOrNewline::Newline))
                            {
                                NewlineAt::Top
                            } else {
                                NewlineAt::None
                            };

                            fmt_comments_only(
                                buf,
                                spaces_after_expr.iter(),
                                newline_at,
                                return_indent,
                            );
                            buf.newline();
                        }

                        _ => {
                            expr_below.format(buf, return_indent);
                        }
                    }
                }

                Expr::SpaceAfter(expr_above, spaces_below_expr) => {
                    buf.newline();
                    expr_above.format(buf, return_indent);
                    fmt_comments_only(buf, spaces_below_expr.iter(), NewlineAt::Top, return_indent);
                    buf.newline();
                }

                _ => {
                    buf.newline();
                    loc_condition.format(buf, return_indent);
                    buf.newline();
                }
            }
            buf.indent(indent);
        } else {
            buf.spaces(1);
            loc_condition.format_with_options(buf, Parens::NotNeeded, Newlines::Yes, indent);
            buf.spaces(1);
        }

        buf.push_str("then");

        if is_multiline {
            match &loc_then.value {
                Expr::SpaceBefore(expr_below, spaces_below) => {
                    // we want exactly one newline, user-inserted extra newlines are ignored.
                    buf.newline();
                    fmt_comments_only(buf, spaces_below.iter(), NewlineAt::Bottom, return_indent);

                    match &expr_below {
                        Expr::SpaceAfter(expr_above, spaces_above) => {
                            expr_above.format(buf, return_indent);

                            // If any of the spaces is a newline, add a newline at the top.
                            // Otherwise leave it as just a comment.
                            let newline_at = if spaces_above
                                .iter()
                                .any(|spaces| matches!(spaces, CommentOrNewline::Newline))
                            {
                                NewlineAt::Top
                            } else {
                                NewlineAt::None
                            };

                            fmt_comments_only(buf, spaces_above.iter(), newline_at, return_indent);
                            buf.newline();
                        }

                        _ => {
                            expr_below.format(buf, return_indent);
                        }
                    }
                }
                _ => {
                    buf.newline();
                    loc_then.format(buf, return_indent);
                    buf.newline();
                }
            }
        } else {
            buf.push_str("");
            buf.spaces(1);
            loc_then.format(buf, return_indent);
        }
    }

    buf.indent(indent);
    if is_multiline {
        buf.push_str("else");
        buf.newline();
    } else {
        buf.push_str(" else");
        buf.spaces(1);
    }

    final_else.format(buf, return_indent);
}

fn fmt_closure<'a, 'buf>(
    buf: &mut Buf<'buf>,
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

    let mut it = loc_patterns.iter().peekable();

    while let Some(loc_pattern) = it.next() {
        loc_pattern.format(buf, indent);

        if it.peek().is_some() {
            buf.indent(indent);
            if arguments_are_multiline {
                buf.push(',');
                buf.newline();
            } else {
                buf.push_str(",");
                buf.spaces(1);
            }
        }
    }

    if arguments_are_multiline {
        buf.newline();
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

fn fmt_backpassing<'a, 'buf>(
    buf: &mut Buf<'buf>,
    loc_patterns: &'a [Loc<Pattern<'a>>],
    loc_body: &'a Loc<Expr<'a>>,
    loc_ret: &'a Loc<Expr<'a>>,
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

    let pattern_needs_parens = loc_patterns
        .iter()
        .any(|p| pattern_needs_parens_when_backpassing(&p.value));

    if pattern_needs_parens {
        buf.indent(indent);
        buf.push('(');
    }

    let mut it = loc_patterns.iter().peekable();

    while let Some(loc_pattern) = it.next() {
        loc_pattern.format(buf, indent);

        if it.peek().is_some() {
            if arguments_are_multiline {
                buf.push(',');
                buf.newline();
            } else {
                buf.push_str(",");
                buf.spaces(1);
            }
        }
    }

    if pattern_needs_parens {
        buf.push(')');
    }

    if arguments_are_multiline {
        buf.newline();
        buf.indent(indent);
    } else {
        buf.spaces(1);
    }

    buf.push_str("<-");

    let is_multiline = loc_ret.value.is_multiline();

    // If the body is multiline, go down a line and indent.
    let body_indent = if is_multiline {
        indent + INDENT
    } else {
        indent
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
    loc_ret.format_with_options(buf, Parens::NotNeeded, Newlines::Yes, indent);
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

fn fmt_record<'a, 'buf>(
    buf: &mut Buf<'buf>,
    update: Option<&'a Loc<Expr<'a>>>,
    fields: Collection<'a, Loc<AssignedField<'a, Expr<'a>>>>,
    indent: u16,
) {
    let loc_fields = fields.items;
    let final_comments = fields.final_comments();
    buf.indent(indent);
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
                buf.spaces(1);
                record_var.format(buf, indent);
                buf.push_str(" &");
            }
        }

        let is_multiline = loc_fields.iter().any(|loc_field| loc_field.is_multiline())
            || !final_comments.is_empty();

        if is_multiline {
            let field_indent = indent + INDENT;
            for (index, field) in loc_fields.iter().enumerate() {
                // comma addition is handled by the `format_field_multiline` function
                // since we can have stuff like:
                // { x # comment
                // , y
                // }
                // In this case, we have to move the comma before the comment.

                let is_first_item = index == 0;
                if let AssignedField::SpaceBefore(_sub_field, spaces) = &field.value {
                    let is_only_newlines = spaces.iter().all(|s| s.is_newline());
                    if !is_first_item
                        && !is_only_newlines
                        && count_leading_newlines(spaces.iter()) > 1
                    {
                        buf.newline();
                    }

                    fmt_comments_only(buf, spaces.iter(), NewlineAt::Top, field_indent);

                    if !is_only_newlines && count_leading_newlines(spaces.iter().rev()) > 0 {
                        buf.newline();
                    }
                }

                format_field_multiline(buf, &field.value, field_indent, "");
            }

            if count_leading_newlines(final_comments.iter()) > 1 {
                buf.newline();
            }

            fmt_comments_only(buf, final_comments.iter(), NewlineAt::Top, field_indent);

            buf.newline();
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

fn format_field_multiline<'a, 'buf, T>(
    buf: &mut Buf<'buf>,
    field: &AssignedField<'a, T>,
    indent: u16,
    separator_prefix: &str,
) where
    T: Formattable,
{
    use self::AssignedField::*;
    match field {
        RequiredValue(name, spaces, ann) => {
            buf.newline();
            buf.indent(indent);
            buf.push_str(name.value);

            if !spaces.is_empty() {
                fmt_spaces(buf, spaces.iter(), indent);
            }

            buf.push_str(separator_prefix);
            buf.push_str(":");
            buf.spaces(1);
            ann.value.format(buf, indent);
            buf.push(',');
        }
        OptionalValue(name, spaces, ann) => {
            buf.newline();
            buf.indent(indent);
            buf.push_str(name.value);

            if !spaces.is_empty() {
                fmt_spaces(buf, spaces.iter(), indent);
            }

            buf.push_str(separator_prefix);
            buf.push_str("?");
            buf.spaces(1);
            ann.value.format(buf, indent);
            buf.push(',');
        }
        LabelOnly(name) => {
            buf.newline();
            buf.indent(indent);
            buf.push_str(name.value);
            buf.push(',');
        }
        AssignedField::SpaceBefore(sub_field, _spaces) => {
            // We have something like that:
            // ```
            // # comment
            // field,
            // ```
            // we'd like to preserve this

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
                    BinOp::Assignment
                    | BinOp::IsAliasType
                    | BinOp::IsOpaqueType
                    | BinOp::Backpassing => false,
                })
        }
        Expr::If(_, _) => true,
        _ => false,
    }
}
