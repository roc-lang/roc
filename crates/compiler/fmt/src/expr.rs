use crate::annotation::{except_last, is_collection_multiline, Formattable, Newlines, Parens};
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
    is_expr_suffixed, AssignedField, Base, Collection, CommentOrNewline, Expr, ExtractSpaces,
    Pattern, TryTarget, WhenBranch,
};
use roc_parse::ast::{StrLiteral, StrSegment};
use roc_parse::ident::Accessor;
use roc_parse::keyword;
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

            MalformedSuffixed(loc_expr) => loc_expr.is_multiline(),

            // These expressions never have newlines
            Float(..)
            | Num(..)
            | NonBase10Int { .. }
            | SingleQuote(_)
            | AccessorFunction(_)
            | RecordUpdater(_)
            | Var { .. }
            | Underscore { .. }
            | MalformedIdent(_, _)
            | Tag(_)
            | OpaqueRef(_)
            | Crash
            | Dbg
            | Try => false,

            RecordAccess(inner, _) | TupleAccess(inner, _) | TrySuffix { expr: inner, .. } => {
                inner.is_multiline()
            }

            // These expressions always have newlines
            Defs(_, _) | When(_, _) => true,

            List(items) => is_collection_multiline(items),

            Str(literal) => is_str_multiline(literal),
            Apply(loc_expr, args, _) => {
                loc_expr.is_multiline() || args.iter().any(|loc_arg| loc_arg.is_multiline())
            }

            Expect(condition, continuation) => {
                condition.is_multiline() || continuation.is_multiline()
            }
            DbgStmt(condition, _) => condition.is_multiline(),
            LowLevelDbg(_, _, _) => unreachable!(
                "LowLevelDbg should only exist after desugaring, not during formatting"
            ),
            Return(_return_value, _after_return) => true,

            If {
                if_thens: branches,
                final_else,
                ..
            } => {
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
            })
            | EmptyRecordBuilder(loc_subexpr)
            | SingleFieldRecordBuilder(loc_subexpr)
            | OptionalFieldInRecordBuilder(_, loc_subexpr) => loc_subexpr.is_multiline(),

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

            Record(fields) => is_collection_multiline(fields),
            Tuple(fields) => is_collection_multiline(fields),
            RecordUpdate { fields, .. } => is_collection_multiline(fields),
            RecordBuilder { fields, .. } => is_collection_multiline(fields),
        }
    }

    fn format_with_options(&self, buf: &mut Buf, parens: Parens, newlines: Newlines, indent: u16) {
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
                    sub_expr.format_with_options(buf, Parens::NotNeeded, newlines, indent);
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
            Try => {
                buf.indent(indent);
                buf.push_str("try");
            }
            Apply(loc_expr, loc_args, _) => {
                // Sadly this assertion fails in practice. The fact that the parser produces code like this is going to
                // confuse the formatter, because it depends on being able to "see" spaces that logically come before the inner
                // expr in several places - which is necessarily the case when the `loc_expr` of the apply itself has
                // SpaceBefore.
                //
                // TODO: enforce in the type system that spaces must be pushed to the "outside".
                // In other words, Expr::Apply should look something like the following, and there shouldn't be Expr::SpaceBefore and ::SpaceAfter.
                //
                // ```
                // Apply(&'a SpaceAfter<Loc<Expr<'a>>>, &'a [&'a SpaceBefore<Loc<Expr<'a>>>], CalledVia),
                // ```
                //
                // assert!(loc_expr.extract_spaces().before.is_empty(), "{:#?}", self);

                buf.indent(indent);
                if apply_needs_parens && !loc_args.is_empty() {
                    buf.push('(');
                }

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
                                && matches!(
                                    a.value.extract_spaces().item,
                                    Expr::Tuple(_) | Expr::List(_) | Expr::Record(_)
                                )
                                && a.extract_spaces().before == [CommentOrNewline::Newline]
                        })
                        .unwrap_or_default();

                let needs_indent = !should_reflow_outdentable
                    && (!loc_expr.extract_spaces().after.is_empty()
                        || except_last(loc_args).any(|a| a.is_multiline())
                        || loc_args
                            .last()
                            .map(|a| {
                                a.is_multiline()
                                    && (!a.extract_spaces().before.is_empty()
                                        || !is_outdentable(&a.value))
                            })
                            .unwrap_or_default());

                let arg_indent = if needs_indent {
                    indent + INDENT
                } else {
                    indent
                };

                let expr_needs_parens =
                    matches!(loc_expr.value.extract_spaces().item, Expr::Closure(..))
                        && !loc_args.is_empty();

                if expr_needs_parens {
                    buf.push('(');
                }

                loc_expr.format_with_options(buf, Parens::InApply, Newlines::Yes, indent);

                if expr_needs_parens {
                    buf.indent(indent);
                    buf.push(')');
                }

                for loc_arg in loc_args.iter() {
                    if should_reflow_outdentable {
                        buf.spaces(1);

                        // Ignore any comments+newlines before/after.
                        // We checked above that there's only a single newline before the last arg,
                        // which we're intentionally ignoring.

                        let arg = loc_arg.extract_spaces();
                        arg.item.format_with_options(
                            buf,
                            Parens::InApply,
                            Newlines::Yes,
                            arg_indent,
                        );
                    } else if needs_indent {
                        let arg = loc_arg.extract_spaces();
                        fmt_spaces(buf, arg.before.iter(), arg_indent);
                        buf.ensure_ends_with_newline();
                        arg.item.format_with_options(
                            buf,
                            Parens::InApply,
                            Newlines::Yes,
                            arg_indent,
                        );
                        fmt_spaces(buf, arg.after.iter(), arg_indent);
                    } else {
                        buf.spaces(1);
                        loc_arg.format_with_options(
                            buf,
                            Parens::InApply,
                            Newlines::Yes,
                            arg_indent,
                        );
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
                format_sq_literal(buf, string);
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
                fmt_record_like(
                    buf,
                    None,
                    *fields,
                    indent,
                    format_assigned_field_multiline,
                    assigned_field_to_space_before,
                );
            }
            RecordUpdate { update, fields } => {
                fmt_record_like(
                    buf,
                    Some(RecordPrefix::Update(update)),
                    *fields,
                    indent,
                    format_assigned_field_multiline,
                    assigned_field_to_space_before,
                );
            }
            RecordBuilder { mapper, fields } => {
                fmt_record_like(
                    buf,
                    Some(RecordPrefix::Mapper(mapper)),
                    *fields,
                    indent,
                    format_assigned_field_multiline,
                    assigned_field_to_space_before,
                );
            }
            Closure(loc_patterns, loc_ret) => {
                fmt_closure(buf, loc_patterns, loc_ret, indent);
            }
            Backpassing(loc_patterns, loc_body, loc_ret) => {
                fmt_backpassing(buf, loc_patterns, loc_body, loc_ret, indent);
            }
            Defs(defs, ret) => {
                {
                    let indent = if parens == Parens::InOperator {
                        buf.indent(indent);
                        buf.push('(');
                        buf.newline();
                        indent + INDENT
                    } else {
                        indent
                    };

                    // It should theoretically be impossible to *parse* an empty defs list.
                    // (Canonicalization can remove defs later, but that hasn't happened yet!)
                    debug_assert!(!defs.is_empty());

                    fmt_defs(buf, defs, indent);

                    match &ret.value {
                        SpaceBefore(sub_expr, spaces) => {
                            buf.spaces(1);
                            fmt_spaces(buf, spaces.iter(), indent);

                            buf.indent(indent);

                            sub_expr.format_with_options(
                                buf,
                                Parens::NotNeeded,
                                Newlines::Yes,
                                indent,
                            );
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

                if parens == Parens::InOperator {
                    buf.ensure_ends_with_newline();
                    buf.indent(indent);
                    buf.push(')');
                }
            }
            Expect(condition, continuation) => {
                fmt_expect(buf, condition, continuation, self.is_multiline(), indent);
            }
            Dbg => {
                buf.indent(indent);
                buf.push_str("dbg");
            }
            DbgStmt(condition, continuation) => {
                fmt_dbg_stmt(buf, condition, continuation, self.is_multiline(), indent);
            }
            LowLevelDbg(_, _, _) => unreachable!(
                "LowLevelDbg should only exist after desugaring, not during formatting"
            ),
            Return(return_value, after_return) => {
                fmt_return(buf, return_value, after_return, parens, newlines, indent);
            }
            If {
                if_thens: branches,
                final_else,
                indented_else,
            } => {
                fmt_if(
                    buf,
                    branches,
                    final_else,
                    self.is_multiline(),
                    *indented_else,
                    indent,
                );
            }
            When(loc_condition, branches) => fmt_when(buf, loc_condition, branches, indent),
            Tuple(items) => fmt_collection(buf, indent, Braces::Round, *items, Newlines::No),
            List(items) => fmt_collection(buf, indent, Braces::Square, *items, Newlines::No),
            BinOps(lefts, right) => fmt_binops(buf, lefts, right, false, indent),
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

                let needs_newline = match &sub_expr.value {
                    SpaceBefore(..) => true,
                    Str(text) => is_str_multiline(text),
                    _ => false,
                };
                let needs_parens =
                    needs_newline && matches!(unary_op.value, called_via::UnaryOp::Negate);

                if needs_parens {
                    // Unary negation can't be followed by whitespace (which is what a newline is) - so
                    // we need to wrap the negated value in parens.
                    buf.push('(');
                }

                let inner_indent = if needs_parens {
                    indent + INDENT
                } else {
                    indent
                };

                sub_expr.format_with_options(buf, Parens::InApply, newlines, inner_indent);

                if needs_parens {
                    buf.push(')');
                }
            }
            AccessorFunction(key) => {
                buf.indent(indent);
                buf.push('.');
                match key {
                    Accessor::RecordField(key) => buf.push_str(key),
                    Accessor::TupleIndex(key) => buf.push_str(key),
                }
            }
            RecordUpdater(key) => {
                buf.indent(indent);
                buf.push('&');
                buf.push_str(key);
            }
            RecordAccess(expr, key) => {
                expr.format_with_options(buf, Parens::InApply, Newlines::Yes, indent);
                buf.push('.');
                buf.push_str(key);
            }
            TupleAccess(expr, key) => {
                expr.format_with_options(buf, Parens::InApply, Newlines::Yes, indent);
                buf.push('.');
                buf.push_str(key);
            }
            TrySuffix { expr, target } => {
                expr.format_with_options(buf, Parens::InApply, Newlines::Yes, indent);
                match target {
                    TryTarget::Task => buf.push('!'),
                    TryTarget::Result => buf.push('?'),
                }
            }
            MalformedIdent(str, _) => {
                buf.indent(indent);
                buf.push_str(str)
            }
            MalformedSuffixed(loc_expr) => {
                buf.indent(indent);
                loc_expr.format_with_options(buf, parens, newlines, indent);
            }
            PrecedenceConflict { .. } => {}
            EmptyRecordBuilder { .. } => {}
            SingleFieldRecordBuilder { .. } => {}
            OptionalFieldInRecordBuilder(_, _) => {}
        }
    }
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

fn format_str_segment(seg: &StrSegment, buf: &mut Buf, indent: u16) {
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
                format_str_segment(seg, buf, 0)
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
                        format_str_segment(seg, buf, indent);
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

fn fmt_binops<'a>(
    buf: &mut Buf,
    lefts: &'a [(Loc<Expr<'a>>, Loc<BinOp>)],
    loc_right_side: &'a Loc<Expr<'a>>,
    part_of_multi_line_binops: bool,
    indent: u16,
) {
    let is_multiline = part_of_multi_line_binops
        || loc_right_side.value.is_multiline()
        || lefts.iter().any(|(expr, _)| expr.value.is_multiline());

    let is_any_lefts_suffixed = lefts.iter().any(|(left, _)| is_expr_suffixed(&left.value));
    let is_right_suffixed = is_expr_suffixed(&loc_right_side.value);
    let is_any_suffixed = is_any_lefts_suffixed || is_right_suffixed;

    let mut is_first = false;
    let mut adjusted_indent = indent;

    if is_any_suffixed {
        // we only want to indent the remaining lines if this is a suffixed expression.
        is_first = true;
    }

    for (loc_left_side, loc_binop) in lefts {
        let binop = loc_binop.value;

        loc_left_side.format_with_options(buf, Parens::InOperator, Newlines::No, adjusted_indent);

        if is_first {
            // indent the remaining lines, but only if the expression is suffixed.
            is_first = false;
            adjusted_indent = indent + 4;
        }

        if is_multiline {
            buf.ensure_ends_with_newline();
            buf.indent(adjusted_indent);
        } else {
            buf.spaces(1);
        }

        push_op(buf, binop);

        buf.spaces(1);
    }

    loc_right_side.format_with_options(buf, Parens::InOperator, Newlines::Yes, adjusted_indent);
}

fn format_spaces(buf: &mut Buf, spaces: &[CommentOrNewline], newlines: Newlines, indent: u16) {
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

fn fmt_when<'a>(
    buf: &mut Buf,
    loc_condition: &'a Loc<Expr<'a>>,
    branches: &[&'a WhenBranch<'a>],
    indent: u16,
) {
    let is_multiline_condition = loc_condition.is_multiline();
    buf.ensure_ends_with_newline();
    buf.indent(indent);
    buf.push_str("when");
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

fn fmt_dbg_stmt<'a>(
    buf: &mut Buf,
    condition: &'a Loc<Expr<'a>>,
    continuation: &'a Loc<Expr<'a>>,
    _: bool,
    indent: u16,
) {
    buf.ensure_ends_with_newline();
    buf.indent(indent);
    buf.push_str("dbg");

    buf.spaces(1);

    fn should_outdent(mut expr: &Expr) -> bool {
        loop {
            match expr {
                Expr::ParensAround(_) | Expr::List(_) | Expr::Record(_) | Expr::Tuple(_) => {
                    return true
                }
                Expr::SpaceAfter(inner, _) => {
                    expr = inner;
                }
                _ => return false,
            }
        }
    }

    let inner_indent = if should_outdent(&condition.value) {
        indent
    } else {
        indent + INDENT
    };

    let cond_value = condition.value.extract_spaces();

    let is_defs = matches!(cond_value.item, Expr::Defs(_, _));

    let newlines = if is_defs { Newlines::Yes } else { Newlines::No };

    condition.format_with_options(buf, Parens::NotNeeded, newlines, inner_indent);

    // Always put a blank line after the `dbg` line(s)
    buf.ensure_ends_with_blank_line();

    continuation.format(buf, indent);
}

fn fmt_expect<'a>(
    buf: &mut Buf,
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

    buf.spaces(1);

    let return_indent = if return_value.is_multiline() {
        indent + INDENT
    } else {
        indent
    };

    return_value.format(buf, return_indent);

    if let Some(after_return) = after_return {
        if after_return.value.extract_spaces().before.is_empty() {
            buf.ensure_ends_with_newline();
        }
        after_return.format_with_options(buf, parens, newlines, indent);
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

    let mut it = loc_patterns.iter().peekable();

    while let Some(loc_pattern) = it.next() {
        loc_pattern.format_with_options(buf, Parens::InAsPattern, Newlines::No, indent);

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

fn fmt_backpassing<'a>(
    buf: &mut Buf,
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

    let mut it = loc_patterns.iter().peekable();

    while let Some(loc_pattern) = it.next() {
        let needs_parens = if pattern_needs_parens_when_backpassing(&loc_pattern.value) {
            Parens::InApply
        } else {
            Parens::NotNeeded
        };

        loc_pattern.format_with_options(buf, needs_parens, Newlines::No, indent);

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

enum RecordPrefix<'a> {
    Update(&'a Loc<Expr<'a>>),
    Mapper(&'a Loc<Expr<'a>>),
}

fn fmt_record_like<'a, Field, Format, ToSpaceBefore>(
    buf: &mut Buf,
    prefix: Option<RecordPrefix<'a>>,
    fields: Collection<'a, Loc<Field>>,
    indent: u16,
    format_field_multiline: Format,
    to_space_before: ToSpaceBefore,
) where
    Field: Formattable,
    Format: Fn(&mut Buf, &Field, u16, &str),
    ToSpaceBefore: Fn(&'a Field) -> Option<(&'a Field, &'a [CommentOrNewline<'a>])>,
{
    let loc_fields = fields.items;
    let final_comments = fields.final_comments();
    buf.indent(indent);
    if loc_fields.is_empty() && final_comments.iter().all(|c| c.is_newline()) && prefix.is_none() {
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
                buf.push_str(" &");
            }
            Some(RecordPrefix::Mapper(mapper_var)) => {
                buf.spaces(1);
                mapper_var.format(buf, indent);
                buf.push_str(" <-");
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
                if let Some((_sub_field, spaces)) = to_space_before(&field.value) {
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

fn format_assigned_field_multiline<T>(
    buf: &mut Buf,
    field: &AssignedField<T>,
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
                buf.indent(indent);
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
                buf.indent(indent);
            }

            buf.push_str(separator_prefix);
            buf.push_str("?");
            buf.spaces(1);
            ann.value.format(buf, indent);
            buf.push(',');
        }
        IgnoredValue(name, spaces, ann) => {
            buf.newline();
            buf.indent(indent);
            buf.push('_');
            buf.push_str(name.value);

            if !spaces.is_empty() {
                fmt_spaces(buf, spaces.iter(), indent);
                buf.indent(indent);
            }

            buf.push_str(separator_prefix);
            buf.push_str(":");
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

            format_assigned_field_multiline(buf, sub_field, indent, separator_prefix);
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
            format_assigned_field_multiline(buf, sub_field, indent, separator_prefix);
            fmt_comments_only(buf, spaces.iter(), NewlineAt::Top, indent);
        }
    }
}

fn assigned_field_to_space_before<'a, T>(
    field: &'a AssignedField<'a, T>,
) -> Option<(&AssignedField<'a, T>, &'a [CommentOrNewline<'a>])> {
    match field {
        AssignedField::SpaceBefore(sub_field, spaces) => Some((sub_field, spaces)),
        _ => None,
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
                })
        }
        Expr::If { .. } => true,
        Expr::Defs(_, _) => true,
        Expr::SpaceBefore(e, _) => sub_expr_requests_parens(e),
        Expr::SpaceAfter(e, _) => sub_expr_requests_parens(e),
        _ => false,
    }
}
