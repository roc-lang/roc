use crate::annotation::{Formattable, Newlines, Parens};
use crate::expr::{fmt_str_literal, format_sq_literal, is_str_multiline};
use crate::spaces::{fmt_comments_only, fmt_spaces, NewlineAt, INDENT};
use crate::Buf;
use roc_parse::ast::{Base, CommentOrNewline, Pattern, PatternAs};

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

            Pattern::OptionalField(_, expr) => expr.is_multiline(),

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

    fn format_with_options(&self, buf: &mut Buf, parens: Parens, newlines: Newlines, indent: u16) {
        use self::Pattern::*;

        match self {
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

                let indent_more = if self.is_multiline() {
                    indent + INDENT
                } else {
                    indent
                };

                if parens {
                    buf.push('(');
                }

                loc_pattern.format_with_options(buf, Parens::InApply, Newlines::No, indent);

                for loc_arg in loc_arg_patterns.iter() {
                    buf.spaces(1);
                    loc_arg.format_with_options(buf, Parens::InApply, Newlines::No, indent_more);
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
                        loc_pattern.format(buf, indent);

                        if it.peek().is_some() {
                            buf.push_str(",");
                            buf.spaces(1);
                        }
                    }
                    buf.spaces(1);
                }

                buf.push_str("}");
            }

            RequiredField(name, loc_pattern) => {
                buf.indent(indent);
                buf.push_str(name);
                buf.push_str(":");
                buf.spaces(1);
                loc_pattern.format(buf, indent);
            }

            OptionalField(name, loc_pattern) => {
                buf.indent(indent);
                buf.push_str(name);
                buf.push_str(" ?");
                buf.spaces(1);
                loc_pattern.format(buf, indent);
            }

            &NumLiteral(string) => {
                buf.indent(indent);
                buf.push_str(string);
            }
            &NonBase10Literal {
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
            &FloatLiteral(string) => {
                buf.indent(indent);
                buf.push_str(string);
            }
            StrLiteral(literal) => fmt_str_literal(buf, *literal, indent),
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
                    loc_pattern.format(buf, indent);

                    if it.peek().is_some() {
                        buf.push_str(",");
                        buf.spaces(1);
                    }
                }

                buf.push_str(")");
            }
            List(loc_patterns) => {
                buf.indent(indent);
                buf.push_str("[");

                let mut it = loc_patterns.iter().peekable();
                while let Some(loc_pattern) = it.next() {
                    loc_pattern.format(buf, indent);

                    if it.peek().is_some() {
                        buf.push_str(",");
                        buf.spaces(1);
                    }
                }

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
                    buf.push('(');
                }

                fmt_pattern(buf, &pattern.value, indent, parens);

                pattern_as.format(buf, indent + INDENT);

                if needs_parens {
                    buf.push(')');
                }
            }

            // Space
            SpaceBefore(sub_pattern, spaces) => {
                if !sub_pattern.is_multiline() {
                    fmt_comments_only(buf, spaces.iter(), NewlineAt::Bottom, indent)
                } else {
                    fmt_spaces(buf, spaces.iter(), indent);
                }

                sub_pattern.format_with_options(buf, parens, newlines, indent);
            }
            SpaceAfter(sub_pattern, spaces) => {
                sub_pattern.format_with_options(buf, parens, newlines, indent);

                if starts_with_inline_comment(spaces.iter()) {
                    buf.spaces(1);
                }

                if !sub_pattern.is_multiline() {
                    fmt_comments_only(buf, spaces.iter(), NewlineAt::Bottom, indent)
                } else {
                    fmt_spaces(buf, spaces.iter(), indent);
                }
            }

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
    }
}

fn starts_with_inline_comment<'a, I: IntoIterator<Item = &'a CommentOrNewline<'a>>>(
    spaces: I,
) -> bool {
    matches!(
        spaces.into_iter().next(),
        Some(CommentOrNewline::LineComment(_))
    )
}
