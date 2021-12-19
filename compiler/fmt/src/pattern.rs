use crate::annotation::{Formattable, Newlines, Parens};
use crate::spaces::{fmt_comments_only, fmt_spaces, NewlineAt};
use crate::Buf;
use roc_parse::ast::{Base, Pattern};

pub fn fmt_pattern<'a, 'buf>(
    buf: &mut Buf<'buf>,
    pattern: &'a Pattern<'a>,
    indent: u16,
    parens: Parens,
) {
    pattern.format_with_options(buf, parens, Newlines::No, indent);
}

impl<'a> Formattable for Pattern<'a> {
    fn is_multiline(&self) -> bool {
        // Theory: a pattern should only be multiline when it contains a comment
        match self {
            Pattern::SpaceBefore(_, spaces) | Pattern::SpaceAfter(_, spaces) => {
                debug_assert!(!spaces.is_empty());

                spaces.iter().any(|s| s.is_comment())
            }

            Pattern::RecordDestructure(fields) => fields.iter().any(|f| f.is_multiline()),
            Pattern::RequiredField(_, subpattern) => subpattern.is_multiline(),

            Pattern::OptionalField(_, expr) => expr.is_multiline(),

            Pattern::Identifier(_)
            | Pattern::GlobalTag(_)
            | Pattern::PrivateTag(_)
            | Pattern::Apply(_, _)
            | Pattern::NumLiteral(_)
            | Pattern::NonBase10Literal { .. }
            | Pattern::FloatLiteral(_)
            | Pattern::StrLiteral(_)
            | Pattern::Underscore(_)
            | Pattern::Malformed(_)
            | Pattern::MalformedIdent(_, _)
            | Pattern::QualifiedIdentifier { .. } => false,
        }
    }

    fn format_with_options<'buf>(
        &self,
        buf: &mut Buf<'buf>,
        parens: Parens,
        newlines: Newlines,
        indent: u16,
    ) {
        use self::Pattern::*;

        match self {
            Identifier(string) => {
                buf.indent(indent);
                buf.push_str(string)
            }
            GlobalTag(name) | PrivateTag(name) => {
                buf.indent(indent);
                buf.push_str(name);
            }
            Apply(loc_pattern, loc_arg_patterns) => {
                buf.indent(indent);
                // Sometimes, an Apply pattern needs parens around it.
                // In particular when an Apply's argument is itself an Apply (> 0) arguments
                let parens = !loc_arg_patterns.is_empty() && parens == Parens::InApply;

                if parens {
                    buf.push('(');
                }

                loc_pattern.format_with_options(buf, Parens::InApply, Newlines::No, indent);

                for loc_arg in loc_arg_patterns.iter() {
                    buf.spaces(1);
                    loc_arg.format_with_options(buf, Parens::InApply, Newlines::No, indent);
                }

                if parens {
                    buf.push(')');
                }
            }
            RecordDestructure(loc_patterns) => {
                buf.indent(indent);
                buf.push_str("{");
                buf.spaces(1);

                let mut it = loc_patterns.iter().peekable();

                while let Some(loc_pattern) = it.next() {
                    loc_pattern.format(buf, indent);

                    if it.peek().is_some() {
                        buf.push_str(",");
                        buf.spaces(1);
                    }
                }

                buf.push_str(" }");
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

            NumLiteral(string) => {
                buf.indent(indent);
                buf.push_str(string);
            }
            NonBase10Literal {
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
            FloatLiteral(string) => {
                buf.indent(indent);
                buf.push_str(string);
            }
            StrLiteral(literal) => {
                todo!("Format string literal: {:?}", literal);
            }
            Underscore(name) => {
                buf.indent(indent);
                buf.push('_');
                buf.push_str(name);
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
                // if only_comments {
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
