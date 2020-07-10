use crate::annotation::Parens;
use crate::spaces::{fmt_comments_only, fmt_spaces};
use bumpalo::collections::String;
use roc_parse::ast::{Base, Pattern};

pub fn fmt_pattern<'a>(
    buf: &mut String<'a>,
    pattern: &'a Pattern<'a>,
    indent: u16,
    parens: Parens,
    only_comments: bool,
) {
    use self::Pattern::*;

    match pattern {
        Identifier(string) => buf.push_str(string),
        GlobalTag(name) | PrivateTag(name) => {
            buf.push_str(name);
        }
        Apply(loc_pattern, loc_arg_patterns) => {
            // Sometimes, an Apply pattern needs parens around it.
            // In particular when an Apply's argument is itself an Apply (> 0) arguments
            let parens = !loc_arg_patterns.is_empty() && parens == Parens::InApply;

            if parens {
                buf.push('(');
            }

            fmt_pattern(
                buf,
                &loc_pattern.value,
                indent,
                Parens::InApply,
                only_comments,
            );

            for loc_arg in loc_arg_patterns.iter() {
                buf.push(' ');
                fmt_pattern(buf, &loc_arg.value, indent, Parens::InApply, only_comments);
            }

            if parens {
                buf.push(')');
            }
        }
        RecordDestructure(loc_patterns) => {
            buf.push_str("{ ");

            let mut it = loc_patterns.iter().peekable();

            while let Some(loc_pattern) = it.next() {
                fmt_pattern(
                    buf,
                    &loc_pattern.value,
                    indent,
                    Parens::NotNeeded,
                    only_comments,
                );

                if it.peek().is_some() {
                    buf.push_str(", ");
                }
            }

            buf.push_str(" }");
        }

        RecordField(name, loc_pattern) => {
            buf.push_str(name);
            buf.push_str(": ");
            fmt_pattern(
                buf,
                &loc_pattern.value,
                indent,
                Parens::NotNeeded,
                only_comments,
            );
        }

        NumLiteral(string) => buf.push_str(string),
        NonBase10Literal {
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
        FloatLiteral(string) => buf.push_str(string),
        StrLiteral(string) => buf.push_str(string),
        BlockStrLiteral(lines) => {
            for line in *lines {
                buf.push_str(line)
            }
        }
        Underscore => buf.push('_'),

        // Space
        SpaceBefore(sub_pattern, spaces) => {
            if only_comments {
                fmt_comments_only(buf, spaces.iter(), indent)
            } else {
                fmt_spaces(buf, spaces.iter(), indent);
            }
            fmt_pattern(buf, sub_pattern, indent, parens, only_comments);
        }
        SpaceAfter(sub_pattern, spaces) => {
            fmt_pattern(buf, sub_pattern, indent, parens, only_comments);
            if only_comments {
                fmt_comments_only(buf, spaces.iter(), indent)
            } else {
                fmt_spaces(buf, spaces.iter(), indent);
            }
        }

        Nested(sub_pattern) => {
            fmt_pattern(buf, sub_pattern, indent, parens, only_comments);
        }

        // Malformed
        Malformed(string) => buf.push_str(string),
        QualifiedIdentifier { module_name, ident } => {
            if !module_name.is_empty() {
                buf.push_str(module_name);
                buf.push('.');
            }

            buf.push_str(ident);
        }
    }
}
