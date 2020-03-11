use crate::spaces::{fmt_comments_only, fmt_spaces};
use bumpalo::collections::String;
use roc_parse::ast::{Base, Pattern};

pub fn fmt_pattern<'a>(
    buf: &mut String<'a>,
    pattern: &'a Pattern<'a>,
    indent: u16,
    apply_needs_parens: bool,
    only_comments: bool,
) {
    use self::Pattern::*;

    match pattern {
        Identifier(string) => buf.push_str(string),
        GlobalTag(name) | PrivateTag(name) => {
            buf.push_str(name);
        }
        Apply(loc_pattern, loc_arg_patterns) => {
            if apply_needs_parens {
                buf.push('(');
            }

            fmt_pattern(buf, &loc_pattern.value, indent, true, only_comments);

            for loc_arg in loc_arg_patterns.iter() {
                buf.push(' ');
                fmt_pattern(buf, &loc_arg.value, indent, true, only_comments);
            }

            if apply_needs_parens {
                buf.push(')');
            }
        }
        RecordDestructure(loc_patterns) => {
            buf.push_str("{ ");

            let mut is_first = true;

            for loc_pattern in *loc_patterns {
                if is_first {
                    is_first = false;
                } else {
                    buf.push_str(", ");
                }

                fmt_pattern(buf, &loc_pattern.value, indent, true, only_comments);
            }

            buf.push_str(" }");
        }

        RecordField(name, loc_pattern) => {
            buf.push_str(name);
            buf.push_str(": ");
            fmt_pattern(buf, &loc_pattern.value, indent, true, only_comments);
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

            buf.push('0');

            buf.push(match base {
                Base::Hex => 'x',
                Base::Octal => 'o',
                Base::Binary => 'b',
            });

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
            fmt_pattern(buf, sub_pattern, indent, apply_needs_parens, only_comments);
        }
        SpaceAfter(sub_pattern, spaces) => {
            fmt_pattern(buf, sub_pattern, indent, apply_needs_parens, only_comments);
            if only_comments {
                fmt_comments_only(buf, spaces.iter(), indent)
            } else {
                fmt_spaces(buf, spaces.iter(), indent);
            }
        }

        Nested(sub_pattern) => {
            fmt_pattern(buf, sub_pattern, indent, apply_needs_parens, only_comments);
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
